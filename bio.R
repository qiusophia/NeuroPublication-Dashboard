# ============================================================
# NeuroPublication Dashboard — R Shiny
# PubMed E-utilities + Claude API abstract classification
# 2020–2026 | Neurodegeneration / Tau / Brain
# ============================================================
# SETUP: Run once in your R console before launching:
#
#   install.packages(c("shiny","shinydashboard","shinyWidgets",
#     "shinyjs","httr","jsonlite","xml2","DT","dplyr","plotly",
#     "shinycssloaders"))
#
# Launch with:  shiny::runApp("app.R")
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(httr)
library(jsonlite)
library(xml2)
library(DT)
library(dplyr)
library(plotly)
library(shinycssloaders)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) &&
                             nchar(as.character(a[1])) > 0) a else b

# ── Scientists ────────────────────────────────────────────────────────────
SCIENTISTS <- list(
  list(name="Acosta-Uribe J",   affil="University of California Santa Barbara"),
  list(name="Berendzen K",      affil="University of California, San Francisco"),
  list(name="Boeynaems S",      affil="Baylor College of Medicine"),
  list(name="Bowles K",         affil="University of Edinburgh"),
  list(name="Boxer A",          affil="University of California, San Francisco"),
  list(name="Butler D",         affil="Neural Stem Cell Institute"),
  list(name="Clarke J",         affil="University of Cambridge"),
  list(name="Clelland C",       affil="University of California, San Francisco"),
  list(name="Crary J",          affil="Icahn School of Medicine at Mount Sinai"),
  list(name="Cuervo AM",        affil="Albert Einstein College of Medicine"),
  list(name="Diamond M",        affil="UT Southwestern Medical Center"),
  list(name="Dickson D",        affil="Mayo Clinic, Jacksonville"),
  list(name="Disney M",         affil="The Scripps Research Institute"),
  list(name="Duff K",           affil="University College London"),
  list(name="Elahi F",          affil="Icahn School of Medicine at Mount Sinai"),
  list(name="Emborg M",         affil="University of Wisconsin"),
  list(name="Farrell K",        affil="Icahn School of Medicine at Mount Sinai"),
  list(name="Frost B",          affil="Brown University"),
  list(name="Gan L",            affil="Weill Cornell Medical College"),
  list(name="Geschwind DH",     affil="University of California, Los Angeles"),
  list(name="Gestwicki J",      affil="University of California, San Francisco"),
  list(name="Goate A",          affil="Icahn School of Medicine at Mount Sinai"),
  list(name="Grinberg L",       affil="Mayo Clinic, Jacksonville"),
  list(name="Haggarty S",       affil="Harvard Medical School"),
  list(name="Han S",            affil="Northwestern University"),
  list(name="Holtzman D",       affil="Washington University in St. Louis"),
  list(name="Hyman B",          affil="Harvard Medical School"),
  list(name="Ichida J",         affil="University of Southern California"),
  list(name="Kampmann M",       affil="University of California, San Francisco"),
  list(name="Kao A",            affil="University of California, San Francisco"),
  list(name="Karch C",          affil="Washington University in St. Louis"),
  list(name="Kelly J",          affil="The Scripps Research Institute"),
  list(name="Kovacs G",         affil="University of Toronto"),
  list(name="Krichevsky A",     affil="Harvard Medical School"),
  list(name="Lasagna-Reeves C", affil="Baylor College of Medicine"),
  list(name="Lee S",            affil="University of California, San Francisco"),
  list(name="McKee A",          affil="Boston University"),
  list(name="Mead E",           affil="University of Oxford"),
  list(name="Miller T",         affil="Washington University in St. Louis"),
  list(name="Morimoto R",       affil="Northwestern University"),
  list(name="Murphy E",         affil="University of Oxford"),
  list(name="Murray M",         affil="Mayo Clinic, Jacksonville"),
  list(name="Neylan T",         affil="University of California, San Francisco"),
  list(name="Orr M",            affil="Washington University"),
  list(name="Petersson EJ",     affil="University of Pennsylvania"),
  list(name="Possin K",         affil="University of California, San Francisco"),
  list(name="Rabinovici G",     affil="University of California, San Francisco"),
  list(name="Rauch J",          affil="University of Massachusetts"),
  list(name="Rexach J",         affil="University of California, Los Angeles"),
  list(name="Rubinsztein D",    affil="University of Cambridge"),
  list(name="Seeley W",         affil="University of California, San Francisco"),
  list(name="Shoichet B",       affil="University of California, San Francisco"),
  list(name="Skidmore J",       affil="University of Cambridge"),
  list(name="Southworth D",     affil="University of California, San Francisco"),
  list(name="Stehouwer J",      affil="University of Pittsburgh"),
  list(name="Steen J",          affil="Harvard Medical School"),
  list(name="Svensson S",       affil="Oxiant Discovery"),
  list(name="Temple S",         affil="Neural Stem Cell Institute"),
  list(name="Tsai LH",          affil="Massachusetts Institute of Technology"),
  list(name="VandeVrede L",     affil="University of California, San Francisco"),
  list(name="Vasdev N",         affil="University of Toronto"),
  list(name="Walsh C",          affil="University of California, San Francisco"),
  list(name="Woerman A",        affil="Colorado State University"),
  list(name="Yokoyama J",       affil="University of California, San Francisco")
)
SCIENTIST_NAMES <- sapply(SCIENTISTS, `[[`, "name")

# ── Data type colours (9 types) ───────────────────────────────────────────
DATA_TYPE_COLORS <- c(
  "Clinical"         = "#e8694a",
  "Animal Model"     = "#5b9e6b",
  "In Vitro"         = "#4a7fc1",
  "Computational"    = "#9b6bbf",
  "Review"           = "#c4a442",
  "WGS"              = "#e05fa0",
  "Exome Sequencing" = "#3dbfbf",
  "scRNA-seq"        = "#f07c3a",
  "Unknown"          = "#7a8999"
)
ALL_DATA_TYPES <- names(DATA_TYPE_COLORS)

ALL_FOCUS_TAGS <- c("Tau","TDP-43","\u03b1-Synuclein","Amyloid",
                    "Neuroinflammation","Protein Aggregation",
                    "Autophagy","Genomics/Sequencing","Other")

NEURO_QUERY <- paste0(
  "(neurodegeneration OR tauopathy OR tau OR Alzheimer OR dementia OR ",
  "\"frontotemporal dementia\" OR \"Parkinson disease\" OR \"TDP-43\" OR ",
  "amyloid OR synuclein OR neurofibrillary OR \"MAPT\" OR \"APOE\" OR ",
  "\"neurodegeneration\" OR \"brain atrophy\" OR prion OR \"Lewy body\" OR ",
  "\"ALS\" OR \"amyotrophic lateral sclerosis\" OR \"Huntington\")"
)

# ═══════════════════════════════════════════════════════════════════════════
# PubMed helpers
# ═══════════════════════════════════════════════════════════════════════════

# Affiliation variants: PubMed stores affiliations inconsistently.
# We try the full name, then a shortened version, to maximise recall.
affil_variants <- function(affiliation) {
  base <- list(affiliation)
  # Common shortenings PubMed uses
  shorts <- list(
    "University of California, San Francisco" = c("UCSF", "UC San Francisco"),
    "University of California Santa Barbara"  = c("UC Santa Barbara", "UCSB",
                                                  "University of California, Santa Barbara"),
    "University of California, Los Angeles"   = c("UCLA", "UC Los Angeles"),
    "Washington University in St. Louis"      = c("Washington University", "WUSTL"),
    "Icahn School of Medicine at Mount Sinai" = c("Mount Sinai", "Icahn School"),
    "Harvard Medical School"                  = c("Harvard Medical", "HMS"),
    "Massachusetts Institute of Technology"   = c("MIT"),
    "University of Cambridge"                 = c("Cambridge University"),
    "Mayo Clinic, Jacksonville"               = c("Mayo Clinic")
  )
  extra <- shorts[[affiliation]]
  if (!is.null(extra)) c(base, extra) else base
}

do_esearch <- function(query, max_results) {
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
    "?db=pubmed&term=", utils::URLencode(query, reserved = TRUE),
    "&retmax=", max_results, "&retmode=json"
  )
  tryCatch({
    resp <- GET(url, timeout(15))
    if (status_code(resp) != 200) return(character(0))
    dat  <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    dat$esearchresult$idlist %||% character(0)
  }, error = function(e) character(0))
}

fetch_pmids <- function(scientist_name, affiliation, max_results = 50) {
  # IMPORTANT: Never break early — always run ALL strategies and union results.
  # Early breaks were the cause of missing papers (e.g. Acosta-Uribe getting 17/20).
  # We collect everything, then cap only at the very end.
  FETCH_CAP <- max(max_results * 3, 150)  # internal cap per query, well above any scientist's output
  
  all_ids <- character(0)
  variants <- affil_variants(affiliation)
  
  # Build name variants: original, no-hyphen, and first-last swapped
  name_variants <- unique(c(
    scientist_name,
    gsub("-", " ", scientist_name),                                    # "Acosta Uribe J"
    gsub("^(\\S+)\\s+(\\S+)$", "\\2 \\1", scientist_name)  # "J Acosta-Uribe"
  ))
  
  # ── Strategy 1: name variants x affiliation variants, NO topic filter ──
  # Affiliation confirms identity so we fetch ALL 2020-2026 papers.
  # No NEURO_QUERY here — it was silently dropping valid papers (PSEN1, admixture, etc.)
  for (nm in name_variants) {
    for (affil in variants) {
      q   <- paste0('"', nm, '"[Author] AND "', affil, '"[Affiliation] AND 2020:2026[PDAT]')
      ids <- do_esearch(q, FETCH_CAP)
      all_ids <- union(all_ids, ids)
      Sys.sleep(0.15)
    }
  }
  
  # ── Strategy 2: name variants + broad neuro topic, no affiliation filter ──
  # Catches papers where the affiliation is listed under a collaborating institution
  # or stored in a format none of our variants matched.
  for (nm in name_variants) {
    q   <- paste0('"', nm, '"[Author] AND ', NEURO_QUERY, ' AND 2020:2026[PDAT]')
    ids <- do_esearch(q, FETCH_CAP)
    all_ids <- union(all_ids, ids)
    Sys.sleep(0.15)
  }
  
  # ── Strategy 3: name variants with NO filters at all ──
  # Last resort — catches any remaining papers missed by strategies 1 & 2.
  # Only runs if we still have fewer papers than expected (< 10).
  if (length(all_ids) < 10) {
    for (nm in name_variants) {
      q   <- paste0('"', nm, '"[Author] AND 2020:2026[PDAT]')
      ids <- do_esearch(q, FETCH_CAP)
      all_ids <- union(all_ids, ids)
      Sys.sleep(0.15)
    }
  }
  
  # Cap only at the very end, after collecting everything
  unique(all_ids)[seq_len(min(length(all_ids), max_results))]
}


fetch_details <- function(pmids) {
  if (length(pmids) == 0) return(NULL)
  ids_str <- paste(pmids, collapse = ",")
  sum_url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=",
                    ids_str, "&retmode=json")
  abs_url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",
                    ids_str, "&rettype=abstract&retmode=xml")
  tryCatch({
    sum_resp <- GET(sum_url, timeout(20))
    abs_resp <- GET(abs_url, timeout(20))
    sum_data <- fromJSON(content(sum_resp, "text", encoding = "UTF-8"))
    uids     <- sum_data$result$uids
    if (is.null(uids) || length(uids) == 0) return(NULL)
    
    abs_xml   <- read_xml(content(abs_resp, "text", encoding = "UTF-8"))
    abs_nodes <- xml_find_all(abs_xml, "//PubmedArticle")
    abstracts <- list()
    for (n in abs_nodes) {
      pmid <- xml_text(xml_find_first(n, ".//PMID"))
      abst <- paste(xml_text(xml_find_all(n, ".//AbstractText")), collapse = " ")
      abstracts[[pmid]] <- abst
    }
    rows <- lapply(uids, function(uid) {
      item <- sum_data$result[[uid]]
      if (is.null(item)) return(NULL)
      authors <- tryCatch({
        if (!is.null(item$authors) && is.data.frame(item$authors) && nrow(item$authors) > 0)
          paste(item$authors$name, collapse = ", ") else ""
      }, error = function(e) "")
      data.frame(
        pmid     = uid,
        title    = item$title    %||% "",
        authors  = authors,
        journal  = item$fulljournalname %||% item$source %||% "",
        year     = gsub(".*?(\\d{4}).*", "\\1", item$pubdate %||% ""),
        abstract = abstracts[[uid]] %||% "",
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, Filter(Negate(is.null), rows))
  }, error = function(e) { message("fetch_details error: ", e$message); NULL })
}

# ═══════════════════════════════════════════════════════════════════════════
# Keyword classification — instant fallback
# ═══════════════════════════════════════════════════════════════════════════

classify_keywords <- function(title, abstract) {
  text <- tolower(paste(title, abstract))
  
  is_review  <- grepl("\\breview\\b|meta-analysis|systematic review|perspective|commentary|overview", text)
  is_wgs     <- grepl("whole.genome sequenc|\\bwgs\\b|whole genome seq|genome-wide sequenc", text)
  is_exome   <- grepl("exome sequenc|\\bwes\\b|whole.exome|exome capture|exome-wide|targeted exome", text)
  is_scrna   <- grepl(paste0("single.cell rna|\\bscrna-seq\\b|\\bscrnaseq\\b|single.nucleus rna|",
                             "\\bsnrna-seq\\b|10x genomics|dropseq|drop-seq|smart-seq|",
                             "spatial transcriptom"), text)
  is_clinical <- grepl(paste0("patient|patients|clinical trial|cohort|biomarker|cerebrospinal fluid|",
                              "\\bcsf\\b|plasma|serum|\\bmri\\b|\\bpet\\b|neuroimaging|",
                              "longitudinal study|postmortem|post-mortem|autopsy|neuropatholog"), text)
  is_animal   <- grepl(paste0("\\bmouse\\b|\\bmice\\b|\\brat\\b|\\brats\\b|transgenic|knockout|",
                              "drosophila|c\\. elegans|zebrafish|non-human primate|\\bnhp\\b|",
                              "animal model|\\brodent\\b|in vivo"), text)
  is_comp     <- grepl(paste0("bioinformatics|machine learning|deep learning|neural network|",
                              "\\bgwas\\b|rna-seq|proteomics|transcriptomics|algorithm|",
                              "computational|network analysis|simulation"), text)
  is_vitro    <- grepl(paste0("cell culture|\\bipsc\\b|induced pluripotent|\\bneuron\\b|hek293|",
                              "primary culture|organoid|brain organoid|\\bin vitro\\b|biochemical|",
                              "recombinant|purified protein|\\bcrispr\\b|transfection|western blot"), text)
  
  data_type <- if      (is_review)   "Review"
  else if (is_wgs)      "WGS"
  else if (is_exome)    "Exome Sequencing"
  else if (is_scrna)    "scRNA-seq"
  else if (is_clinical) "Clinical"
  else if (is_animal)   "Animal Model"
  else if (is_comp)     "Computational"
  else if (is_vitro)    "In Vitro"
  else                  "Unknown"
  
  focus_tags <- c(
    if (grepl("\\btau\\b|tauopathy|neurofibrillary|phospho.tau|p-tau",                   text)) "Tau",
    if (grepl("tdp-43|tardbp|tdp43",                                                      text)) "TDP-43",
    if (grepl("alpha.synuclein|\\bsynuclein\\b|lewy body|\\bsnca\\b",                    text)) "\u03b1-Synuclein",
    if (grepl("\\bamyloid\\b|\\babeta\\b|a-beta|\\bapp\\b|presenilin|\\bplaque\\b",       text)) "Amyloid",
    if (grepl("neuroinflamm|\\bmicroglia\\b|\\bastrocyte\\b|cytokine|trem2",              text)) "Neuroinflammation",
    if (grepl("aggregat|\\bfibril\\b|oligomer|misfolded|phase.separ|condensate",          text)) "Protein Aggregation",
    if (grepl("\\bautophagy\\b|lysosom|ubiquitin|proteasome|mitophagy",                  text)) "Autophagy",
    if (grepl("whole.genome|exome|scrna|transcriptom|single.cell|spatial",               text)) "Genomics/Sequencing"
  )
  if (length(focus_tags) == 0) focus_tags <- "Other"
  
  list(
    dataType   = data_type,
    focus      = paste(focus_tags, collapse = ", "),
    confidence = "keyword",
    reasoning  = "Keyword-matched (Claude not yet run)"
  )
}

# ═══════════════════════════════════════════════════════════════════════════
# Claude API — batch abstract classification
# Uses claude-haiku-4-5 (fast + cheap)
# Returns data.frame: pmid, data_type, focus, confidence, reasoning
# ═══════════════════════════════════════════════════════════════════════════

claude_classify_batch <- function(papers_df, api_key) {
  
  build_prompt <- function(df) {
    entries <- mapply(function(pmid, title, abstract) {
      sprintf("[PMID:%s]\nTitle: %s\nAbstract: %s",
              pmid, title, substr(abstract, 1, 600))
    }, df$pmid, df$title, df$abstract)
    
    paste0(
      'You are a neuroscience research classifier. Classify each paper below.\n\n',
      'Output ONLY a valid JSON array — no markdown fences, no preamble.\n',
      'Each element must have exactly these fields:\n',
      '  "pmid":       string (copy exactly from input)\n',
      '  "data_type":  one of ["Clinical","Animal Model","In Vitro","Computational",',
      '"Review","WGS","Exome Sequencing","scRNA-seq","Unknown"]\n',
      '  "focus":      comma-separated tags chosen from ["Tau","TDP-43",',
      '"\u03b1-Synuclein","Amyloid","Neuroinflammation","Protein Aggregation",',
      '"Autophagy","Genomics/Sequencing","Other"]\n',
      '  "confidence": "high", "medium", or "low"\n',
      '  "reasoning":  one sentence explaining the data_type choice\n\n',
      'Data-type rules (apply in priority order):\n',
      '1. Review       — systematic review, meta-analysis, perspective, commentary\n',
      '2. WGS          — whole-genome sequencing\n',
      '3. Exome Seq    — whole-exome sequencing / WES\n',
      '4. scRNA-seq    — single-cell/nucleus RNA-seq, spatial transcriptomics\n',
      '5. Clinical     — human patients, clinical trials, CSF/plasma biomarkers,',
      ' imaging, autopsy\n',
      '6. Animal Model — mouse/rat/transgenic/in vivo animal experiments\n',
      '7. Computational — bioinformatics, ML, GWAS, network/systems analysis\n',
      '8. In Vitro     — cell culture, iPSC, organoids, biochemical/structural work\n',
      '9. Unknown      — cannot determine\n\n',
      'Papers:\n\n',
      paste(entries, collapse = "\n\n")
    )
  }
  
  # Default results (filled with keyword values if Claude fails)
  n       <- nrow(papers_df)
  results <- lapply(seq_len(n), function(i) list(
    pmid       = papers_df$pmid[i],
    data_type  = papers_df$data_type[i]  %||% "Unknown",
    focus      = papers_df$focus[i]      %||% "Other",
    confidence = "low",
    reasoning  = "Claude unavailable — keyword fallback"
  ))
  
  tryCatch({
    body <- list(
      model      = "claude-haiku-4-5-20251001",
      max_tokens = 2500,
      messages   = list(list(role = "user", content = build_prompt(papers_df)))
    )
    resp <- POST(
      "https://api.anthropic.com/v1/messages",
      add_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ),
      body    = toJSON(body, auto_unbox = TRUE),
      timeout = 60
    )
    
    if (status_code(resp) == 200) {
      raw      <- fromJSON(content(resp, "text", encoding = "UTF-8"))
      raw_text <- raw$content[[1]]$text
      # Strip any accidental markdown fences
      clean    <- trimws(gsub("```json|```", "", raw_text))
      parsed   <- tryCatch(fromJSON(clean, simplifyVector = FALSE), error = function(e) NULL)
      
      if (!is.null(parsed) && is.list(parsed)) {
        for (item in parsed) {
          idx <- which(papers_df$pmid == as.character(item$pmid))
          if (length(idx) == 1) {
            results[[idx]] <- list(
              pmid       = as.character(item$pmid),
              data_type  = item$data_type  %||% "Unknown",
              focus      = item$focus      %||% "Other",
              confidence = item$confidence %||% "low",
              reasoning  = item$reasoning  %||% ""
            )
          }
        }
      }
    } else {
      msg <- content(resp, "text", encoding = "UTF-8")
      message("Claude API error ", status_code(resp), ": ", msg)
    }
  }, error = function(e) message("claude_classify_batch error: ", e$message))
  
  do.call(rbind, lapply(results, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
}

# ═══════════════════════════════════════════════════════════════════════════
# CSS
# ═══════════════════════════════════════════════════════════════════════════

DARK_CSS <- "
body,.content-wrapper,.main-footer{background:#0a0d14!important;font-family:'Helvetica Neue',Arial,sans-serif;}
.skin-black .main-header .logo,.skin-black .main-header .navbar{background:#0a0f1e!important;border-bottom:1px solid #1a2540;}
.skin-black .main-sidebar{background:#080c18!important;}
.skin-black .sidebar-menu>li>a{color:#7a9abf;}
.skin-black .sidebar-menu>li.active>a,.skin-black .sidebar-menu>li>a:hover{background:#1a2a4a!important;color:#a0c0e0!important;}
.box{background:#0e1525!important;border:1px solid #1e2d45!important;border-top:none!important;color:#b0c0d8;}
.box-header{background:#0e1525!important;border-bottom:1px solid #1e2d45!important;}
.box-title{color:#b0c0d8!important;}
table.dataTable{background:#0e1525!important;color:#b0c0d8!important;}
table.dataTable thead th{background:#0d1120!important;color:#7a9abf!important;border-bottom:1px solid #1e2d45!important;}
table.dataTable tbody tr{background:#0e1525!important;}
table.dataTable tbody tr:hover td{background:#111e35!important;}
table.dataTable tbody td{border-top:1px solid #1a2540!important;}
.dataTables_filter input,.dataTables_length select{background:#0d1120!important;border:1px solid #1e2d45!important;color:#b0c0d8!important;border-radius:5px;padding:3px 7px;}
.dataTables_info,.dataTables_paginate{color:#5a7099!important;}
.paginate_button{background:#0d1120!important;color:#5a7099!important;border:1px solid #1e2d45!important;border-radius:4px!important;}
.paginate_button.current{background:#1a2a4a!important;color:#8ab0f0!important;}
.form-control,.selectize-input{background:#0d1120!important;border:1px solid #1e2d45!important;color:#b0c0d8!important;border-radius:6px;}
.selectize-dropdown{background:#0d1120!important;border:1px solid #1e2d45!important;color:#b0c0d8!important;}
.selectize-dropdown .option:hover,.selectize-dropdown .active{background:#1a2a4a!important;}
.progress{background:#1a2540!important;border-radius:4px;}
.progress-bar{background:#3b63c8!important;}
label{color:#7a9abf!important;}
hr{border-color:#1e2d45;}
.btn-primary{background:linear-gradient(135deg,#3b63c8,#5a4fcf)!important;border:none!important;}
.btn-warning{background:#b8941a!important;border:none!important;color:white!important;}
.btn-success{background:#2a7a4a!important;border:none!important;color:white!important;}
/* Data type badges */
.bdt{border-radius:4px;padding:2px 8px;font-size:11px;font-weight:600;color:white;white-space:nowrap;display:inline-block;}
.bdt-Clinical{background:#e8694a;}
.bdt-AnimalModel{background:#5b9e6b;}
.bdt-InVitro{background:#4a7fc1;}
.bdt-Computational{background:#9b6bbf;}
.bdt-Review{background:#c4a442;}
.bdt-WGS{background:#e05fa0;}
.bdt-ExomeSequencing{background:#3dbfbf;}
.bdt-scRNAseq{background:#f07c3a;}
.bdt-Unknown{background:#7a8999;}
/* Confidence */
.conf-high{color:#5b9e6b;font-size:11px;font-weight:700;}
.conf-medium{color:#c4a442;font-size:11px;font-weight:700;}
.conf-low{color:#7a8999;font-size:11px;}
.conf-keyword{color:#4a7fc1;font-size:11px;}
/* Claude badge */
.claude-badge{display:inline-block;background:linear-gradient(135deg,#3b2a6a,#5a3fa0);
  border:1px solid #7a5fd0;color:#c8b8f8;border-radius:12px;padding:2px 10px;
  font-size:11px;font-weight:600;margin-left:6px;}
/* Detail panel */
.detail-panel{background:#0d1120;border:1px solid #1e2d45;border-radius:10px;padding:18px;margin-top:12px;}
.detail-panel h4{color:#c8d8f0;margin-bottom:10px;}
.detail-panel p{color:#6a8aaa;margin-bottom:6px;font-size:13px;}
.detail-panel .abstract-text{color:#5a7099;font-size:12px;line-height:1.7;}
.reasoning-box{background:#0a1220;border-left:3px solid #5a4fcf;padding:8px 12px;
  border-radius:0 6px 6px 0;margin-top:8px;font-size:12px;color:#8a9abf;font-style:italic;}
/* Filters */
input[type=checkbox]{accent-color:#3b63c8;}
.checkbox label{color:#8fa3c8!important;font-size:12px;}
.filter-badge{display:inline-block;background:#1a2a4a;border:1px solid #2a3a5a;color:#6a90c8;
  border-radius:20px;padding:3px 12px;font-size:11px;font-family:monospace;text-align:center;
  width:100%;margin-top:6px;}
.btn-filter{background:#0d1120!important;border:1px solid #1e2d45!important;color:#8fa3c8!important;
  border-radius:6px!important;font-size:12px!important;width:100%;}
/* API key box */
.api-key-box{background:#0a1520;border:1px solid #1e3050;border-radius:8px;padding:14px;}
.api-status-ok{color:#5b9e6b;font-size:13px;font-weight:600;}
.api-status-warn{color:#b8941a;font-size:13px;}
.api-status-err{color:#e8694a;font-size:13px;}
/* Classify progress */
.classify-progress{background:#0d1525;border:1px solid #1e2d45;border-radius:8px;
  padding:10px 14px;margin-top:8px;font-size:12px;color:#6a8aaa;}
"

# ═══════════════════════════════════════════════════════════════════════════
# UI
# ═══════════════════════════════════════════════════════════════════════════

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = HTML("&#x1F9E0; NeuroPublication Dashboard"),
    tags$li(class = "dropdown", style = "padding:8px 16px 0 0;",
            uiOutput("header_claude_status"))
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Overview",      tabName = "dash", icon = icon("chart-bar")),
      menuItem("Publications",  tabName = "pubs", icon = icon("book-open")),
      menuItem("Scientists",    tabName = "sci",  icon = icon("users")),
      menuItem("AI Classifier", tabName = "ai",   icon = icon("robot")),
      menuItem("Settings",      tabName = "sets", icon = icon("cog"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML(DARK_CSS))),
    tabItems(
      
      # ── OVERVIEW ──────────────────────────────────────────────────────
      tabItem(tabName = "dash",
              fluidRow(
                box(width = 12, status = "primary",
                    title = "Fetch Publications from PubMed (2020\u20132026)",
                    fluidRow(
                      column(5,
                             pickerInput("sel_scientists", "Select scientists  (blank = all 63)",
                                         choices = SCIENTIST_NAMES, multiple = TRUE,
                                         options = list(`live-search` = TRUE, `actions-box` = TRUE,
                                                        `selected-text-format` = "count > 3",
                                                        `count-selected-text`  = "{0} scientists"))),
                      column(2, br(),
                             actionButton("btn_fetch_sel", "Fetch Selected",
                                          class = "btn-primary btn-block", icon = icon("download"))),
                      column(2, br(),
                             actionButton("btn_fetch_all", "Fetch All 63",
                                          class = "btn-warning btn-block", icon = icon("globe"))),
                      column(3, br(), uiOutput("fetch_status_ui"))
                    ),
                    uiOutput("progress_ui"),
                    uiOutput("claude_progress_ui"),
                    hr(style = "border-color:#1a2a40;margin:14px 0 10px;"),
                    
                    # Filter row
                    fluidRow(
                      column(2,
                             tags$label("Scientist",
                                        style = "color:#6a90b8;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             pickerInput("f_scientist", label = NULL,
                                         choices = c("All Scientists" = "All", sort(SCIENTIST_NAMES)),
                                         selected = "All", multiple = FALSE,
                                         options = list(`live-search` = TRUE,
                                                        `style` = "btn-filter", `title` = "All"))),
                      column(2,
                             tags$label("Year",
                                        style = "color:#6a90b8;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             selectInput("f_year", label = NULL,
                                         choices = c("All Years"="All",2026,2025,2024,2023,2022,2021,2020),
                                         selected = "All"),
                             uiOutput("year_debug")),
                      column(3,
                             tags$label("Data Type",
                                        style = "color:#6a90b8;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             div(style = "margin-top:4px;",
                                 checkboxGroupInput("f_datatype", label = NULL,
                                                    choices  = ALL_DATA_TYPES,
                                                    selected = ALL_DATA_TYPES,
                                                    inline   = TRUE)),
                             div(style = "display:flex;gap:6px;margin-top:2px;",
                                 actionLink("sel_all_types",  "All",  style = "font-size:11px;color:#4a7ab8;"),
                                 span("|", style = "color:#2a3a50;font-size:11px;"),
                                 actionLink("sel_none_types", "None", style = "font-size:11px;color:#4a7ab8;"))),
                      column(3,
                             tags$label("Study Focus",
                                        style = "color:#6a90b8;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             div(style = "margin-top:4px;",
                                 checkboxGroupInput("f_focus", label = NULL,
                                                    choices  = ALL_FOCUS_TAGS,
                                                    selected = ALL_FOCUS_TAGS,
                                                    inline   = TRUE)),
                             div(style = "display:flex;gap:6px;margin-top:2px;",
                                 actionLink("sel_all_focus",  "All",  style = "font-size:11px;color:#4a7ab8;"),
                                 span("|", style = "color:#2a3a50;font-size:11px;"),
                                 actionLink("sel_none_focus", "None", style = "font-size:11px;color:#4a7ab8;"))),
                      column(2, br(),
                             selectInput("f_confidence", "Confidence",
                                         choices = c("All"           = "All",
                                                     "High (Claude)" = "high",
                                                     "Medium (Claude)" = "medium",
                                                     "Low (Claude)"  = "low",
                                                     "Keyword only"  = "keyword"),
                                         selected = "All"),
                             actionButton("btn_reset_filters", "Reset Filters",
                                          class = "btn-default btn-block btn-sm", icon = icon("undo")),
                             uiOutput("filter_count_badge"))
                    )
                )
              ),
              
              # Value boxes
              fluidRow(
                valueBoxOutput("vbox_total",      width = 3),
                valueBoxOutput("vbox_clinical",   width = 3),
                valueBoxOutput("vbox_ai_class",   width = 3),
                valueBoxOutput("vbox_classified", width = 3)
              ),
              
              # Charts
              fluidRow(
                box(width = 4, title = "Publications by Data Type",
                    withSpinner(plotlyOutput("plot_datatype",  height = 260), color = "#3b63c8")),
                box(width = 4, title = "Study Focus Areas",
                    withSpinner(plotlyOutput("plot_focus",     height = 260), color = "#3b63c8")),
                box(width = 4, title = "Publications by Year",
                    withSpinner(plotlyOutput("plot_year",      height = 260), color = "#3b63c8"))
              ),
              fluidRow(
                box(width = 12, title = "Top Scientists by Publication Count",
                    withSpinner(plotlyOutput("plot_scientists", height = 220), color = "#3b63c8"))
              )
      ),
      
      # ── PUBLICATIONS ────────────────────────────────────────────────────
      tabItem(tabName = "pubs",
              fluidRow(
                box(width = 12,
                    fluidRow(
                      column(9, h4("Publications", style = "color:#c8d8f0;margin:0;font-size:16px;")),
                      column(3, style = "text-align:right;", uiOutput("reclassify_btn_ui"))
                    ),
                    br(),
                    withSpinner(DTOutput("pub_table"), color = "#3b63c8"),
                    uiOutput("pub_detail_ui")
                )
              )
      ),
      
      # ── SCIENTISTS ──────────────────────────────────────────────────────
      tabItem(tabName = "sci",
              fluidRow(
                box(width = 6, title = "By Institution",
                    withSpinner(plotlyOutput("plot_institution", height = 340), color = "#3b63c8")),
                box(width = 6, title = "Data Type Mix \u2014 Top 15 Scientists",
                    withSpinner(plotlyOutput("plot_sci_type",    height = 340), color = "#3b63c8"))
              )
      ),
      
      # ── AI CLASSIFIER ───────────────────────────────────────────────────
      tabItem(tabName = "ai",
              fluidRow(
                box(width = 12,
                    title = HTML("&#x1F916; Claude AI Classifier \u2014 Live Abstract Analysis"),
                    fluidRow(
                      column(7,
                             p(paste("Claude reads each abstract and assigns a data type, focus tags,",
                                     "a confidence score (high/medium/low), and a one-sentence reasoning note.",
                                     "Classifications run automatically after fetch if a key is present."),
                               style = "color:#6a8aaa;font-size:13px;margin-bottom:14px;"),
                             uiOutput("ai_api_key_status"),
                             br(),
                             fluidRow(
                               column(6,
                                      sliderInput("claude_batch_size",
                                                  "Papers per API call (batch size)",
                                                  min = 3, max = 20, value = 8, step = 1)),
                               column(6, br(),
                                      materialSwitch("claude_auto",
                                                     "Auto-classify after every fetch",
                                                     value = TRUE, status = "primary"))
                             ),
                             uiOutput("ai_classify_controls")
                      ),
                      column(5,
                             div(class = "api-key-box",
                                 tags$label("Anthropic API Key",
                                            style = "color:#8fa3c8;font-size:13px;margin-bottom:6px;display:block;"),
                                 passwordInput("api_key_input", label = NULL,
                                               placeholder = "sk-ant-api03-..."),
                                 actionButton("btn_save_key", "Save Key",
                                              class = "btn-primary btn-sm", icon = icon("key")),
                                 br(), br(),
                                 uiOutput("api_key_feedback"),
                                 p("Your key is stored only in this R session and sent exclusively to",
                                   " api.anthropic.com for classification.",
                                   style = "color:#3a5a70;font-size:11px;margin-top:8px;")
                             )
                      )
                    ),
                    hr(style = "border-color:#1a2a40;"),
                    h4("Classification Log", style = "color:#8fa3c8;font-size:14px;"),
                    withSpinner(DTOutput("ai_log_table"), color = "#5a4fcf")
                )
              )
      ),
      
      # ── SETTINGS ────────────────────────────────────────────────────────
      tabItem(tabName = "sets",
              fluidRow(
                box(width = 5, title = "Fetch Settings",
                    sliderInput("max_per_sci",
                                "Max publications per scientist",
                                min = 5, max = 100, value = 50, step = 5),
                    br(),
                    div(style = "background:#0d1a0d;border:1px solid #1a3a1a;border-radius:8px;padding:12px;",
                        p(icon("check-circle"), " PubMed E-utilities: no key required.",
                          style = "color:#5b9e6b;font-size:13px;margin:0 0 4px;font-weight:600;"),
                        p("Claude Haiku is used for classification (fast + cost-efficient).",
                          style = "color:#5a7099;font-size:12px;margin:0;"))),
                box(width = 4, title = "Export",
                    downloadButton("dl_csv",     "Download CSV (filtered)",
                                   class = "btn-primary btn-block"),
                    br(), br(),
                    downloadButton("dl_csv_all", "Download CSV (all)",
                                   class = "btn-default btn-block"),
                    br(),
                    p("CSV fields: pmid, title, authors, journal, year, scientist,",
                      " data_type, focus, confidence, reasoning.",
                      style = "color:#5a7099;font-size:12px;margin-top:8px;"))
              )
      )
    )
  )
)

# ═══════════════════════════════════════════════════════════════════════════
# SERVER
# ═══════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    publications  = data.frame(),
    fetching      = FALSE,
    fetch_msg     = "",
    fetch_prog    = 0,
    classifying   = FALSE,
    classify_msg  = "",
    classify_prog = 0,
    api_key       = "",
    api_key_valid = FALSE
  )
  
  # ── Filtered data ──────────────────────────────────────────────────────
  filtered_pubs <- reactive({
    df <- rv$publications
    if (nrow(df) == 0) return(df)
    if (!is.null(input$f_scientist) && input$f_scientist != "All")
      df <- df[df$scientist == input$f_scientist, ]
    if (!is.null(input$f_year) && input$f_year != "All")
      df <- df[trimws(as.character(df$year)) == trimws(input$f_year), ]
    if (!is.null(input$f_datatype) && length(input$f_datatype) < length(ALL_DATA_TYPES))
      df <- df[df$data_type %in% input$f_datatype, ]
    if (!is.null(input$f_focus) && length(input$f_focus) < length(ALL_FOCUS_TAGS)) {
      pat <- paste(gsub("([.()+*])", "\\\\\\1", input$f_focus), collapse = "|")
      df  <- df[grepl(pat, df$focus, ignore.case = TRUE), ]
    }
    if (!is.null(input$f_confidence) && input$f_confidence != "All")
      df <- df[df$confidence == input$f_confidence, ]
    df
  })
  
  # ── API key management ─────────────────────────────────────────────────
  observeEvent(input$btn_save_key, {
    key <- trimws(input$api_key_input)
    if (nchar(key) > 20 && startsWith(key, "sk-ant")) {
      rv$api_key       <- key
      rv$api_key_valid <- TRUE
      showNotification("API key saved for this session.", type = "message")
    } else {
      rv$api_key_valid <- FALSE
      showNotification("Key format looks wrong — should start with 'sk-ant'.", type = "error")
    }
  })
  
  output$api_key_feedback <- renderUI({
    if (rv$api_key_valid)
      div(class = "api-status-ok",
          icon("check-circle"), " Key active — Claude Haiku ready")
    else if (nchar(rv$api_key) == 0)
      div(style = "color:#5a7099;font-size:12px;",
          "No key — keyword classification will be used")
    else
      div(class = "api-status-err", icon("times-circle"), " Key format error")
  })
  
  output$header_claude_status <- renderUI({
    if (rv$classifying)
      span(class = "claude-badge", icon("spinner", class = "fa-spin"), " Classifying...")
    else if (rv$api_key_valid)
      span(class = "claude-badge", icon("robot"), " Claude active")
    else
      span(style = "color:#3a5a70;font-size:11px;padding-top:14px;display:block;",
           "\u2328 keyword mode")
  })
  
  output$ai_api_key_status <- renderUI({
    if (rv$api_key_valid)
      div(class = "api-status-ok",
          icon("check-circle"),
          " Anthropic API key active \u2014 Claude Haiku will classify abstracts in batches")
    else
      div(class = "api-status-warn",
          icon("exclamation-triangle"),
          " No API key \u2014 enter your key on the right to enable Claude classification")
  })
  
  # ── PubMed fetch ───────────────────────────────────────────────────────
  do_fetch <- function(scientists) {
    rv$fetching     <- TRUE
    rv$publications <- data.frame()
    total      <- length(scientists)
    all_df     <- list()
    seen_pmids <- character(0)
    
    for (i in seq_along(scientists)) {
      sci           <- scientists[[i]]
      rv$fetch_msg  <- sprintf("Fetching: %s  (%d / %d)", sci$name, i, total)
      rv$fetch_prog <- round((i - 1) / total * 100)
      updateProgressBar(session, "fetch_progress", value = rv$fetch_prog)
      
      pmids      <- fetch_pmids(sci$name, sci$affil, max_results = isolate(input$max_per_sci))
      new_pmids  <- setdiff(pmids, seen_pmids)
      seen_pmids <- c(seen_pmids, new_pmids)
      
      if (length(new_pmids) > 0) {
        details <- fetch_details(new_pmids)
        if (!is.null(details) && nrow(details) > 0) {
          details$scientist <- sci$name
          # Instant keyword classification
          cls                <- mapply(classify_keywords,
                                       details$title, details$abstract,
                                       SIMPLIFY = FALSE)
          details$data_type  <- sapply(cls, `[[`, "dataType")
          details$focus      <- sapply(cls, `[[`, "focus")
          details$confidence <- sapply(cls, `[[`, "confidence")
          details$reasoning  <- sapply(cls, `[[`, "reasoning")
          all_df[[length(all_df) + 1]] <- details
          rv$publications <- do.call(rbind, all_df)
        }
      }
      Sys.sleep(0.35)
    }
    
    rv$fetch_msg  <- ""
    rv$fetch_prog <- 100
    rv$fetching   <- FALSE
    
    # Auto-trigger Claude
    if (rv$api_key_valid && isTRUE(isolate(input$claude_auto))) {
      shinyjs::delay(600, do_claude_classify())
    }
  }
  
  observeEvent(input$btn_fetch_sel, {
    req(!rv$fetching)
    scientists <- if (length(input$sel_scientists) > 0)
      Filter(function(s) s$name %in% input$sel_scientists, SCIENTISTS)
    else SCIENTISTS
    do_fetch(scientists)
  })
  observeEvent(input$btn_fetch_all, {
    req(!rv$fetching)
    do_fetch(SCIENTISTS)
  })
  
  # ── Claude batch classification ────────────────────────────────────────
  do_claude_classify <- function() {
    req(rv$api_key_valid)
    df <- rv$publications
    req(nrow(df) > 0)
    if (rv$classifying) return()
    
    batch_size <- isolate(input$claude_batch_size) %||% 8
    n          <- nrow(df)
    batches    <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
    total_b    <- length(batches)
    
    rv$classifying   <- TRUE
    rv$classify_prog <- 0
    
    for (bi in seq_along(batches)) {
      idx   <- batches[[bi]]
      batch <- df[idx, , drop = FALSE]
      rv$classify_msg  <- sprintf(
        "Claude classifying batch %d / %d  (%d papers) ...", bi, total_b, nrow(batch))
      rv$classify_prog <- round((bi - 1) / total_b * 100)
      updateProgressBar(session, "classify_progress", value = rv$classify_prog)
      
      result <- claude_classify_batch(batch, rv$api_key)
      
      if (!is.null(result) && nrow(result) > 0) {
        for (j in seq_len(nrow(result))) {
          row_idx <- which(rv$publications$pmid == result$pmid[j])
          if (length(row_idx) == 1) {
            rv$publications$data_type[row_idx]  <- result$data_type[j]
            rv$publications$focus[row_idx]       <- result$focus[j]
            rv$publications$confidence[row_idx]  <- result$confidence[j]
            rv$publications$reasoning[row_idx]   <- result$reasoning[j]
          }
        }
      }
      Sys.sleep(0.4)
    }
    
    rv$classify_prog <- 100
    rv$classify_msg  <- ""
    rv$classifying   <- FALSE
    showNotification(
      sprintf("Claude classified %d publications.", n),
      type = "message", duration = 5)
  }
  
  observeEvent(input$btn_classify_now, { do_claude_classify() })
  
  # ── Fetch / classify status UIs ────────────────────────────────────────
  output$fetch_status_ui <- renderUI({
    if (rv$fetching)
      div(style = "color:#8ab0f0;font-size:13px;",
          icon("spinner", class = "fa-spin"), " ", rv$fetch_msg)
    else if (nrow(rv$publications) > 0)
      div(style = "color:#5b9e6b;font-size:13px;",
          icon("check-circle"),
          sprintf(" %d publications loaded", nrow(rv$publications)))
  })
  
  output$progress_ui <- renderUI({
    if (!rv$fetching) return(NULL)
    div(style = "margin-top:10px;",
        progressBar("fetch_progress", value = rv$fetch_prog,
                    display_pct = TRUE, striped = TRUE, status = "info"))
  })
  
  output$claude_progress_ui <- renderUI({
    if (!rv$classifying) return(NULL)
    div(class = "classify-progress",
        icon("robot"), " ", rv$classify_msg, br(),
        progressBar("classify_progress", value = rv$classify_prog,
                    display_pct = TRUE, striped = TRUE, status = "primary"))
  })
  
  output$ai_classify_controls <- renderUI({
    if (!rv$api_key_valid)
      return(p("Enter your API key to enable Claude.", style = "color:#5a7099;font-size:13px;"))
    n    <- nrow(rv$publications)
    if (n == 0)
      return(p("Fetch publications first.", style = "color:#5a7099;font-size:13px;"))
    n_kw <- sum(rv$publications$confidence == "keyword", na.rm = TRUE)
    tagList(
      p(sprintf("%d publications total — %d still have keyword-only classification.", n, n_kw),
        style = "color:#6a8aaa;font-size:13px;"),
      actionButton("btn_classify_now",
                   "Classify / Re-classify All with Claude",
                   class = "btn-success", icon = icon("robot"))
    )
  })
  
  output$reclassify_btn_ui <- renderUI({
    req(rv$api_key_valid, nrow(rv$publications) > 0)
    actionButton("btn_classify_now_pubs",
                 "Re-classify with Claude",
                 class = "btn-success btn-sm", icon = icon("robot"))
  })
  observeEvent(input$btn_classify_now_pubs, { do_claude_classify() })
  
  # ── Filter helpers ─────────────────────────────────────────────────────
  observeEvent(input$sel_all_types,  {
    updateCheckboxGroupInput(session, "f_datatype", selected = ALL_DATA_TYPES) })
  observeEvent(input$sel_none_types, {
    updateCheckboxGroupInput(session, "f_datatype", selected = character(0)) })
  observeEvent(input$sel_all_focus,  {
    updateCheckboxGroupInput(session, "f_focus",    selected = ALL_FOCUS_TAGS) })
  observeEvent(input$sel_none_focus, {
    updateCheckboxGroupInput(session, "f_focus",    selected = character(0)) })
  observeEvent(input$btn_reset_filters, {
    updatePickerInput(session,        "f_scientist", selected = "All")
    updateSelectInput(session,        "f_year",      selected = "All")
    updateSelectInput(session,        "f_confidence",selected = "All")
    updateCheckboxGroupInput(session, "f_datatype",  selected = ALL_DATA_TYPES)
    updateCheckboxGroupInput(session, "f_focus",     selected = ALL_FOCUS_TAGS)
  })
  
  output$year_debug <- renderUI({
    df <- rv$publications
    if (nrow(df) == 0) return(NULL)
    yrs <- sort(unique(trimws(as.character(df$year))), decreasing = TRUE)
    div(style = "font-size:10px;color:#3a5a40;margin-top:3px;",
        paste("In data:", paste(yrs, collapse = ", ")))
  })
  
  output$filter_count_badge <- renderUI({
    n_f <- nrow(filtered_pubs()); n_t <- nrow(rv$publications)
    if (n_t == 0) return(NULL)
    lbl <- if (n_f == n_t) sprintf("All %d pubs", n_t) else sprintf("%d / %d", n_f, n_t)
    div(class = "filter-badge", lbl)
  })
  
  # ── Value boxes ────────────────────────────────────────────────────────
  output$vbox_total <- renderValueBox({
    valueBox(nrow(rv$publications), "Total Publications", icon = icon("book"), color = "blue")
  })
  output$vbox_clinical <- renderValueBox({
    n <- if (nrow(rv$publications) > 0) sum(rv$publications$data_type == "Clinical") else 0
    valueBox(n, "Clinical Studies", icon = icon("hospital"), color = "red")
  })
  output$vbox_ai_class <- renderValueBox({
    n <- if (nrow(rv$publications) > 0)
      sum(rv$publications$confidence %in% c("high","medium","low"), na.rm = TRUE) else 0
    valueBox(n, "AI-Classified", icon = icon("robot"), color = "purple")
  })
  output$vbox_classified <- renderValueBox({
    n <- if (nrow(rv$publications) > 0) sum(rv$publications$data_type != "Unknown") else 0
    valueBox(n, "Data Type Tagged", icon = icon("tags"), color = "green")
  })
  
  # ── Dark Plotly theme ──────────────────────────────────────────────────
  dk <- function(p) p %>% layout(
    paper_bgcolor = "#0e1525", plot_bgcolor = "#0e1525",
    font   = list(color = "#8fa3c8"),
    xaxis  = list(gridcolor = "#1a2540", zerolinecolor = "#1a2540"),
    yaxis  = list(gridcolor = "#1a2540", zerolinecolor = "#1a2540"),
    margin = list(l = 40, r = 20, t = 10, b = 50),
    legend = list(font = list(color = "#8fa3c8"))
  )
  
  # ── Charts ─────────────────────────────────────────────────────────────
  output$plot_datatype <- renderPlotly({
    df <- filtered_pubs()
    if (nrow(df) == 0) return(plotly_empty(type = "pie") %>% dk())
    cnt <- table(df$data_type)
    plot_ly(labels = names(cnt), values = as.numeric(cnt), type = "pie", hole = 0.45,
            marker = list(colors = unname(DATA_TYPE_COLORS[names(cnt)]),
                          line   = list(color = "#0a0d14", width = 2)),
            textinfo = "label+percent") %>% dk()
  })
  
  output$plot_focus <- renderPlotly({
    df <- filtered_pubs()
    if (nrow(df) == 0) return(plotly_empty() %>% dk())
    foci <- unlist(strsplit(df$focus, ", "))
    foci <- foci[foci != "" & foci != "Other"]
    if (length(foci) == 0) return(plotly_empty() %>% dk())
    cnt <- sort(table(foci), decreasing = TRUE)
    plot_ly(x = as.numeric(cnt), y = names(cnt), type = "bar",
            orientation = "h", marker = list(color = "#3a6f58")) %>%
      dk() %>% layout(yaxis = list(autorange = "reversed"))
  })
  
  output$plot_year <- renderPlotly({
    df <- filtered_pubs()
    if (nrow(df) == 0) return(plotly_empty() %>% dk())
    cnt <- table(df$year)
    plot_ly(x = names(cnt), y = as.numeric(cnt), type = "bar",
            marker = list(color = "#3b63c8")) %>% dk()
  })
  
  output$plot_scientists <- renderPlotly({
    df <- rv$publications
    if (nrow(df) == 0) return(plotly_empty() %>% dk())
    top <- min(20, length(unique(df$scientist)))
    cnt <- sort(table(df$scientist), decreasing = TRUE)[1:top]
    plot_ly(x = names(cnt), y = as.numeric(cnt), type = "bar",
            marker = list(color = "#5a4fcf")) %>% dk()
  })
  
  output$plot_institution <- renderPlotly({
    df <- rv$publications
    if (nrow(df) == 0) return(plotly_empty() %>% dk())
    lu  <- data.frame(scientist = SCIENTIST_NAMES,
                      affil     = sapply(SCIENTISTS, `[[`, "affil"),
                      stringsAsFactors = FALSE)
    mg  <- merge(df, lu, by = "scientist", all.x = TRUE)
    cnt <- sort(table(mg$affil), decreasing = TRUE)[1:min(15, length(unique(mg$affil)))]
    plot_ly(x = as.numeric(cnt), y = names(cnt), type = "bar",
            orientation = "h", marker = list(color = "#4a7fc1")) %>%
      dk() %>% layout(yaxis = list(autorange = "reversed"))
  })
  
  output$plot_sci_type <- renderPlotly({
    df <- rv$publications
    if (nrow(df) == 0) return(plotly_empty() %>% dk())
    top <- names(sort(table(df$scientist), decreasing = TRUE))[
      1:min(15, length(unique(df$scientist)))]
    dft <- df[df$scientist %in% top, ]
    plot_ly(dft, x = ~scientist, color = ~data_type, type = "histogram",
            colors = DATA_TYPE_COLORS) %>%
      dk() %>% layout(barmode = "stack", xaxis = list(tickangle = -45))
  })
  
  # ── Publications table ─────────────────────────────────────────────────
  output$pub_table <- renderDT({
    df <- filtered_pubs()
    if (nrow(df) == 0)
      return(datatable(data.frame(Note = "No publications yet. Use Overview to fetch."),
                       rownames = FALSE))
    
    bdt <- function(dt) {
      cl <- gsub(" ", "", dt, fixed = TRUE)
      cl <- gsub("-", "", cl, fixed = TRUE)
      cl <- gsub("/", "", cl, fixed = TRUE)
      sprintf('<span class="bdt bdt-%s">%s</span>', cl, dt)
    }
    conf_html <- function(c) {
      star <- switch(c, high = "\u2605\u2605\u2605", medium = "\u2605\u2605\u2606",
                     low  = "\u2605\u2606\u2606", keyword = "\u2328", "?")
      cls  <- switch(c, high = "conf-high", medium = "conf-medium",
                     low  = "conf-low",  keyword = "conf-keyword", "conf-low")
      sprintf('<span class="%s">%s</span>', cls, star)
    }
    
    display <- data.frame(
      Title = paste0(
        '<a href="https://pubmed.ncbi.nlm.nih.gov/', df$pmid, '" target="_blank">',
        ifelse(nchar(df$title) > 82, paste0(substr(df$title, 1, 82), "\u2026"), df$title),
        '</a>'),
      `Data Type`  = sapply(df$data_type,  bdt),
      Focus        = df$focus,
      Conf         = sapply(df$confidence %||% "keyword", conf_html),
      Year         = df$year,
      Scientist    = df$scientist,
      Journal      = ifelse(nchar(df$journal) > 36,
                            paste0(substr(df$journal, 1, 36), "\u2026"), df$journal),
      check.names  = FALSE, stringsAsFactors = FALSE
    )
    datatable(display, escape = FALSE, selection = "single", rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, dom = "lfrtip",
                             columnDefs = list(
                               list(width = "36%", targets = 0),
                               list(width = "11%", targets = 1),
                               list(width = "7%",  targets = 3))))
  })
  
  output$pub_detail_ui <- renderUI({
    row <- input$pub_table_rows_selected
    df  <- filtered_pubs()
    if (is.null(row) || nrow(df) == 0) return(NULL)
    pub  <- df[row, ]
    col  <- DATA_TYPE_COLORS[pub$data_type] %||% "#7a8999"
    conf <- pub$confidence %||% "keyword"
    conf_lbl <- switch(conf,
                       high    = "\u2605\u2605\u2605 High confidence (Claude)",
                       medium  = "\u2605\u2605\u2606 Medium confidence (Claude)",
                       low     = "\u2605\u2606\u2606 Low confidence (Claude)",
                       keyword = "\u2328 Keyword-matched",
                       "?"
    )
    conf_col <- switch(conf,
                       high   = "#5b9e6b", medium = "#c4a442",
                       low    = "#7a8999", keyword = "#4a7fc1", "#7a8999")
    div(class = "detail-panel",
        h4(pub$title),
        p(strong("Authors: "), pub$authors),
        p(strong("Journal: "), pub$journal, "  \u00b7  ", strong("Year: "), pub$year),
        p(strong("Data Type: "),
          span(pub$data_type, style = paste0("color:", col, ";font-weight:600;")),
          span(paste0("  ", conf_lbl),
               style = paste0("font-size:12px;color:", conf_col, ";margin-left:8px;"))),
        p(strong("Focus: "), pub$focus),
        if (!is.null(pub$reasoning) && nchar(pub$reasoning %||% "") > 5 &&
            !startsWith(pub$reasoning %||% "", "Keyword"))
          div(class = "reasoning-box", icon("robot"), " Claude: ", pub$reasoning),
        if (!is.na(pub$abstract) && nchar(pub$abstract) > 5)
          tagList(strong("Abstract:", style = "color:#8fa3c8;"),
                  p(class = "abstract-text", pub$abstract)),
        a(href   = paste0("https://pubmed.ncbi.nlm.nih.gov/", pub$pmid),
          target = "_blank",
          style  = "color:#3b63c8;font-size:13px;",
          paste0("Open on PubMed (PMID: ", pub$pmid, ") \u2197"))
    )
  })
  
  # ── Scientists table ───────────────────────────────────────────────────
  # ── AI log table ───────────────────────────────────────────────────────
  output$ai_log_table <- renderDT({
    df <- rv$publications
    if (nrow(df) == 0)
      return(datatable(data.frame(Note = "No data — fetch publications first."),
                       rownames = FALSE))
    display <- data.frame(
      PMID        = df$pmid,
      Title       = ifelse(nchar(df$title) > 70,
                           paste0(substr(df$title, 1, 70), "\u2026"), df$title),
      `Data Type` = df$data_type,
      Focus       = df$focus,
      Confidence  = df$confidence %||% "keyword",
      Reasoning   = {
        r <- df$reasoning %||% ""
        ifelse(nchar(r) > 90, paste0(substr(r, 1, 90), "\u2026"), r)
      },
      check.names = FALSE, stringsAsFactors = FALSE
    )
    datatable(display, rownames = FALSE, selection = "none",
              options = list(pageLength = 20, scrollX = TRUE, dom = "lfrtip",
                             columnDefs = list(list(width = "26%", targets = 1),
                                               list(width = "28%", targets = 5))))
  })
  
  # ── Downloads ──────────────────────────────────────────────────────────
  mk_csv <- function(df) {
    cols <- intersect(c("pmid","title","authors","journal","year","scientist",
                        "data_type","focus","confidence","reasoning"), names(df))
    df[, cols, drop = FALSE]
  }
  output$dl_csv <- downloadHandler(
    filename = function() paste0("neuropubs_filtered_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(mk_csv(filtered_pubs()), f, row.names = FALSE))
  output$dl_csv_all <- downloadHandler(
    filename = function() paste0("neuropubs_all_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(mk_csv(rv$publications), f, row.names = FALSE))
}

shinyApp(ui, server)