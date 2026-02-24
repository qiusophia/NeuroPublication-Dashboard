# ============================================================
# NeuroPublication Dashboard — R Shiny
# PubMed E-utilities + Claude API abstract classification
# 2020–2026 | Neurodegeneration / Tau / Brain
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

DATA_TYPE_COLORS <- c(
  "Clinical"           = "#e8694a",
  "Animal Model"       = "#5b9e6b",
  "In Vitro"           = "#4a7fc1",
  "Computational"      = "#9b6bbf",
  "Review"             = "#c4a442",
  "Whole Genome Sequencing"                = "#e05fa0",
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

AFFIL_SHORT <- list(
  "University of California, San Francisco" = "(\"University of California San Francisco\" OR UCSF)",
  "University of California Santa Barbara"  = "\"UC Santa Barbara\"",
  "University of California, Los Angeles"   = "UCLA",
  "Washington University in St. Louis"      = "\"Washington University\"",
  "Icahn School of Medicine at Mount Sinai" = "\"Mount Sinai\"",
  "Harvard Medical School"                  = "\"Harvard Medical\"",
  "Massachusetts Institute of Technology"   = "MIT",
  "University of Cambridge"                 = "\"University of Cambridge\"",
  "Mayo Clinic, Jacksonville"               = "\"Mayo Clinic\"",
  "Baylor College of Medicine"              = "\"Baylor College\"",
  "Northwestern University"                 = "Northwestern",
  "Brown University"                        = "\"Brown University\"",
  "University of Edinburgh"                 = "Edinburgh",
  "University College London"               = "\"University College London\"",
  "Albert Einstein College of Medicine"     = "\"Albert Einstein\"",
  "UT Southwestern Medical Center"          = "\"UT Southwestern\"",
  "The Scripps Research Institute"          = "Scripps",
  "Weill Cornell Medical College"           = "\"Weill Cornell\"",
  "University of Wisconsin"                 = "\"University of Wisconsin\"",
  "University of Toronto"                   = "Toronto",
  "University of Oxford"                    = "Oxford",
  "University of Southern California"       = "\"University of Southern California\"",
  "University of Pennsylvania"              = "Pennsylvania",
  "University of Massachusetts"             = "\"University of Massachusetts\"",
  "Washington University"                   = "\"Washington University\"",
  "Neural Stem Cell Institute"              = "\"Neural Stem Cell\"",
  "Colorado State University"               = "\"Colorado State\"",
  "University of Pittsburgh"                = "Pittsburgh",
  "Boston University"                       = "\"Boston University\"",
  "Oxiant Discovery"                        = "Oxiant"
)

do_esearch <- function(query, max_results, max_retries = 3, api_key = "") {
  api_param <- if (nchar(api_key) > 0) paste0("&api_key=", api_key) else ""
  
  delay <- if (nchar(api_key) > 0) 0.15 else 0.4
  
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
    "?db=pubmed&term=", utils::URLencode(query, reserved = TRUE),
    "&retmax=", max_results, "&retmode=json", api_param
  )
  
  for (attempt in 1:max_retries) {
    Sys.sleep(delay) 
    tryCatch({
      resp <- GET(url, timeout(30))
      if (status_code(resp) == 200) {
        txt <- content(resp, "text", encoding = "UTF-8")
        if (!grepl("rate limit", txt, ignore.case = TRUE)) {
          dat <- fromJSON(txt)
          return(dat$esearchresult$idlist %||% character(0))
        }
      }
    }, error = function(e) NULL)
    Sys.sleep(1) 
  }
  return(character(0))
}

fetch_pmids <- function(scientist_name, affiliation, max_results = 50, api_key = "") {
  all_ids <- character(0)
  
  nm1 <- scientist_name
  nm2 <- if (grepl("^\\S+ [A-Z]$", scientist_name)) paste0(scientist_name, "*") else scientist_name
  name_variants <- unique(c(nm1, nm2))
  
  name_clause <- paste(sprintf('"%s"[Author]', name_variants), collapse = " OR ")
  if (length(name_variants) > 1) name_clause <- paste0("(", name_clause, ")")
  
  affil_kw <- AFFIL_SHORT[[affiliation]] %||% paste0('"', affiliation, '"')
  q1 <- paste0(name_clause, " AND ", affil_kw, "[Affiliation] AND 2020:2026[PDAT]")
  all_ids <- union(all_ids, do_esearch(q1, max_results, api_key = api_key))
  
  q2 <- paste0(name_clause, " AND ", NEURO_QUERY, " AND 2020:2026[PDAT]")
  all_ids <- union(all_ids, do_esearch(q2, max_results, api_key = api_key))
  
  q3 <- paste0(name_clause, " AND 2020:2026[PDAT]")
  all_ids <- union(all_ids, do_esearch(q3, max_results * 2, api_key = api_key))
  
  unique(all_ids)[seq_len(min(length(all_ids), max_results))]
}

fetch_details <- function(pmids, max_retries = 3, api_key = "") {
  if (length(pmids) == 0) return(NULL)
  ids_str <- paste(pmids, collapse = ",")
  
  api_param <- if (nchar(api_key) > 0) paste0("&api_key=", api_key) else ""
  delay     <- if (nchar(api_key) > 0) 0.15 else 0.4
  
  sum_url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=",
                    ids_str, "&retmode=json", api_param)
  abs_url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",
                    ids_str, "&rettype=abstract&retmode=xml", api_param)
  
  for (attempt in 1:max_retries) {
    tryCatch({
      Sys.sleep(delay) 
      sum_resp <- GET(sum_url, timeout(30))
      
      Sys.sleep(delay) 
      abs_resp <- GET(abs_url, timeout(30))
      
      if (status_code(sum_resp) == 200 && status_code(abs_resp) == 200) {
        sum_txt <- content(sum_resp, "text", encoding = "UTF-8")
        
        if (!grepl("rate limit", sum_txt, ignore.case = TRUE)) {
          sum_data <- fromJSON(sum_txt)
          uids     <- sum_data$result$uids
          
          if (!is.null(uids) && length(uids) > 0) {
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
            return(do.call(rbind, Filter(Negate(is.null), rows)))
          }
        }
      }
    }, error = function(e) { message("fetch_details retry ", attempt, " failed: ", e$message) })
    Sys.sleep(1)
  }
  return(NULL)
}


classify_keywords <- function(title, abstract) {
  text <- tolower(paste(title, abstract))
  is_review   <- grepl("\\breview\\b|meta-analysis|systematic review|perspective|commentary|overview", text)
  is_wgs      <- grepl("whole.genome sequenc|\\bwgs\\b|whole genome seq|genome-wide sequenc", text)
  is_exome    <- grepl("exome sequenc|\\bwes\\b|whole.exome|exome capture|exome-wide|targeted exome", text)
  is_scrna    <- grepl(paste0("single.cell rna|\\bscrna-seq\\b|\\bscrnaseq\\b|single.nucleus rna|",
                              "\\bsnrna-seq\\b|10x genomics|dropseq|drop-seq|smart-seq|spatial transcriptom"), text)
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
    reasoning  = "Keyword-matched (Claude not yet run)"
  )
}

claude_classify_batch <- function(papers_df, api_key) {
  build_prompt <- function(df) {
    entries <- mapply(function(pmid, title, abstract) {
      sprintf("[PMID:%s]\nTitle: %s\nAbstract: %s",
              pmid, title, substr(abstract, 1, 600))
    }, df$pmid, df$title, df$abstract)
    paste0(
      'You are a neuroscience research classifier. Classify each paper below.\n\n',
      'Output ONLY a valid JSON array -- no markdown fences, no preamble.\n',
      'Each element: "pmid", "data_type", "focus", "reasoning".\n\n',
      'Papers:\n\n', paste(entries, collapse = "\n\n")
    )
  }
  n       <- nrow(papers_df)
  results <- lapply(seq_len(n), function(i) list(
    pmid       = papers_df$pmid[i],
    data_type  = papers_df$data_type[i]  %||% "Unknown",
    focus      = papers_df$focus[i]      %||% "Other",
    reasoning  = "Claude unavailable -- keyword fallback"
  ))
  tryCatch({
    body <- list(
      model      = "claude-haiku-4-5-20251001",
      max_tokens = 2500,
      messages   = list(list(role = "user", content = build_prompt(papers_df)))
    )
    resp <- POST(
      "https://api.anthropic.com/v1/messages",
      add_headers("x-api-key" = api_key, "anthropic-version" = "2023-06-01",
                  "content-type" = "application/json"),
      body = toJSON(body, auto_unbox = TRUE), timeout = 60
    )
    sc <- status_code(resp)
    if (sc == 200) {
      raw      <- fromJSON(content(resp, "text", encoding = "UTF-8"))
      raw_text <- raw$content[[1]]$text
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
              reasoning  = item$reasoning  %||% ""
            )
          }
        }
      } else {
        showNotification("Claude returned unparseable JSON. Check R console.", type = "warning")
        message("Claude raw response: ", substr(raw_text, 1, 500))
      }
    } else {
      err_body <- content(resp, "text", encoding = "UTF-8")
      err_msg  <- tryCatch(fromJSON(err_body)$error$message, error = function(e) err_body)
      showNotification(paste0("Claude API error (", sc, "): ", substr(err_msg, 1, 120)),
                       type = "error", duration = 15)
      message("Claude API error ", sc, ": ", err_body)
    }
  }, error = function(e) {
    showNotification(paste0("Claude connection error: ", e$message), type = "error", duration = 15)
    message("claude_classify_batch error: ", e$message)
  })
  do.call(rbind, lapply(results, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
}

# ═══════════════════════════════════════════════════════════════════════════
# CSS - LIGHT THEME
# ═══════════════════════════════════════════════════════════════════════════

LIGHT_CSS <- "
body, .content-wrapper, .main-footer { font-family: 'Helvetica Neue', Arial, sans-serif; }
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

/* Make the whole row look clickable (except inside the expanded details) */
table.dataTable tbody tr:not(.child) { cursor: pointer; }
table.dataTable tbody tr:hover td { background-color: #f8fafc !important; }

.claude-badge{display:inline-block;background:#eef2ff; border:1px solid #c7d2fe;color:#4f46e5;border-radius:12px;padding:2px 10px; font-size:11px;font-weight:600;margin-left:6px;}
.detail-panel{background:#ffffff;border:1px solid #e2e8f0;border-radius:8px;padding:16px; margin: 4px 0;}
.detail-panel h4{color:#1e293b;margin-bottom:10px; font-size: 16px;}
.detail-panel p{color:#475569;margin-bottom:6px;font-size:13px;}
.detail-panel .abstract-text{color:#334155;font-size:12px;line-height:1.7;}
.reasoning-box{background:#f8fafc;border-left:3px solid #5a4fcf;padding:8px 12px; border-radius:0 6px 6px 0;margin-top:8px;font-size:12px;color:#475569;font-style:italic;}
.filter-badge{display:inline-block;background:#f1f5f9;border:1px solid #cbd5e1;color:#475569; border-radius:20px;padding:3px 12px;font-size:11px;font-family:monospace;text-align:center; width:100%;margin-top:6px;}
.api-key-box{background:#f8fafc;border:1px solid #e2e8f0;border-radius:8px;padding:14px;}
.api-status-ok{color:#16a34a;font-size:13px;font-weight:600;}
.api-status-warn{color:#d97706;font-size:13px;}
.api-status-err{color:#dc2626;font-size:13px;}
.classify-progress{background:#f8fafc;border:1px solid #e2e8f0;border-radius:8px; padding:10px 14px;margin-top:8px;font-size:12px;color:#475569;}
.overview-pubs-count{display:inline-block;background:#f1f5f9;border:1px solid #cbd5e1; color:#475569;border-radius:12px;padding:1px 10px;font-size:12px; font-family:monospace;margin-left:8px;vertical-align:middle;}
"


ui <- dashboardPage(
  skin = "blue", 
  dashboardHeader(
    title = HTML("Publication Dashboard"),
    tags$li(class = "dropdown", style = "padding:8px 16px 0 0;",
            uiOutput("header_claude_status"))
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Overview",      tabName = "dash"),
      menuItem("AI Classifier", tabName = "ai"),
      menuItem("Settings",      tabName = "sets")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML(LIGHT_CSS))),
    tabItems(
      
      tabItem(tabName = "dash",
              fluidRow(
                box(width = 12, status = "primary",
                    title = "Search Publications from PubMed",
                    fluidRow(
                      column(5,
                             pickerInput("sel_scientists", "Select scientists",
                                         choices = SCIENTIST_NAMES, multiple = TRUE,
                                         options = list(`live-search` = TRUE, `actions-box` = TRUE,
                                                        `selected-text-format` = "count > 3",
                                                        `count-selected-text`  = "{0} scientists"))),
                      column(2, br(),
                             actionButton("btn_fetch_sel", "Search",
                                          class = "btn-primary btn-block")),
                      column(2, br(),
                             actionButton("btn_fetch_all", "Search All",
                                          class = "btn-warning btn-block")),
                      column(3, br(), uiOutput("fetch_status_ui"))
                    ),
                    uiOutput("progress_ui"),
                    uiOutput("claude_progress_ui"),
                    hr(style = "border-color:#e2e8f0;margin:14px 0 10px;"),
                    fluidRow(
                      column(2,
                             tags$label("Year",
                                        style = "color:#475569;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             selectInput("f_year", label = NULL,
                                         choices = c("All Years"="All",2026,2025,2024,2023,2022,2021,2020),
                                         selected = "All"),
                             uiOutput("year_debug")),
                      column(3,
                             tags$label("Data Type",
                                        style = "color:#475569;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             div(style = "margin-top:4px;",
                                 checkboxGroupInput("f_datatype", label = NULL,
                                                    choices  = ALL_DATA_TYPES,
                                                    selected = ALL_DATA_TYPES,
                                                    inline   = TRUE)),
                             div(style = "display:flex;gap:6px;margin-top:2px;",
                                 actionLink("sel_all_types",  "All",  style = "font-size:11px;color:#2563eb;"),
                                 span("|", style = "color:#cbd5e1;font-size:11px;"),
                                 actionLink("sel_none_types", "None", style = "font-size:11px;color:#2563eb;"))),
                      column(3,
                             tags$label("Study Focus",
                                        style = "color:#475569;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             div(style = "margin-top:4px;",
                                 checkboxGroupInput("f_focus", label = NULL,
                                                    choices  = ALL_FOCUS_TAGS,
                                                    selected = ALL_FOCUS_TAGS,
                                                    inline   = TRUE)),
                             div(style = "display:flex;gap:6px;margin-top:2px;",
                                 actionLink("sel_all_focus",  "All",  style = "font-size:11px;color:#2563eb;"),
                                 span("|", style = "color:#cbd5e1;font-size:11px;"),
                                 actionLink("sel_none_focus", "None", style = "font-size:11px;color:#2563eb;"))),
                      column(2, br(),
                             actionButton("btn_reset_filters", "Reset Filters",
                                          class = "btn-default btn-block btn-sm", icon = icon("undo")),
                             uiOutput("filter_count_badge"))
                    )
                )
              ),
              fluidRow(
                valueBoxOutput("vbox_total",      width = 4),
                valueBoxOutput("vbox_clinical",   width = 4),
                valueBoxOutput("vbox_classified", width = 4)
              ),
              fluidRow(
                box(width = 12, status = "primary",
                    title = uiOutput("overview_pubs_title"),
                    collapsible = TRUE, collapsed = FALSE,
                    withSpinner(DTOutput("overview_pub_table"), color = "#3b63c8")
                )
              )
      ),
      
      tabItem(tabName = "ai",
              fluidRow(
                box(width = 12, status = "primary",
                    title = HTML("&#x1F916; Claude AI Classifier \u2014 Live Abstract Analysis"),
                    fluidRow(
                      column(7,
                             p(paste("Claude reads each abstract and assigns a data type, focus tags,",
                                     "and a one-sentence reasoning note."),
                               style = "color:#475569;font-size:13px;margin-bottom:14px;"),
                             uiOutput("ai_api_key_status"),
                             br(),
                             fluidRow(
                               column(6,
                                      sliderInput("claude_batch_size", "Papers per API call",
                                                  min = 3, max = 20, value = 8, step = 1)),
                               column(6, br(),
                                      materialSwitch("claude_auto", "Auto-classify after every fetch",
                                                     value = TRUE, status = "primary"))
                             ),
                             uiOutput("ai_classify_controls")
                      ),
                      column(5,
                             div(class = "api-key-box",
                                 tags$label("Anthropic API Key",
                                            style = "color:#475569;font-size:13px;margin-bottom:6px;display:block;"),
                                 passwordInput("api_key_input", label = NULL,
                                               placeholder = "sk-ant-api03-..."),
                                 actionButton("btn_save_key", "Save Key",
                                              class = "btn-primary btn-sm", icon = icon("key")),
                                 br(), br(),
                                 uiOutput("api_key_feedback"),
                                 p("Key stored in session only, sent only to api.anthropic.com.",
                                   style = "color:#64748b;font-size:11px;margin-top:8px;")
                             )
                      )
                    ),
                    hr(style = "border-color:#e2e8f0;"),
                    h4("Classification Log", style = "color:#475569;font-size:14px;"),
                    withSpinner(DTOutput("ai_log_table"), color = "#5a4fcf")
                )
              )
      ),
      
      tabItem(tabName = "sets",
              fluidRow(
                box(width = 5, status = "primary", title = "Fetch Settings",
                    sliderInput("max_per_sci", "Max publications per scientist",
                                min = 5, max = 100, value = 50, step = 5),
                    br(),
                    div(class = "api-key-box", style = "margin-bottom:14px;",
                        tags$label("NCBI API Key (Optional)",
                                   style = "color:#475569;font-size:13px;margin-bottom:6px;display:block;"),
                        passwordInput("ncbi_api_key_input", label = NULL,
                                      placeholder = "Paste NCBI key for VIP speed..."),
                        actionButton("btn_save_ncbi_key", "Save Key",
                                     class = "btn-primary btn-sm", icon = icon("key")),
                        uiOutput("ncbi_key_feedback"),
                        p("Boosts limit from 3 req/sec to 10 req/sec.",
                          style = "color:#64748b;font-size:11px;margin-top:8px;")
                    ),
                    div(style = "background:#f0fdf4;border:1px solid #bbf7d0;border-radius:8px;padding:12px;",
                        p(icon("check-circle"), " PubMed E-utilities: Adaptive speed limits enabled.",
                          style = "color:#16a34a;font-size:13px;margin:0 0 4px;font-weight:600;"),
                        p("Claude Haiku is used for classification.",
                          style = "color:#475569;font-size:12px;margin:0;"))),
                box(width = 4, status = "primary", title = "Export",
                    downloadButton("dl_csv",     "Download CSV (filtered)", class = "btn-primary btn-block"),
                    br(), br(),
                    downloadButton("dl_csv_all", "Download CSV (all)",      class = "btn-default btn-block"),
                    br(),
                    p("Fields: pmid, title, authors, journal, year, scientist, data_type, focus, reasoning.",
                      style = "color:#475569;font-size:12px;margin-top:8px;"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    publications       = data.frame(),
    fetching           = FALSE,
    fetch_msg          = "",
    fetch_prog         = 0,
    classifying        = FALSE,
    classify_msg       = "",
    classify_prog      = 0,
    api_key            = "",
    api_key_valid      = FALSE,
    api_key_ncbi       = "",
    api_key_ncbi_valid = FALSE
  )
  
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
    df
  })
  
  observeEvent(input$btn_save_key, {
    key <- trimws(input$api_key_input)
    if (nchar(key) > 20 && startsWith(key, "sk-ant")) {
      rv$api_key       <- key
      rv$api_key_valid <- TRUE
      showNotification("Claude API key saved for this session.", type = "message")
    } else {
      rv$api_key_valid <- FALSE
      showNotification("Claude Key format looks wrong.", type = "error")
    }
  })
  
  output$api_key_feedback <- renderUI({
    if (rv$api_key_valid) div(class = "api-status-ok", icon("check-circle"), " Key active")
    else if (nchar(rv$api_key) == 0) div(style = "color:#64748b;font-size:12px;", "No key -- keyword mode")
    else div(class = "api-status-err", icon("times-circle"), " Key format error")
  })
  output$header_claude_status <- renderUI({
    if (rv$classifying) span(class = "claude-badge", icon("spinner", class = "fa-spin"), " Classifying...")
    else if (rv$api_key_valid) span(class = "claude-badge", icon("robot"), " Claude active")
    else span(style = "color:#64748b;font-size:11px;padding-top:14px;display:block;", "\u2328 keyword mode")
  })
  output$ai_api_key_status <- renderUI({
    if (rv$api_key_valid)
      div(class = "api-status-ok", icon("check-circle"), " API key active")
    else
      div(class = "api-status-warn", icon("exclamation-triangle"), " No API key -- enter key to enable Claude")
  })
  
  observeEvent(input$btn_save_ncbi_key, {
    key <- trimws(input$ncbi_api_key_input)
    if (nchar(key) > 10) {
      rv$api_key_ncbi       <- key
      rv$api_key_ncbi_valid <- TRUE
      showNotification("NCBI API key saved! Fetching speed increased.", type = "message")
    } else if (nchar(key) == 0) {
      rv$api_key_ncbi       <- ""
      rv$api_key_ncbi_valid <- FALSE
      showNotification("NCBI API key cleared. Reverting to standard speed limit.", type = "warning")
    } else {
      rv$api_key_ncbi_valid <- FALSE
      showNotification("NCBI key looks invalid.", type = "error")
    }
  })
  
  output$ncbi_key_feedback <- renderUI({
    if (rv$api_key_ncbi_valid) div(class = "api-status-ok", style="margin-top:8px;", icon("check-circle"), " NCBI Key Active (Fast Mode)")
    else div(style = "color:#64748b;font-size:12px;margin-top:8px;", "No key -- Standard Speed Limit")
  })
  
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
      
      pmids      <- fetch_pmids(sci$name, sci$affil, max_results = isolate(input$max_per_sci), api_key = rv$api_key_ncbi)
      new_pmids  <- setdiff(pmids, seen_pmids)
      seen_pmids <- c(seen_pmids, new_pmids)
      
      if (length(new_pmids) > 0) {
        details <- fetch_details(new_pmids, api_key = rv$api_key_ncbi)
        if (!is.null(details) && nrow(details) > 0) {
          details$scientist <- sci$name
          cls                <- mapply(classify_keywords, details$title, details$abstract, SIMPLIFY = FALSE)
          details$data_type  <- sapply(cls, `[[`, "dataType")
          details$focus      <- sapply(cls, `[[`, "focus")
          details$reasoning  <- sapply(cls, `[[`, "reasoning")
          all_df[[length(all_df) + 1]] <- details
          rv$publications <- do.call(rbind, all_df)
        }
      }
      Sys.sleep(0.05)
    }
    rv$fetch_msg  <- ""
    rv$fetch_prog <- 100
    rv$fetching   <- FALSE
    if (rv$api_key_valid && isTRUE(isolate(input$claude_auto)))
      shinyjs::delay(600, do_claude_classify())
  }
  
  observeEvent(input$btn_fetch_sel, {
    req(!rv$fetching)
    scientists <- if (length(input$sel_scientists) > 0)
      Filter(function(s) s$name %in% input$sel_scientists, SCIENTISTS)
    else SCIENTISTS
    do_fetch(scientists)
  })
  observeEvent(input$btn_fetch_all, { req(!rv$fetching); do_fetch(SCIENTISTS) })
  
  do_claude_classify <- function() {
    req(rv$api_key_valid)
    df <- rv$publications
    req(nrow(df) > 0)
    if (rv$classifying) return()
    batch_size <- isolate(input$claude_batch_size) %||% 8
    n                  <- nrow(df)
    batches    <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
    total_b    <- length(batches)
    rv$classifying   <- TRUE
    rv$classify_prog <- 0
    for (bi in seq_along(batches)) {
      idx   <- batches[[bi]]
      batch <- df[idx, , drop = FALSE]
      rv$classify_msg  <- sprintf("Claude classifying batch %d / %d  (%d papers) ...", bi, total_b, nrow(batch))
      rv$classify_prog <- round((bi - 1) / total_b * 100)
      updateProgressBar(session, "classify_progress", value = rv$classify_prog)
      result <- claude_classify_batch(batch, rv$api_key)
      if (!is.null(result) && nrow(result) > 0) {
        for (j in seq_len(nrow(result))) {
          row_idx <- which(rv$publications$pmid == result$pmid[j])
          if (length(row_idx) == 1) {
            rv$publications$data_type[row_idx]  <- result$data_type[j]
            rv$publications$focus[row_idx]       <- result$focus[j]
            rv$publications$reasoning[row_idx]   <- result$reasoning[j]
          }
        }
      }
      Sys.sleep(0.1)
    }
    rv$classify_prog <- 100
    rv$classify_msg  <- ""
    rv$classifying   <- FALSE
    showNotification(sprintf("Claude classified %d publications.", n), type = "message", duration = 5)
  }
  
  observeEvent(input$btn_classify_now, { do_claude_classify() })
  
  output$fetch_status_ui <- renderUI({
    if (rv$fetching)
      div(style = "color:#2563eb;font-size:13px;", icon("spinner", class = "fa-spin"), " ", rv$fetch_msg)
    else if (nrow(rv$publications) > 0)
      div(style = "color:#16a34a;font-size:13px;", icon("check-circle"),
          sprintf(" %d publications loaded", nrow(rv$publications)))
  })
  output$progress_ui <- renderUI({
    if (!rv$fetching) return(NULL)
    div(style = "margin-top:10px;",
        progressBar("fetch_progress", value = rv$fetch_prog, display_pct = TRUE, striped = TRUE, status = "info"))
  })
  output$claude_progress_ui <- renderUI({
    if (!rv$classifying) return(NULL)
    div(class = "classify-progress",
        icon("robot"), " ", rv$classify_msg, br(),
        progressBar("classify_progress", value = rv$classify_prog, display_pct = TRUE, striped = TRUE, status = "primary"))
  })
  output$ai_classify_controls <- renderUI({
    if (!rv$api_key_valid) return(p("Enter your API key to enable Claude.", style = "color:#64748b;font-size:13px;"))
    n <- nrow(rv$publications)
    if (n == 0) return(p("Fetch publications first.", style = "color:#64748b;font-size:13px;"))
    tagList(
      p(sprintf("%d publications loaded.", n), style = "color:#475569;font-size:13px;"),
      actionButton("btn_classify_now", "Classify / Re-classify All with Claude", class = "btn-success", icon = icon("robot"))
    )
  })
  
  observeEvent(input$sel_all_types,  { updateCheckboxGroupInput(session, "f_datatype", selected = ALL_DATA_TYPES) })
  observeEvent(input$sel_none_types, { updateCheckboxGroupInput(session, "f_datatype", selected = character(0)) })
  observeEvent(input$sel_all_focus,  { updateCheckboxGroupInput(session, "f_focus",    selected = ALL_FOCUS_TAGS) })
  observeEvent(input$sel_none_focus, { updateCheckboxGroupInput(session, "f_focus",    selected = character(0)) })
  observeEvent(input$btn_reset_filters, {
    updatePickerInput(session,        "f_scientist",  selected = "All")
    updateSelectInput(session,        "f_year",       selected = "All")
    updateCheckboxGroupInput(session, "f_datatype",   selected = ALL_DATA_TYPES)
    updateCheckboxGroupInput(session, "f_focus",      selected = ALL_FOCUS_TAGS)
  })
  output$year_debug <- renderUI({
    df <- rv$publications
    if (nrow(df) == 0) return(NULL)
    yrs <- sort(unique(trimws(as.character(df$year))), decreasing = TRUE)
    div(style = "font-size:10px;color:#475569;margin-top:3px;", paste("In data:", paste(yrs, collapse = ", ")))
  })
  output$filter_count_badge <- renderUI({
    n_f <- nrow(filtered_pubs()); n_t <- nrow(rv$publications)
    if (n_t == 0) return(NULL)
    lbl <- if (n_f == n_t) sprintf("All %d pubs", n_t) else sprintf("%d / %d", n_f, n_t)
    div(class = "filter-badge", lbl)
  })
  
  
  # ── Detail Renderer ────────────────────────────────────────────────────
  render_detail <- function(pub) {
    if (is.null(pub)) return("")
    col <- DATA_TYPE_COLORS[pub$data_type] %||% "#7a8999"
    
    html_content <- div(class = "detail-panel",
                        h4(pub$title),
                        p(strong("Authors: "), pub$authors),
                        p(strong("Journal: "), pub$journal, "  \u00b7  ", strong("Year: "), pub$year),
                        p(strong("Data Type: "),
                          span(pub$data_type, style = paste0("color:", col, ";font-weight:600;"))),
                        p(strong("Focus: "), pub$focus),
                        if (!is.null(pub$reasoning) && nchar(pub$reasoning %||% "") > 5 && !startsWith(pub$reasoning %||% "", "Keyword"))
                          div(class = "reasoning-box", icon("robot"), " Claude: ", pub$reasoning),
                        if (!is.na(pub$abstract) && nchar(pub$abstract) > 5)
                          tagList(strong("Abstract:", style = "color:#475569;"), p(class = "abstract-text", pub$abstract)),
                        a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/", pub$pmid), target = "_blank",
                          style = "color:#3b63c8;font-size:13px;", paste0("Open on PubMed (PMID: ", pub$pmid, ") \u2197"))
    )
    
    return(as.character(html_content))
  }
  
  build_display_df <- function(df) {
    bdt <- function(dt) {
      cl <- gsub("[/ -]", "", dt)
      sprintf('<span class="bdt bdt-%s">%s</span>', cl, dt)
    }
    
    # Pre-render the HTML abstract details for every row
    details_html <- sapply(seq_len(nrow(df)), function(i) {
      render_detail(df[i, ])
    })
    
    # We removed the "+" column here entirely
    data.frame(
      Title       = paste0('<a href="https://pubmed.ncbi.nlm.nih.gov/', df$pmid, '" target="_blank">',
                           ifelse(nchar(df$title) > 82, paste0(substr(df$title, 1, 82), "\u2026"), df$title), '</a>'),
      `Data Type` = sapply(df$data_type, bdt),
      Focus       = df$focus,
      Year        = df$year,
      Scientist   = df$scientist,
      Journal     = ifelse(nchar(df$journal) > 36, paste0(substr(df$journal, 1, 36), "\u2026"), df$journal),
      `_details`  = details_html, # Now located at index 6 (since R is 1-indexed, but JS is 0-indexed)
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  
  output$overview_pubs_title <- renderUI({
    n_f <- nrow(filtered_pubs()); n_t <- nrow(rv$publications)
    if (n_t == 0) return(span("Filtered Publications", style = "color:#1e293b;"))
    lbl <- if (n_f == n_t) sprintf("%d", n_t) else sprintf("%d / %d", n_f, n_t)
    tagList(
      span("Filtered Publications", style = "color:#1e293b;"),
      span(lbl, class = "overview-pubs-count")
    )
  })
  
  # ── Table Renderer with Whole-Row Click JS ─────────────────────────────
  output$overview_pub_table <- renderDT({
    df <- filtered_pubs()
    if (nrow(df) == 0) {
      msg <- if (nrow(rv$publications) == 0)
        "No publications fetched yet -- use the buttons above to fetch from PubMed."
      else
        "No publications match the current filters."
      return(datatable(data.frame(Note = msg), rownames = FALSE,
                       options = list(dom = "t", paging = FALSE)))
    }
    
    display_df <- build_display_df(df)
    
    datatable(display_df, escape = FALSE, selection = "none", rownames = FALSE, 
              options = list(
                pageLength = 12, scrollX = TRUE, dom = "lfrtip",
                columnDefs = list(
                  list(visible = FALSE, targets = 6), # Hide the _details column (JS index 6)
                  list(width = "35%", targets = 0)    # Title column width
                )
              ),
              # JavaScript callback to listen for clicks anywhere on the row
              callback = JS("
        table.on('click', 'tbody td', function(e) {
          // If the user clicked the hyperlink specifically, just let them go to PubMed!
          if (e.target.tagName === 'A') return;
          
          var tr = $(this).closest('tr');
          
          // Do not collapse if the user is clicking inside the already-expanded abstract text
          if (tr.hasClass('child') || tr.closest('.child').length) return; 
          
          var row = table.row(tr);
          
          if (row.child.isShown()) {
            row.child.hide();
          } else {
            // Show the hidden HTML from index 6
            row.child(row.data()[6]).show(); 
          }
        });
      ")
    )
  })
  
  output$ai_log_table <- renderDT({
    df <- rv$publications
    if (nrow(df) == 0)
      return(datatable(data.frame(Note = "No data -- fetch publications first."), rownames = FALSE))
    display <- data.frame(
      PMID        = df$pmid,
      Title       = ifelse(nchar(df$title) > 70, paste0(substr(df$title, 1, 70), "\u2026"), df$title),
      `Data Type` = df$data_type,
      Focus       = df$focus,
      Reasoning   = { r <- df$reasoning %||% ""; ifelse(nchar(r) > 90, paste0(substr(r, 1, 90), "\u2026"), r) },
      check.names = FALSE, stringsAsFactors = FALSE
    )
    datatable(display, rownames = FALSE, selection = "none",
              options = list(pageLength = 20, scrollX = TRUE, dom = "lfrtip",
                             columnDefs = list(list(width = "26%", targets = 1),
                                               list(width = "28%", targets = 5))))
  })
  
  mk_csv <- function(df) {
    cols <- intersect(c("pmid","title","authors","journal","year","scientist",
                        "data_type","focus","reasoning"), names(df))
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