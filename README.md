# NeuroPublication Dashboard
### **AI-Powered PubMed Research Classifier | R Shiny + Claude 3.5**

An automated dashboard designed to track, classify, and analyze neurodegeneration research (2020–2026). This tool bridges the gap between raw PubMed metadata and actionable research insights using Large Language Models (LLMs).

---

## Key Features

* **Live PubMed Integration:** Fetches real-time metadata (Titles, Abstracts, Authors) via NCBI E-utilities API.
* **AI Classification:** Uses **Claude 3.5 (Anthropic API)** to categorize abstracts into 9 study types (e.g., Clinical, WGS, scRNA-seq).
* **Intelligent Fallback:** Features a robust keyword-matching system for classification when API keys are unavailable.
* **Interactive UI:** Built with `shinydashboard` and custom **JavaScript** for row-level abstract expansion and seamless filtering.
* **Export Ready:** Download filtered research datasets directly to CSV for downstream meta-analysis.

---

## Tech Stack

* **Language:** R (Shiny, Tidyverse, ShinyJS)
* **APIs:** Anthropic Claude (Haiku/Sonnet), PubMed E-utilities (RESTful)
* **Web:** Custom CSS, HTML, JavaScript (DataTable Callbacks)
* **Data Handling:** `jsonlite`, `xml2`, `httr`, `dplyr`

---

## How to Use

Clone the Repo
```bash
git clone [https://github.com/qiusophia/NeuroPublication-Dashboard.git](https://github.com/qiusophia/NeuroPublication-Dashboard.git)
```bash


Install in R
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "httr", 
                   "jsonlite", "xml2", "DT", "dplyr", "plotly", "shinycssloaders"))
Run
shiny::runApp()
