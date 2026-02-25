NeuroPublication Dashboard
AI-Powered PubMed Research Classifier | R Shiny + Claude 3.5
An automated dashboard designed to track, classify, and analyze neurodegeneration research (2020–2026). This tool bridges the gap between raw PubMed metadata and actionable research insights using Large Language Models (LLMs).

Key Features
Live PubMed Integration: Fetches real-time metadata (Titles, Abstracts, Authors) via NCBI E-utilities API.

AI Classification: Uses Claude 3.5 (Anthropic API) to categorize abstracts into 9 study types (e.g., Clinical, WGS, scRNA-seq).

Intelligent Fallback: Features a robust keyword-matching system when API keys are unavailable.

Interactive UI: Built with shinydashboard and custom JavaScript for row-level abstract expansion and filtering.

Export Ready: Download filtered research datasets directly to CSV for meta-analysis.

Tech Stack
Language: R (Shiny, Tidyverse, ShinyJS)

APIs: Anthropic Claude (Haiku/Sonnet), PubMed E-utilities (RESTful)

Web: Custom CSS, HTML, JavaScript (DataTable Callbacks)

Data Handling: jsonlite, xml2, httr, dplyr

How to Use
Clone the Repo:

Bash
git clone https://github.com/your-username/neuropub-dashboard.git

Install Dependencies:
Open R and run:

R
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "httr", "jsonlite", "xml2", "DT", "dplyr", "plotly", "shinycssloaders"))
Run the App:

R
shiny::runApp()
(Optional) Enter your Anthropic API Key in the "Settings" tab to enable live AI reasoning.

📊 Project Background
This project was developed to streamline the literature review process for neurodegenerative diseases (Tauopathies, Alzheimer's, etc.). By automating the classification of study methodologies, researchers can quickly identify trends in specific technologies like Single-Cell RNA sequencing or Whole Genome Sequencing.

Pro-Tips for your GitHub:
Add a GIF or Screenshot: Take a screenshot of the dashboard and put it at the very top. Visuals get 10x more attention from recruiters.

The Repository Description: Use this: "R Shiny dashboard utilizing the Claude API to classify neurodegeneration research abstracts from PubMed (2020-2026)."

Topics/Tags: Add these to your repo: r-shiny, bioinformatics, nlp, pubmed-api, data-science, claude-ai.
