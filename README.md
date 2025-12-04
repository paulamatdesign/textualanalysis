# textualanalysis

This Shiny app provides a simple interface for exploring and analyzing text data,  
especially useful for UX interview summaries or similar qualitative documents.

Upload one or several `.docx` files and generate:

- Token tables  
- Keyword-in-context (KWIC) searches  
- Topic modeling (LDA)  
- Sentiment analysis  
- Word frequencies  
- Wordclouds  
- Text co-occurrence networks  
- Correspondence analysis (CA)

All processing runs locally while the app is active — **no data is stored or shared**.

---

## Features

### 🔤 Token Table

Shows a sample of the document–feature matrix used for further analysis.

### 🔍 KWIC Search

View how a chosen keyword appears within its surrounding context.

### 🧩 Topic Modeling (LDA)

Identifies recurring themes across your documents.

### 🙂 Sentiment Analysis

Counts positive and negative terms and computes a positivity ratio.

### ☁️ Wordcloud

Highlights the most frequent meaningful words.

### 📊 Word Frequency Plot

Displays the most common words above a chosen frequency threshold.

### 🔗 Text Network

Builds a co-occurrence network to show relationships between terms.

### 🗺️ Correspondence Analysis

Maps documents in a 2D space based on vocabulary similarities.

---

## Installation

Make sure you have the required R packages:

install.packages(c(
  "shiny", "readtext", "readr", "quanteda", "dplyr", "ggplot2",
  "quanteda.textplots", "quanteda.textstats", "quanteda.textmodels",
  "stringr", "seededlda", "lubridate", "lexicon", "FactoMineR",
  "topicmodels", "spacyr"
))

shiny::runApp("app.R")
