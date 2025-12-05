# app.R
library(shiny)
library(readtext)
library(quanteda)
library(dplyr)
library(ggplot2)

library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.textmodels)      # needed for textmodel_ca()
library(topicmodels)

##### UI #####

ui <- fluidPage(
  fluidRow(
    column(
      width = 8,
      offset = 2,
      
      titlePanel("Textual Analysis"),
      
      tags$style(HTML("
          @import url('https://fonts.googleapis.com/css2?family=Funnel+Display:wght@300..800&family=Funnel+Sans:wght@300..800&display=swap');
      
          html, p {
              font-family: 'Funnel Sans', sans-serif !important;
          }
      
          h1, h2, h3 {
              font-family: 'Funnel Display', sans-serif !important;
              font-weight: 800 !important;
          }
          
          body {
            padding: 4rem 0;
          }
          .citation {
            font-size: smaller;
            color: #767676;
          }
          .intro {
            font-size: larger;
          }
          .loading-text {
            margin-top: 2rem;
            background: gold;
            padding: 0.5rem 1.1rem;
            border-radius: 20px;
            font-size: smaller;
            width: fit-content;
          }
          .scroll-x {
            width: 100%;
            overflow-x: auto;
          }
          .scroll-x > table, 
          .scroll-x > div {
            width: 100% !important;
          }
      ")),
      
      p(HTML(
        'This R-based app lets you upload .docx files (often UX interview summaries) and run several types of text analysis. Your data is never stored or shared. Created by <a href="https://paulamatdesign.github.io/" target="_blank">Paul Amat</a>.'
      ), class = "intro"),
      
      p(HTML(
        'See <a href="https://github.com/paulamatdesign/textualanalysis" target="_blank">GitHub and Licence details</a>. Made with <a href="https://quanteda.io/" target="_blank">Quanteda</a>.'
      ), class = "citation"),
      
      fileInput("docs", "Upload .docx file(s)", multiple = TRUE, accept = ".docx"),
      
      actionButton("load_example", "Show an example"),
      
      # Everything below will be created only when files are loaded
      uiOutput("analysis_panels")
    )
  )
)

##### Server #####

server <- function(input, output, session) {
  
  # Only show analysis UI once we have some data
  output$analysis_panels <- renderUI({
    req(datafiles())
    tagList(
      p("Loading... It may take a few seconds.", class = "loading-text"),
      h3("Token Table (extract)"),
      p(
        "Shows a small sample of the document–feature matrix built from your files."
      ),
      div(class = "scroll-x", tableOutput("show_tokens")),
      
      h3("KWIC Search"),
      p(
        "Lets you pick a keyword and view it in context, so you can see how people ",
        "actually use that word in their sentences."
      ),
      textInput("kwic_pattern", "Enter keyword", "need"),
      div(class = "scroll-x", tableOutput("show_kwic")),
      
      h3("LDA (Topics)"),
      p(
        "Runs a topic model (LDA) on your corpus and lists the main words for each ",
        "topic, helping you spot recurring themes across documents."
      ),
      sliderInput("n_topics", "Topics number", 2, 20, 8, step = 1),
      div(class = "scroll-x", tableOutput("show_lda")),
      
      h3("Sentiment Analysis"),
      p(
        "Counts positive and negative words and computes a simple positivity ratio, ",
        "to give you a quick sense of overall tone."
      ),
      div(class = "scroll-x", tableOutput("show_sentiment")),
      
      h3("Wordcloud"),
      p(
        "Displays the most frequent meaningful words as a wordcloud, with size ",
        "roughly reflecting how often they appear."
      ),
      div(class = "scroll-x", plotOutput("show_wordcloud")),
      
      h3("Frequency"),
      p(
        "Plots the most frequent words above the chosen threshold, so you can see ",
        "which terms dominate your corpus."
      ),
      sliderInput("freq_limit", "Minimum frequency", 5, 100, 20, step = 5),
      div(class = "scroll-x", plotOutput("show_freq")),
      
      h3("Textplot Network"),
      p(
        "Builds a co-occurrence network of words, showing which terms tend to appear ",
        "together in the same context."
      ),
      sliderInput("network_limit", "Minimum frequency", 60, 400, 120, step = 5),
      div(class = "scroll-x", plotOutput("show_network")),
      
      h3("Correspondence Analysis"),
      p(
        "Maps documents in a 2D space based on their vocabulary, helping you see ",
        "which texts are similar or different from each other."
      ),
      p("At least 3 documents must be loaded.", class = "loading-text"),
      div(class = "scroll-x", plotOutput("show_ca"))
    )
  })
  
  # Load files (user-uploaded or examples/)
  datafiles <- reactive({
    # trigger on either docs or example button
    if (input$load_example > 0) {
      example_files <- list.files("examples", pattern = "\\.docx$", full.names = TRUE)
      req(length(example_files) > 0)
      imported <- readtext(example_files)
    } else {
      req(input$docs)
      imported <- readtext(input$docs$datapath)
    }
    imported
  })
  
  # Build raw corpus
  raw <- reactive({
    imported <- datafiles()
    
    data <- data.frame(
      doc_id = imported$doc_id,
      text   = imported$text,
      stringsAsFactors = FALSE
    )
    
    corpus(data, text_field = "text")
  })
  
  # Build clean corpus
  corp <- reactive({
    raw() %>%
      tokens(remove_punct = TRUE) %>%
      tokens_replace(
        pattern     = lexicon::hash_lemmas$token,
        replacement = lexicon::hash_lemmas$lemma
      ) %>%
      tokens_wordstem() %>%
      dfm() %>%
      dfm_remove(pattern = stopwords("en"))
  })
  
  #### TOKEN TABLE ####
  output$show_tokens <- renderTable({
    df <- corp() %>%
      convert(to = "data.frame")
    
    head(df, 10)[ , 1:12]
  })
  
  #### CORRESPONDENCE ANALYSIS ####
  output$show_ca <- renderPlot({
    tmod_ca <- corp() %>%
      textmodel_ca()
    
    dat_ca <- data.frame(
      dim1 = coef(tmod_ca, doc_dim = 1)$coef_document,
      dim2 = coef(tmod_ca, doc_dim = 2)$coef_document
    )
    
    ggplot(dat_ca, aes(x = dim1, y = dim2, label = rownames(dat_ca))) +
      #geom_point(aes(color = dim1), size = 2) +
      
      # text offset using nudge_x / nudge_y
      geom_text(
        aes(color = dim1),
        #nudge_x = 0.20,       # adjust horizontal offset
        #nudge_y = 0.16,       # adjust vertical offset
        size = 5
      ) +
      
      # smooth color gradient
      scale_color_gradient(
        low = "#1f77b4",      # blue
        high = "#d62728"      # red
      ) +
      
      coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2)) +
      labs(x = "Dimension 1", y = "Dimension 2") +
      guides(color = "none", fill = "none", linetype = "none") +
      theme_minimal(base_size = 14)
    
  })
  
  #### WORDCLOUD ####
  output$show_wordcloud <- renderPlot({
    corp() %>%
      textplot_wordcloud(max_words = 80, rotation = 0, min_size = 0.85)
  })
  
  #### KWIC ####
  output$show_kwic <- renderTable({
    req(input$kwic_pattern)
    
    x <- raw() %>%
      tokens(remove_punct = TRUE) %>%
      tokens_replace(
        pattern     = lexicon::hash_lemmas$token,
        replacement = lexicon::hash_lemmas$lemma
      ) %>%
      kwic(pattern = input$kwic_pattern, window = 7) %>%
      head(10)
      
      as.data.frame(x)[, c("docname", "from", "to", "pre", "keyword", "post")]
  })
  
  #### LDA TOPICS ####
  output$show_lda <- renderTable({
    dfm <- corp()
    
    # simple guard: need enough features for LDA
    req(nfeat(dfm) > 0, ndoc(dfm) > 1)
    
    lda_model <- LDA(convert(dfm, to = "topicmodels"), k = input$n_topics)
    
    # convert term matrix to a data.frame for nicer printing
    as.data.frame(terms(lda_model, 10))
  })
  
  #### SENTIMENT ####
  output$show_sentiment <- renderTable({
    raw() %>%
      tokens(remove_punct = TRUE) %>%
      tokens_replace(
        pattern     = lexicon::hash_lemmas$token,
        replacement = lexicon::hash_lemmas$lemma
      ) %>%
      tokens_wordstem() %>%
      tokens_lookup(dictionary = data_dictionary_LSD2015) %>%
      dfm() %>%
      convert(to = "data.frame") %>%
      mutate(
        positive = ifelse(is.na(positive), 0, positive),
        negative = ifelse(is.na(negative), 0, negative),
        ratio_pos = ifelse(
          positive + negative > 0,
          round(positive / (negative + positive), 2),
          NA_real_
        )
      )
  })
  
  #### NETWORK ####
  output$show_network <- renderPlot({
    req(input$network_limit)
    
    corp() %>%
      fcm() %>%
      textplot_network(min_freq = input$network_limit, edge_size = 1)
  })
  
  #### FREQUENCY PLOT ####
  output$show_freq <- renderPlot({
    req(input$freq_limit)
    
    d <- corp() %>%
      textstat_frequency(n = input$freq_limit)
    
    d %>%
      ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
      geom_point() +
      coord_flip() +
      labs(x = NULL, y = "Frequency") +
      theme_minimal()
  })
}

shinyApp(ui, server)
