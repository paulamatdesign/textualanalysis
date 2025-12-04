# app.R
library(shiny)
library(readtext)
library(readr)
library(quanteda)
library(dplyr)
library(ggplot2)

library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.textmodels)
library(stringr)
library(seededlda)
library(lubridate)
library(lexicon)
library(FactoMineR)
library(topicmodels)
library(spacyr)

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
      ")),
      
      p(
        "This R-based app lets you upload .docx files (often UX interview summaries) ",
        "and run several types of text analysis, including tokens, KWIC, sentiment, topics, ",
        "word frequencies, and more. Everything is processed locally while the app is running—",
        "your data is never stored or shared. Created by ",
        a("Paul Amat", href = "https://paulamatdesign.github.io/", target = "_blank"),
        "."
      ),
      
      fileInput("docs", "Upload .docx file(s)", multiple = TRUE),
      
      # Everything below will be created only when files are loaded
      uiOutput("analysis_panels")
    )
  )
)

##### Server #####

server <- function(input, output, session) {
  corp <- reactive({
    req(input$docs)
    
    imported <- readtext(input$docs$datapath)
    
    doc_id <- imported$doc_id
    text   <- imported$text
    
    data <- data.frame(
      doc_id = doc_id,
      text   = text,
      stringsAsFactors = FALSE
    )
    
    corpus(data, text_field = "text")
  })
  
  output$analysis_panels <- renderUI({
    req(input$docs)  # if no files, nothing is shown
    
    tagList(
      h3("Token Table (extract)"),
      p(
        "Shows a small sample of the document–feature matrix built from your files, ",
        "so you can see how the text is turned into data for the other analyses."
      ),
      tableOutput("show_tokens"),
      
      h3("KWIC Search"),
      p(
        "Lets you pick a keyword and view it in context, so you can see how people ",
        "actually use that word in their sentences."
      ),
      textInput("kwic_pattern", "Enter keyword", "need"),
      tableOutput("show_kwic"),
      
      h3("LDA (Topics)"),
      p(
        "Runs a topic model (LDA) on your corpus and lists the main words for each ",
        "topic, helping you spot recurring themes across documents."
      ),
      tableOutput("show_lda"),
      
      h3("Sentiment Analysis"),
      p(
        "Counts positive and negative words and computes a simple positivity ratio, ",
        "to give you a quick sense of overall tone."
      ),
      tableOutput("show_sentiment"),
      
      h3("Wordcloud"),
      p(
        "Displays the most frequent meaningful words as a wordcloud, with size ",
        "roughly reflecting how often they appear."
      ),
      plotOutput("show_wordcloud"),
      
      h3("Frequency"),
      p(
        "Plots the most frequent words above the chosen threshold, so you can see ",
        "which terms dominate your corpus."
      ),
      numericInput("freq_limit", "Minimum frequency", 20),
      plotOutput("show_freq"),
      
      h3("Textplot Network"),
      p(
        "Builds a co-occurrence network of words, showing which terms tend to appear ",
        "together in the same context."
      ),
      numericInput("network_limit", "Minimum frequency", 120),
      plotOutput("show_network"),
      
      h3("Correspondence Analysis"),
      p(
        "Maps documents in a 2D space based on their vocabulary, helping you see ",
        "which texts are similar or different from each other."
      ),
      plotOutput("show_ca")
    )
  })
  
  output$show_tokens <- renderTable({
    dfm_matrix <- corp() %>% 
      tokens() %>%
      dfm()
    
    # convert dfm to a regular data frame
    df <- convert(dfm_matrix, to = "data.frame")
    
    df[, 0:13]
  })
  
  output$show_ca <- renderPlot({
    tmod_ca <- corp() %>%
      tokens(remove_punct = TRUE) %>%
      tokens_replace(
        pattern     = lexicon::hash_lemmas$token,
        replacement = lexicon::hash_lemmas$lemma
      ) %>%
      tokens_wordstem() %>%
      dfm() %>%
      dfm_remove(pattern = stopwords("en")) %>%
      textmodel_ca()
    
    dat_ca <- data.frame(dim1 = coef(tmod_ca, doc_dim = 1)$coef_document, 
                         dim2 = coef(tmod_ca, doc_dim = 2)$coef_document)
    
    ggplot(dat_ca, aes(x = dim1, y = dim2, label = rownames(dat_ca))) +
      geom_text(color = rgb(0, 0, 0, 0.7), size = 5) +
      coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2)) +
      labs(x = "Dimension 1", y = "Dimension 2") +
      theme_minimal()
  })
  output$show_wordcloud <- renderPlot({
    corp() %>%
      tokens(remove_punct = TRUE) %>%
      tokens_replace(
        pattern     = lexicon::hash_lemmas$token,
        replacement = lexicon::hash_lemmas$lemma
      ) %>%
      tokens_wordstem() %>%
      dfm() %>%
      dfm_remove(pattern = stopwords("en")) %>%
      textplot_wordcloud(max_words = 80, rotation = 0, min_size = 0.85)
  })
  output$show_kwic <- renderTable({
    req(input$kwic_pattern)           # make sure a keyword is entered
    kw <- corp() %>%
      tokens(remove_punct = TRUE) %>%
      tokens_replace(
        pattern     = lexicon::hash_lemmas$token,
        replacement = lexicon::hash_lemmas$lemma
      ) %>%
      kwic(pattern = input$kwic_pattern, window = 7) %>%
      head(10)
    
    # turn kwic into a data.frame and keep the main columns
    df <- as.data.frame(kw)[, c("docname", "from", "to", "pre", "keyword", "post")]
    df
  })
  output$show_lda <- renderTable({
    dfm <- corp() %>%
      tokens(remove_punct = TRUE) %>%
      tokens_replace(
        pattern     = lexicon::hash_lemmas$token,
        replacement = lexicon::hash_lemmas$lemma
      ) %>%
      dfm() %>%
      dfm_remove(pattern = stopwords("en"))
    
    lda_model <- LDA(convert(dfm, to = "topicmodels"), k = 8)
    
    terms(lda_model, 10)  # les 5 mots les plus représentatifs par thème
  })
  output$show_sentiment <- renderTable({
    corp() %>%
      tokens(remove_punct = TRUE) %>%
      tokens_replace(
        pattern     = lexicon::hash_lemmas$token,
        replacement = lexicon::hash_lemmas$lemma
      ) %>%
      tokens_wordstem() %>%
      tokens_lookup(dictionary =  data_dictionary_LSD2015) %>%
      dfm() %>%
      convert(to = "data.frame") %>%
      mutate(ratio_pos = round(positive / (negative+positive), 2))
  })
  output$show_network <- renderPlot({
    req(input$network_limit)
    corp() %>%
      tokens(remove_punct = TRUE) %>%
      tokens_replace(
        pattern     = lexicon::hash_lemmas$token,
        replacement = lexicon::hash_lemmas$lemma
      ) %>%
      dfm() %>%
      dfm_remove(pattern = stopwords("en")) %>%
      fcm() %>%
      textplot_network(min_freq = input$network_limit)
  })
  output$show_freq <- renderPlot({
    req(input$freq_limit)
    d <- corp() %>%
      tokens(remove_punct = TRUE) %>%
      tokens_replace(
        pattern     = lexicon::hash_lemmas$token,
        replacement = lexicon::hash_lemmas$lemma
      ) %>%
      tokens_wordstem() %>%
      dfm() %>%
      dfm_remove(pattern = stopwords("en")) %>%
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
