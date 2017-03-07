#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
# Dependencies that will need to be loaded into Shiny Application
library(data.table)
library(stringr)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
        
        # Data frames for making predictions
        bigramdf <- readRDS("BigramTraindf.rds")
        trigramdf <- readRDS("TrigramTraindf.rds")
        
        # Key to convert user entered contractions into long form that will match up with the predictors in the processed data
        key <- c("aren't", "are not", "can't", "can not",
                 "could've", "could have", "couldn't", "could not", 
                 "didn't", "did not", "doesn't", "does not", "don't", "do not",
                 "hadn't", "had not", "hasn't", "has not", "haven't", "have not",
                 "he'd", "he had", "he'll", "he will", "he's", "he is", 
                 "how'd", "how did", "how'll", "how will", "how's", "how is", 
                 "i'd", "I would", "i'll", "I will", "i'm", "I am", "i've", "I have", 
                 "isn't", "is not", "it'd", "it had", "it'll", "it will", 
                 "it's", "it is", "mightn't", "might not", "might've", "might have",
                 "mustn't", "must not", "must've", "must have", "needn't", "need not",
                 "she'd", "she had", "she'll", "she will", "she's", "she is",
                 "should've", "should have", "shouldn't", "should not", 
                 "somebody's", "somebody is", "someone's", "someone is", 
                 "something's", "something is", "that'll", "that will", 
                 "that's", "that is", "that'd", "that would", "there'd", "there had",
                 "there's", "there is", "they'd", "they had", "they'll", "they will",
                 "they're", "they are", "they've", "they have", "wasn't", "was not",
                 "we'd", "we had", "we'll", "we will", "we're", "we are", 
                 "we've", "we have", "weren't", "were not", "what'd", "what did",
                 "what'll", "what will", "what're", "what are", "what's", "what is",
                 "what've", "what have", "when's", "when is", "where'd", "where did",
                 "where's", "where is", "where've", "where have", "who'd", "who did", 
                 "who'll", "who will", "who's", "who is", "why'd", "why did", 
                 "why're", "why are", "why's", "why is", "won't", "will not", 
                 "would've", "would have", "wouldn't", "would not", 
                 "you'd", "you would", "you'll", "you will", "you're", "you are",
                 "you've", "you have")
        
        contractions <- key[c(TRUE, FALSE)]
        full <- key[c(FALSE, TRUE)]
        
        # Word prediction algorithm to be used in Shiny Application
        predictalg <- function(ngram, 
                               contr = contractions, 
                               f = full,
                               df1 = trigramdf,
                               df2 = bigramdf,
                               item = 1){
                ngram <- tolower(ngram)
                ngram <- gsub("[^'[:lower:] ]", "", ngram)
                ngram <- gsub("\\s+"," ", ngram)
                ngram <- word(ngram,-2:-1)
                for (i in 1:2){
                        if (ngram[2] %in% contr){
                                index <- which(contr == ngram[2])
                                ngram <- gsub(contr[index], f[index],
                                              ngram[2])
                                ngram <- word(ngram,-2:-1)
                        }
                        if (ngram[1] %in% contr){
                                index <- which(contr == ngram[1])
                                ngram[1] <- gsub(contr[index], f[index],
                                                 ngram[1])
                                ngram[1] <- word(ngram[1],-1)
                        }
                }
                ngram <- paste(ngram[1],ngram[2])
                pred <- data.table()
                prob1 <- 0
                
                if (ngram %in% df1$predictor){
                        pred <- df1[predictor == ngram]
                        prob1 <- sum(pred[,probability])
                }
                
                if (dim(pred)[1] < 3){
                        nm1gram <- word(ngram,-1)
                        pred2 <- df2[predictor == nm1gram]
                        TriBeta <- 1 - prob1
                        pred2$probability <- TriBeta * pred2$probability
                        pred <- rbind.data.frame(pred,pred2)
                        rm(pred2)
                }
                
                pred <- pred %>% group_by(predicted) %>%
                        summarise(probability = sum(probability)) %>%
                        arrange(-probability) %>%
                        top_n(3, probability)
                
                if (dim(pred)[1] == 0){
                        pred <- data.frame(predicted = c(".", "?", "!"), probability =
                                                   c(1/3,1/3,1/3))
                }
                
                return(pred[[1]][item])
        }
        
        # You can access the value of the widget with input$text, e.g.
        #preds <- reactive({
        #        predictalg({ trimws(input$text) })
        #})        
        value1 <- reactive({
                predictalg({ trimws(input$text) }, item = 1)
        })
        value2 <- reactive({
                predictalg({ trimws(input$text) }, item = 2)
        })
        value3 <- reactive({
                predictalg({ trimws(input$text) }, item = 3)
        })
        output$value1 <- value1
        output$value2 <- value2
        output$value3 <- value3
        
        
        #selection = NULL
        observeEvent(input$word1, {
                #selection = value1
                name <- paste(input$text, value1())
                updateTextInput(session, "text", value=name)
        })
        observeEvent(input$word2, {
                #selection = value1
                name <- paste(input$text, value2())
                updateTextInput(session, "text", value=name)
        })
        observeEvent(input$word3, {
                #selection = value3
                name <- paste(input$text, value3())
                updateTextInput(session, "text", value=name)
        })
})