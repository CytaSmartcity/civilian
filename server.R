
  
  api_key = "DUunV9m55WIUTAYZmfMLoaoEU"
  api_secret = "YkQyHH4uXIOJnnNB1XgCkj30fCtt7TPKaywu4ljEN99lHsXGVs"
  access_token = "910822811429634048-lLg87KjfIzBomHSThdYr2zMIAw0Io1F"
  access_token_secret = "qqCEnVu7YzBUQu2Hwr9LJfHn6sXVRgkbk5xQdylMPemY4"
  soauth <- setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
  
  library(shiny)
  library(twitteR)
  library(wordcloud)
  library(tm)
  shinyServer(function (input, output) {
    rawData <- reactive(
      { tweets <- searchTwitter(input$term, n=input$cant,lang=input$lang)
      
      return(return(twListToDF(tweets)))
      })
    
    output$tablel <- renderTable( {
      rawData()
    })
    
    output$wordcl<- renderPlot(
      {
        #Extract message
        #tweets_txt <- sapply(rawData(), function(x) x$getText())
        #str(tweets_txt)
        #tweets_txt <- enc2native(rawData()$text)
        #convert to corpus
        #tweets_corpus <- Corpus(VectorSource(tweets_txt))
        
          #tweets_corpus
        #Checking the first message
        #  inspect(tweets_corpus[1])
        #Data cleaning
        #tweets_clean <- tm_map(tweets_corpus, removePunctuation)
        #tweets_clean <- tm_map(tweets_clean, removeWords, stopwords("english"))
        #tweets_clean <- tm_map(tweets_clean, removeNumbers)
        #tweets_clean <- tm_map(tweets_clean, stripWhitespace)
        #tweets_clean <- tm_map(tweets_clean, content_transformer(tolower))
        
        #some extra cleaning
        #tweets_clean <- tm_map(tweets_clean, removeWords, c(input$term))
        
        tw.text <- rawData()$text
        tw.text <- enc2native(rawData()$text)
        tw.text <- tolower(tw.text)
        tw.text <- removeWords(tw.text,c(stopwords('en'),'rt'))
        tw.text <- removeNumbers(tw.text)
        tw.text <- removePunctuation(tw.text)
        tw.text <- stripWhitespace(tw.text)
        tw.text <- unlist(strsplit(tw.text,' '))
        word <- sort(table(tw.text),TRUE)
        #wordc <- head(word,n=15)
        wordcloud(names(word),word,random.color=TRUE,colors=rainbow(10), max.words = 100, scale=c(5,2),min.freq=1)
        #wordcloud(wordc, random.order = FALSE, max.words = 100, scale = c(5, 0.5))
        }
    )
  })

  