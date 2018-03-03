
woeid <- read_csv("data/woeid.csv")
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  if(file.exists("data/titanic_rf.csv")) file.remove("data/titanic_rf.csv")
  source("MachineLearning.R")
  
  dataDevTrain <- reactive({
    inFileDevTrain <- input$datatrain
    if(is.null(inFileDevTrain))
      return(NULL)
    
    read.csv(inFileDevTrain$datapath, stringsAsFactors = input$strToFactor1)
  })
  
  output$structure <- renderPrint({
    ifelse(is.null(dataDevTrain()), "No Data available",
           return(str(dataDevTrain()))
    )
  })
  
  output$summarize <- renderPrint({
    ifelse(is.null(dataDevTrain()), "No Data available",
           return(summary(dataDevTrain()))
    )
  })
  
  output$prevDevTrain <- renderDataTable({
    dataDevTrain()
  })
  
  output$dwnTrain <- downloadHandler(
    filename = function() {
      "titanic-train.csv"
    },
    content = function(file) {
      file.copy("data/titanic-train.csv", file)
    }
  )
  
  dataPredTest <- reactive({
    inFilePredTest <- input$datapred
    if(is.null(inFilePredTest))
      return(NULL)
    
    read.csv(inFilePredTest$datapath, stringsAsFactors = input$strToFactor2)
  })
  
  output$univariateTrain <- renderUI({
    selectInput("univarTrain", "Select variable", choices = names(dataDevTrain()))
  })
  
  output$uniSummary <- renderPrint({
    ifelse(is.null(dataDevTrain()), "No Data available",
           return(summary(dataDevTrain()[,input$univarTrain]))
    )
  })
  
  output$isNA <- renderPrint({
    ifelse(is.null(dataDevTrain()), "No Data available",
           return(sum(is.na(dataDevTrain()[,input$univarTrain]) | dataDevTrain()[,input$univarTrain]==""))
    )
  })
  
  output$uniplotTrain <- renderPlot({
    if(is.null(input$datatrain))
      return(NULL)
    
    if(is.numeric(dataDevTrain()[, input$univarTrain]))
    {
      ggplot(dataDevTrain(), aes_string(x = input$univarTrain)) + geom_histogram(fill = "blue")
    } else if(is.character(dataDevTrain()[, input$univarTrain]))
    {
      ggplot(dataDevTrain(), aes_string(x = input$univarTrain)) + geom_bar(aes_string(fill = input$univarTrain))
    }
  })
  
  
   
   observeEvent(input$preprocDev, {
     if(is.null(dataDevTrain()))
     {
       showModal(modalDialog(
         title = "Error",
         "No data available",
         easyClose = FALSE
       ))
     } else {
       showModal(modalDialog(
         title = "Processing...",
         "Please wait. This will take several minutes.",
         easyClose = TRUE,
         footer = NULL
       ))
       preprocedDev <<- preProc(dataDevTrain())
       showModal(modalDialog(
         title = "Information",
         "Processes Done",
         easyClose = FALSE
       ))
     }
   })
   
   observeEvent(input$modDev, {
     if(is.null(dataDevTrain()))
     {
       showModal(modalDialog(
         title = "Error",
         "No data available",
         easyClose = FALSE
       ))
     } else {
       showModal(modalDialog(
         title = "Processing...",
         "Please wait. This will take several minutes.",
         easyClose = TRUE,
         footer = NULL
       ))
       newMod <<- modelDev(preprocedDev)
       showModal(modalDialog(
         title = "Information",
         "Processes Done",
         easyClose = FALSE
       ))
     }
   })
   
   output$metric <- renderPrint({
     confMat(newMod)
   })
   
   
   
   output$dwnTest <- downloadHandler(
     filename = function() {
       "titanic-test.csv"
     },
     content = function(file) {
       file.copy("data/titanic-test.csv", file)
     }
   )
   
   output$univariateTest <- renderUI({
     selectInput("univarTest", "Select variable", choices = names(dataPredTest()))
   })
   
   output$predSummary <- renderPrint({
     ifelse(is.null(dataPredTest()), "No Data available",
            return(summary(dataPredTest()[,input$univarTest]))
     )
   })
   
   output$predisNA <- renderPrint({
     ifelse(is.null(dataPredTest()), "No Data available",
            return(paste("Number of missing value:",sum(is.na(dataPredTest()[,input$univarTest]) | dataPredTest()[,input$univarTest]=="")))
     )
   })
   
   output$uniplotTest <- renderPlot({
     if(is.null(input$datapred))
       return(NULL)
     
     if(is.numeric(dataPredTest()[, input$univarTest]))
     {
       ggplot(dataPredTest(), aes_string(x = input$univarTest)) + geom_histogram(fill = "blue")
     } else if(is.character(dataPredTest()[, input$univarTest]))
     {
       ggplot(dataPredTest(), aes_string(x = input$univarTest)) + geom_bar(aes_string(fill = input$univarTest))
     }
   })
   
   observeEvent(input$procPred, {
     if(is.null(input$datapred))
     {
       # showNotification("Error: No data available", type = "error")
       showModal(modalDialog(
         title = "Error!",
         "No data available",
         easyClose = FALSE
       ))
     } else {
       # showNotification("Processing...", type = "message", duration = 15)
       showModal(modalDialog(
         title = "Processing...",
         "Please wait. This will take several minutes.",
         easyClose = TRUE,
         footer = NULL
       ))
       
       predTestFun(preprocedPred)
       # showNotification("Process Done!", type = "message", closeButton = TRUE, duration = 15)
       showModal(modalDialog(
         title = "Information",
         "Process Done!",
         easyClose = FALSE
       ))
     }
   })
   
   output$dwnPred <- downloadHandler(
     filename = function() {
       if(file.exists("data/titanic_rf.csv"))
       {
         "titanic-test-predicted.csv"
       } else {
         # showNotification("Error: No Data Available")
         showModal(modalDialog(
           title = "Error!",
           "No data available",
           easyClose = FALSE
         ))
         return("No-Data-Available.csv")
       }
       },
     content = function(file) {
       if(file.exists("data/titanic_rf.csv"))
       {
         file.copy("data/titanic_rf.csv", file)
       } else {
         # showNotification("Error: No Data Available")
         showModal(modalDialog(
           title = "Error!",
           "No data available",
           easyClose = FALSE
         ))
         testPred <- data.frame("PassengerId" = NA, "Survived" = NA)
         write.csv(testPred, "data/titanic_rf.csv", row.names = F)
         file.copy("data/titanic_rf.csv", file)
         if(file.exists("data/titanic_rf.csv")) file.remove("data/titanic_rf.csv")
       }
       }
   )
   
   observeEvent(input$preprocPred, {
     if(is.null(dataPredTest()))
     {
       showModal(modalDialog(
         title = "Error",
         "No data available",
         easyClose = FALSE
       ))
     } else {
       showModal(modalDialog(
         title = "Processing...",
         "Please wait. This will take several minutes.",
         easyClose = TRUE,
         footer = NULL
       ))
       preprocedPred <<- preProc(dataPredTest(), dev = FALSE)
       showModal(modalDialog(
         title = "Information",
         "Processes Done",
         easyClose = FALSE
       ))
     }
   })
   
   # Twitter Mining
   

   observe({
     x <- input$woecountry
     if(x=="World")
     {
       updateSelectInput(session, "woearea", label = "Location/Area", choices = "Worldwide")
     } else {
       updateSelectInput(session, "woearea", label = "Location/Area", choices = woeid$name[woeid$country == x])
     }
   })

   trendtop <- eventReactive(input$trendBut, {
     trending <- getTrends(woeid$woeid[woeid$name == input$woearea])
     names(trending)[1:2] <- c("Trending Topic", "URL")
     return(trending[,1:2])
   })
   
   output$trends <- renderTable({
     if(input$trendBut == 0) return(NULL)
     trendtop()
   })
   
   worcl <- eventReactive(input$searchtw, {
     if(input$searchterm=="")
     {
       # showNotification("Error: No data available", type = "error")
       showModal(modalDialog(
         title = "Error!",
         "Keyword can not blank",
         easyClose = FALSE
       ))
     } else {
       # showNotification("Processing...", type = "message", duration = 15)
       showModal(modalDialog(
         title = "Processing...",
         "Please wait. This will take several minutes.",
         easyClose = TRUE,
         footer = NULL
       ))
       
       {
       
       cat("\nsearching...\n")
       
       tweets <- twListToDF(searchTwitteR(input$searchterm, n = input$ntweets, lang = "id"))
       tweets <- tweets[tweets$isRetweet==FALSE, ]
       
       clean_tweet = gsub("&amp;", "", tweets$text)
       clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
       clean_tweet = gsub("@\\w+", "", clean_tweet)
       clean_tweet = gsub("#\\w+", "", clean_tweet)
       clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
       clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
       clean_tweet = gsub("http\\w+", "", clean_tweet) 
       clean_tweet = gsub("\\n", " ", clean_tweet) 
       clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet)
       clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet)
       clean_tweet = gsub("[^0-9 a-z A-Z ///]", "", clean_tweet)
       
       clean_tweet = tolower(clean_tweet)
       clean_tweet = clean_tweet[clean_tweet!='']
       
       stopw = as_tibble(read.table("data/stopword-id.txt", header = T, stringsAsFactors = F))
       
       text_df <- data_frame(line = 1:length(clean_tweet), text = clean_tweet)
       
       tidy_text <- text_df %>% 
         unnest_tokens(word, text) %>%
         anti_join(stopw)
       
       tidy_text1 <- sapply(as.character(tidy_text$word), katadasaR)
       tidy_txt <- table(tidy_text1)
       showModal(modalDialog(
         title = "Information",
         "Process Done!",
         easyClose = FALSE
       ))
       
       return(tidy_txt)
     }
     }
     
   })
   
   output$wordcld <- renderWordcloud2({
     wordcloud2(worcl())
   })
   
   
   # Web Scraping
   
})

