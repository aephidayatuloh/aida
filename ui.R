library(shiny)
library(shinyjs)
library(caret)
library(randomForest)
library(doSNOW)
library(ggplot2)
# library(RSQLite)
library(ROAuth)
library(twitteR)
library(stringr)
library(wordcloud2)
library(readr)
library(dplyr)
library(tidytext)
library(katadasaR)

source("TwitterAuth.R")

woeid <- read_csv("data/woeid.csv")

twitter.connect()
cat(1)
print(1)


 PAGE_TITLE <- "Application for<br/>Integrated Data Analytic"
shinyUI(
  fluidPage(
    tags$head(
      tags$style(type='text/css', ".predbutt { vertical-align: middle; height: 50px; width: 15%; font-size: 20px; align: middle; display: flex; justify-content: center;}")
      # tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
    ),
    fluidRow(titlePanel(windowTitle = "AIDAnalytic",
                        title =
                          div(column(8,
                                     h1(HTML(PAGE_TITLE), style = "font-size: 230%; font-weight: bold; color: darkblue;")
                          ),
                          column(4,
                                 img(
                                   src = "images/aidanalytics2.png",
                                   # height = 70,
                                   width = 310 #,
                                   # style = "margin:10px 10px; color: blue;"
                                 ),
                                 img(
                                   src = "images/marketing-data-analytics_chart.png",
                                   # height = 70,
                                   width = 300,
                                   style = "margin:10px 10px; color: blue;"
                                 )
                          )
                          )
    )
    ),
    br(),
    p("Finding an accurate machine learning is not the end of the project.
      
      In this post you will discover how to finalize your machine learning model in R including: making predictions on unseen data, re-building the model from scratch and saving your model for later use.
      
      Let’s get started."),
    tabsetPanel(type = "pills", 
                tabPanel("Predictive",
                         h1("Predictive Modeling", style = "text-transformation: bold;"),
                         column(3,
                                wellPanel("If you want to try to build model (Model Development) on this app, you need to download the training data first.",
                                          br(),
                                          downloadButton("dwnTrain", "Training"),
                                          br(),
                                          br(),
                                          "If you want to try to predict using built model (Predictive) on this app, you need to download the test data first.",
                                          br(),
                                          downloadButton("dwnTest", "Test"),
                                          style = "background: #347bbc; color: white; text-transformation: bold;")
                         ),
                         column(9,
                                tabsetPanel(type = "pills",
                                            tabPanel("Home",
                                                     br(),
                                                     fluidRow(
                                                       br(),
                                                       column(6,
                                                              HTML('<iframe width="900" height="490" src="https://www.youtube.com/embed/z8PRU46I3NY" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>')
                                                       )
                                                     ),
                                                     p(HTML("<strong>DISCLAIMER</strong>"), "This is not the model builder web app platform like H2O or Microsoft Azure Machine Learning Studio. This app is built only using Titanic dataset from Kaggle Titanic machine learning competition. So, use other datasets would not fit this app and would be error.")
                                            ),
                                            tabPanel("Model Development",
                                                     h1("Build Predictive Model"),
                                                     tabsetPanel(
                                                       tabPanel("Data",
                                                                br(),
                                                                h3("How To Use"),
                                                                p("You must click the", HTML("<strong>Browse...</strong>"), "button and upload data you want to use as train data."),
                                                                column(9,
                                                                       fileInput("datatrain", "Upload Data File"),
                                                                       checkboxInput("strToFactor1", "String as factor")
                                                                )
                                                       ),
                                                       tabPanel("Preview",
                                                                br(),
                                                                column(12,
                                                                       verbatimTextOutput("structure")
                                                                ),
                                                                column(12,
                                                                       verbatimTextOutput("summarize")
                                                                ),
                                                                uiOutput("univariateTrain"),
                                                                verbatimTextOutput("uniSummary"),
                                                                p("Is there any missing value or blank?"),
                                                                verbatimTextOutput("isNA"),
                                                                plotOutput("uniplotTrain"),
                                                                dataTableOutput("prevDevTrain")),
                                                       tabPanel("Preprocess",
                                                                br(),
                                                                actionButton("preprocDev", "Auto Pre-processing")
                                                       ),
                                                       tabPanel("Modeling",
                                                                br(),
                                                                actionButton("modDev", "Develop Model")
                                                       ),
                                                       tabPanel("Metrics",
                                                                verbatimTextOutput("metric"))
                                                     )
                                            ),
                                            tabPanel("Predictive",
                                                     h1("Use Predictive Model"),
                                                     tabsetPanel(
                                                       tabPanel("Data",
                                                                br(),
                                                                h3("How To Use"),
                                                                p("You must click the", HTML("<strong>Browse...</strong>"), "button and upload data you want to predict."),
                                                                fileInput("datapred", "Upload Data File"),
                                                                checkboxInput("strToFactor2", "String as factor")
                                                                ),
                                                       tabPanel("Preview",
                                                                br(),
                                                                uiOutput("univariateTest"),
                                                                verbatimTextOutput("predSummary"),
                                                                verbatimTextOutput("predisNA"),
                                                                plotOutput("uniplotTest")
                                                       ),
                                                       tabPanel("Preprocess",
                                                                br(),
                                                                actionButton("preprocPred", "Auto Pre-processing")),
                                                       tabPanel("Predict",
                                                                br(),
                                                                div(class = "predbutt",
                                                                    actionButton("procPred", "Predict Now!")
                                                                ),
                                                                br(),
                                                                br(),
                                                                br()
                                                       ),
                                                       tabPanel("Results",
                                                                br(),
                                                                p("Download the predicted result as CSV file. The file consists of 2 (two) columns (PassengerId, Survived)."),
                                                                br(),
                                                                downloadButton("dwnPred", "Predicted"),
                                                                br(),
                                                                br(),
                                                                br()
                                                       )
                                                     )
                                            )
                                )
                         )
                ),
                tabPanel("Twitter Mining",
                         h1("Extract Insight From Twitter"),
                         tabsetPanel(type = "pills",
                                  tabPanel("Trending Topics",
                                           h1("Trending Topic Today"),
                                           column(3,
                                                  selectInput("woecountry", "Country",
                                                       choices = woeid$country),
                                                  selectInput("woearea", "Location/Area",
                                                              choices = woeid$name),
                                                  actionButton("trendBut", "Refresh", icon = icon("hostory"))
                                                  ),
                                           column(9,
                                                  tableOutput("trends"),
                                                  br()
                                           )
                                           ),
                                 tabPanel("wordcloud",
                                        column(3,
                                               br(),
                                                 textInput("searchterm", "Search Keyword"),
                                                 numericInput("ntweets", "Max. Number of tweets", min = 0, max = 1000, value = 500),
                                                 br(),
                                                 br(),
                                                 actionButton("searchtw", "Process", icon = icon("searchengine"))
                                        ),
                                        column(9,
                                                 wordcloud2Output("wordcld")
                                        )
                                  ),
                                  tabPanel("Web Scrapping",
                                           h1("Extract Contents From A Web"),
                                           # column(1, h5("URL: ")),
                                           column(8, textInput("urlscrap", label = NULL, placeholder = "URL: (e.g https://www.microsoft.com/)", width = "100%")),
                                           column(4, actionButton("scrapBut", "Process")),
                                           # br(),
                                           column(12,
                                                  h3("Information"),
                                                  renderWordcloud2("webcloud")
                                           )
                                           )
                         )
                ),
                tabPanel("Stat Learn",
                         h1("Under Construction")
                         ),
                tabPanel("Graphical Learn",
                         h1("Under Construction")
                         ),
                tabPanel("Database Learn",
                         h1("Under Construction")
                         )
    ),
    br(),
    br(),
    br(),
    br(),
    h5("Copyright 2018 Aep Hidayatuloh", style = "text-align: center; font-weight: bold;")
    
    )
)