library(shiny)
#library(shinyjs)
library(shinythemes)
library(tidyverse)
library(readr)
library(bspam)
library(runjags)
library(shinyWidgets)

# check JAGS env
# This is for running JAGS on M2
findjags <- findjags()
if (findjags == "JAGS not found") {
  # need to set path to find JAGS
  runjags::runjags.options(jagspath="/hpc/applications/jags/4.3.0/gcc-6.3.0/bin/jags")
} # end


<<<<<<< HEAD
############################################################################################################
##                                     USER INTERFACE SECTION                                             ##
############################################################################################################

=======
## Define UI for dataset viewer app ----
>>>>>>> 4dd78d5673fba7fa88da7f747701dc7e17dff723
ui <- fluidPage(
  
  #style for status bars...
  tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 800px;
              font-size: 30px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }
           "
      )
    )
  ),
  
  navbarPage("bspam Shiny App", theme = shinytheme("lumen"),
             navbarMenu("Welcome",
       ## ------- This is for Welcome page ------- ####
                        tabPanel("Welcome Page", fluid = TRUE,
                                 fluidRow(
                                   column(12,
                                          h2(p("Welcome to bspam Shiny App!!")),
                                          br(),
                                          h4(p("This is the interactive dashboard for bspam R package. bspam stands for
                                               `binomial log-normal speed-accruacy modeling'. Use of this app does not
                                               require any knowledge of R. All tasks can be completed interactively by
                                               following the direcitons provided under the menus/tabs.")),
                                          h4(p("bspam package has
                                               functions to fit the speed-accuracy psychometric model for count outcome data
                                               (Potgieter, Kamata & Kara, 2017; Kara, Kamata, Potgieter & Nese, 2020),
                                               where the accuracy is modeled by a binomial count latent variable model.
                                               For example, the use of this modeling technique allows model-based calibration
                                               and scoring for oral reading fluency (ORF) assessment data collected from reading passages."),
                                          ),
                                          h4(p("bspam Shiny App has three main tabs: Data Preparation, Model Fitting, and Score
                                               Estimation.")),
                                          h3(p("Data Preparation")),
                                          h4(p("Data preparation allows users to prepare their data for the analyses. For demonstration
                                          purposes, data preparation tab provides access to several datasets available in the bspam
                                               package. Users can uplaod their datasets in various formats including `.rds` (R data serialization) and 
                                               `.csv (comma-separated values)`.
                                               Once the data are loaded to the app, the next step is assigning the relevant columns to required
                                               type of variables for fitting the model. This is done by using the dropdown selection
                                               menus. Users can explore their raw and prepared dataets under the relevant view tabs.
                                               A descriptive summary of the prepared dataset is also provided under the Summary Statistics
                                               tab."
                                          )),
                                          h3(p("Model Fitting")),
                                          h4(p("This page has the options for performing model fitting, namely, estimation of task
                                               paramters. User can select the desired options for the estimation. All non-mandatory options
                                               are pre-selected as the default options as in the relevant bspam function." )),
                                          h3(p("Score Estimation")),
                                          h4(p("This page has the options for performing score estimation, namely, estimation of person
                                          parameters and model-based scores (in the scale of number of successful tasks per minute). 
                                          User can select the desired options for score estimation.
                                          All non-mandatory options are pre-selected as the default options as in the relevant bspam function." )),
                                          br(),
                                          h3(p("References")),
                                          h4(p("Potgieter, C. J., Kamata, A., & Kara, Y. (2017). An EM algorithm for estimating an oral reading speed and accuracy model.", a("https://arxiv.org/abs/1705.10446", href="https://arxiv.org/abs/1705.10446"))),
                                          h4(p("Kara, Y., Kamata, A., Potgieter, C., & Nese, J. F. (2020). Estimating model-based oral reading fluency: A Bayesian approach. Educational and Psychological Measurement, 80(5), 847-869.", a("https://doi.org/10.1177/0013164419900208", href="https://doi.org/10.1177/0013164419900208")))
                                   ))
                                 
                        ),
       ## ------- This is for About page ------- ####
                        tabPanel("About", fluid = TRUE, icon = icon("info-circle"),
                                 fluidRow(
                                   column(12,
                                          h3(p("About the bspam Shiny App & bspam R package")),
                                          h4(p("This Shiny app and the bspam R package have been developed as part of the project entitled 'Developing Computational Tools for Model-Based Oral Reading Fluency Assessments', 
                                               funded by Institute of Education Sciences, U.S. Department of Education through Grant R305D200038 to Southern Methodist University. 
                                               The opinions expressed are those of the authors and do not represent views of the Institute or the U.S. Department of Education. Please click", a("here", href="https://ies.ed.gov/funding/grantsearch/details.asp?ID=3410"), "for detailed information about the funding."
                                               )),
                                          br(),
                                          h3(p("Citation")),
                                          h4(p("bspam Shiny App:")),
                                          h4(p("[add citation here]")),
                                          h4(p("bspam R package:")),
                                          h4(p("[add ciation here]")),
                                          br(),
                                          h3(p("Resources")),
                                          h4(p("For a more detailed information about the bspam package, please see the ",
                                               a("GitHub page ", href = "https://github.com/kamataak/bspam"), "and package ", a("website", href="https://kamataak.github.io/bspam/"), ".")),
                                          h4(p("The source code for the Shiny app is available ", a("on GitHub", href = "https://github.com/wangkuo08/ShinyBspam"), ".")),
                                          br(), 
                                          h3(p("Copyright Statement")),
                                          h4(p("Copyright (C) 2022-2023 The ORF Project Team")),
                                          h4(p("The bspam package is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the 
                                          Free Software Foundation, either version 3 of the License, or any later version.")),
                                          h4(p("The bspam package is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
                                               without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
                                               See the GNU General Public License for more details. You should have received a copy of the GNU General Public 
                                               License along with this package. If not, see ", a("http://www.gnu.org/licenses/.", href="http://www.gnu.org/licenses/"), "."))
                                 )
                        )
             )),
             # end  navbarMenu
       
       ######################################################========DATA PREPARATION TAB============#########################################
       
             tabPanel("Data Preparation", fluid = TRUE, icon = icon("database"),
                      #tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        #sidebarPanel(titlePanel("Please Select Dataset")),
                        
                        sidebarPanel(style = "background: #0033A0", width = 2,
                                     fluidRow(
                                       column(width = 12,
                                              wellPanel(
                                                h4(HTML("<b> Try bspam Datasets </b>")),
                                                # Input: Selector for choosing dataset ----
                                                selectInput(inputId = "dataset",
                                                            label = HTML("Select"),
                                                            choices = c("none","passage2")),
                                                
                                              ),
                                              conditionalPanel(condition = "input.dataset== 'none'",
                                                               wellPanel(
                                                                 h4(HTML("<b> Load Your Dataset </b>")),
                                                                 selectInput(inputId = "datafile",
                                                                             label = HTML("Select your file type"),
                                                                             choices = c("rds | rda | rdata", "csv | tsv")),
                                                                 
                                                                 # fileInput(inputId = "upload", NULL, multiple = FALSE),
                                                                 # try UI
                                                                 uiOutput('resetUploadInput'),
                                                                 actionButton(inputId = "getUploadBtn", label = "Click to load"),)),
                                              
                                              wellPanel( 
                                                h4(HTML("<b> Assign Column Names </b>")),
                                                # person.id = "id.student",
                                                # occasion = "occasion",
                                                # group = "grade",
                                                # task.id = "id.passage",
                                                # max.counts = "numwords.pass",
                                                # obs.counts = "wrc",
                                                # time = "sec"
                                                # h4("Set arguments:"),
                                                
                                                selectInput(inputId = "person.id",
                                                            label = "person.id", choices = NULL),
                                                selectInput(inputId = "occasion",
                                                            label = "occasion", choices = NULL),
                                                selectInput(inputId = "group",
                                                            label = "group", choices = NULL),
                                                selectInput(inputId = "task.id",
                                                            label = "task.id", choices = NULL),
                                                selectInput(inputId = "max.counts",
                                                            label = "max.counts", choices = NULL),
                                                selectInput(inputId = "obs.counts",
                                                            label = "obs.counts", choices = NULL),
                                                selectInput(inputId = "time",
                                                            label = "time", choices = NULL),
                                                
                                                #  hr(style = "border-top: 2px solid #D3D3D3;"),
                                                
                                                # Input: Numeric entry for number of obs to view ----
                                                #      numericInput(inputId = "obs",
                                                #                   label = "Number of observations to view:",
                                                #                   value = 10),
                                              ),
                                              wellPanel( 
                                                h4(HTML("<b> Prepare Your Data </b>")),
                                                actionButton(inputId = "runBtn", label = "Run", icon = icon("gears")),
                                                actionButton(inputId = "resetBtn", label = "Reset", styleclass = "warning")),
                                              wellPanel(
                                                h4(HTML("<b> Save Prepared Data </b>")),
                                                textInput(inputId = "saveas", "Enter file name below"),
                                                downloadButton("downloadData", "Save")),
                                       ))),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          # useShinyjs(),
                          # style = "overflow-y:scroll; max-height: 800px; position:relative; align: centre",
                          
                          # Output: Formatted text for caption ----
                          
                          
                          # Output: Verbatim text for data summary ----
                          
                          
                          # Output: HTML table with requested number of observations ----
                          tabsetPanel(id = "prepareTabset",
                                      tabPanel("View Raw Data", dataTableOutput("raw_data")),
                                      tabPanel("View Prepared Data", dataTableOutput("prep_data")),
                                      tabPanel("Summary Statistics", verbatimTextOutput("summary"))
                                      
                          )
                        )
                        
                      )
             ),
       
       ######################################################========MODEL FITTING TAB============#########################################
            
        tabPanel("Model Fitting", fluid = TRUE, icon = icon("ruler"),
                      sidebarLayout(
                        sidebarPanel(style = "background: #0033A0", width = 2,
                                     fluidRow(
                                       column(width = 12,
                                              wellPanel(
                                                h4(HTML("<b> Did you prepare your data via Data Preparation Tab? </b>")),
                                                br(),
                                                radioButtons(inputId = "fit_dat", label = NULL, inline = F,
                                                             c("Yes, use the stored prepared data." = "yes",
                                                               "No, upload previously prepared data." = "no")),
                                              
                                              conditionalPanel(condition = "input.fit_dat == 'no'",
                                             
                                            
                                                  # hr(style = "border-top: 2px solid #D3D3D3;"),
                                                  # load data
                                                  #fileInput(inputId = "upload.prepared", NULL, multiple = FALSE),
                                                  # try UI
                                                  uiOutput('resettableInput'),
                                                  actionButton(inputId = "getPrepared.Btn", label = "Click to load"),
                                                )),
                                              
                                              wellPanel(
                                                h4(HTML("<b> Choose Estimator </b>")),
                                                radioButtons(inputId = "est", label = NULL, inline = TRUE,
                                                             c("mcem" = "mcem",
                                                               "bayes" = "bayes")),
                                                
                                                #      conditionalPanel( ## DELETE THAT FOR NOW!! USE ONLY DEFAULT DATA FROM PREP STEP!
                                                #         condition = "input.est == 'bayes'",
                                                #  hr(style = "border-top: 2px solid #D3D3D3;"),
                                                #        selectInput(inputId = "fit.model.person.id",
                                                #                   label = "person.id", choices = NULL),
                                                #      selectInput(inputId = "fit.model.task.id",
                                                #                 label = "task.id", choices = NULL),
                                                #    selectInput(inputId = "fit.model.max.counts",
                                                #               label = "max.counts", choices = NULL),
                                                #  selectInput(inputId = "fit.model.obs.counts",
                                                #             label = "obs.counts", choices = NULL),
                                                # selectInput(inputId = "fit.model.time",
                                                #             label = "time", choices = NULL)
                                                # ),
                                                
                                                # hr(style = "border-top: 2px solid #D3D3D3;"),
                                                selectInput("parSet", "Parameters Setting:",
                                                            c("Default" = "1", "[Custom]" ="2")
                                                ),
                                                
                                                conditionalPanel(
                                                  condition = "(input.parSet == '2') && (input.est=='mcem')",
                                                  
                                                  # hr(style = "border-top: 2px solid #D3D3D3;"),
                                                  sliderInput(inputId = "k.in", label = "k.in:", value = c(5), min = 2, max = 10),
                                                  sliderInput(inputId = "rep.in", label = "rep.in:", value = c(2), min = 2, max = 100),
                                                  
                                                  radioButtons(inputId = "se", label = "se:",
                                                               c("none" = "none",
                                                                 "analytic" = "analytic",
                                                                 "bootstrap" = "bootstrap")),
                                                  radioButtons(inputId = "verbose", label = "verbose:", inline = TRUE,
                                                               c("False" = FALSE,
                                                                 "True" = TRUE))
                                                  
                                                ),
                                                actionButton(inputId = "fit.model.Btn", label = "fit.model"),
                                                actionButton(inputId = "resetBtn", label = "Reset", styleclass = "warning")
                                              ),
                                              
                                              # hr(style = "border-top: 2px solid #D3D3D3;"),
                                              wellPanel(
                                                h4(HTML("<b> Save fit.model data </b>")),
                                                textInput(inputId = "save.fit.model.as", "Enter file name below"),
                                                downloadButton("download.fit.model.data", "Save"))
                                              
                                       ))),
                        mainPanel(
                          tabsetPanel(id = "fit.model.Tabset",
                                      #tabPanel("View Raw Data", dataTableOutput("raw_data")),
                                      tabPanel("Upload.Data", dataTableOutput("prep.data")),
                                      tabPanel("fit.model.summary", verbatimTextOutput("fit.model.summary"))
                                      
                          )
                        )
                      ) # end sidebarLayout
             ), # end tabPanel("Model Fitting"
             
       ######################################################========SCORE ESTIMATION TAB============#########################################
       
             tabPanel("Score Estimation", fluid = TRUE, icon = icon("chart-bar"),
                      sidebarLayout(
                        sidebarPanel(style="background: #0033A0", width = 2,
                                     fluidRow(
                                       column(width = 12,
                                              wellPanel(
                                                h4(HTML("<b> Which task calibration data will be used? </b>")),
                                                br(),
                                                radioButtons(inputId = "calibUseData", label = NULL, inline = F, 
                                                             c("Use the stored task parameters from model fitting tab." = '1',
                                                               "Upload previously calibrated task parameters." = '2')),
                                                
                                                
                                                conditionalPanel(condition = "input.calibUseData == '2'", 
                                                                 actionButton(inputId = "calib_load.Btn", label = "Load data")
                                                )),
                                              
                                              wellPanel(
                                                h4(HTML("<b> Which person data will be used? </b>")),
                                                br(),
                                                radioButtons(inputId = "scoreUseData",
                                                            label = NULL,
                                                            inline = F,
                                                           choices = c("Use the stored person data from data preparation/model fitting tab." = "1", 
                                                                       "Upload previously prepared person data." ="2")
                                                ),
                                              
                                                conditionalPanel(
                                                  condition = "input.scoreUseData == '2'",
                                                  uiOutput('score.resettableInput'),
                                                  actionButton(inputId = "score.getPrepared.Btn", label = "Load data"),
                                                )),
                                              
                                              wellPanel(
                                                h4(HTML("<b> Select Scoring Estimator </b>")),
                                                br(),
                                                radioButtons(inputId = "scoreEst", label = NULL, inline = FALSE,
                                                             c("mle" = "mle",
                                                               "map" = "map",
                                                               "eap" = "eap",
                                                               "bayes" = "bayes")),
                                                selectInput("scoreParSet", "Estimator Options",
                                                            c("Default" = "1", "[Custom]" ="2")
                                                ),
                                                conditionalPanel(
                                                  condition = "input.scoreEst == 'bayes'",
                                                  selectInput(inputId = "score.person.id",
                                                              label = "person.id", choices = NULL),
                                                  selectInput(inputId = "score..task.id",
                                                              label = "task.id", choices = NULL),
                                                  selectInput(inputId = "score.max.counts",
                                                              label = "max.counts", choices = NULL),
                                                  selectInput(inputId = "score.obs.counts",
                                                              label = "obs.counts", choices = NULL),
                                                  selectInput(inputId = "score.time",
                                                              label = "time", choices = NULL)
                                                ),
                                                
                                                # Only show this panel if the 
                                                conditionalPanel(
                                                  condition = "input.scoreParSet == '2' & input.scoreEst!='bayes'",
                                                  
                                                  sliderInput(inputId = "score.failsafe", label = "failsafe:", value = c(0), min = 0, max = 50),
                                                  sliderInput(inputId = "score.bootstrap", label = "bootstrp:", value = c(100), min = 50, max = 500),
                                                  
                                                  radioButtons(inputId = "score.se", label = "se:",
                                                               c("analytic" = "analytic",
                                                                 "bootstrap" = "bootstrap"), inline = TRUE)
                                                ),
                                              ),
                                              
                                              
                                              wellPanel(h4(HTML("<b> Other Options </b>")),
                                                        br(),
                                                        radioButtons(inputId = "scoreExtOption", 
                                                                     label = "Perform External Scoring",
                                                                      choices = c("no" = "no",
                                                                                  "yes" = "yes"), inline = TRUE, selected = "no"),
                                                        
                                                        conditionalPanel(condition = "input.scoreExtOption == 'yes'",
                                                                         textInput(inputId = "score.external", 
                                                                                   label = "Scoring based on external option", 
                                                                                   value = "")),
                                                        
                                                        radioButtons(inputId = "score.type", label = "Output Type",
                                                                     c("general" = "general",
                                                                       "orf" = "orf"), inline = TRUE),
                                                        ),
                                     
                                     wellPanel(
                                       h4(HTML("<b> Which cases will be scored? </b>")),
                                       br(),
                                     radioButtons(inputId = "scoreCases", label = NULL,
                                                  c("default" = "default",
                                                    "input" = "input",
                                                    "upload" = "upload"), inline = TRUE, selected = "default"),
                                     conditionalPanel(
                                       condition = "input.scoreCases == 'input'",
                                       textInput(inputId = "input.score.cases", label = "input cases:", value = ""),
                                     ),
                                     conditionalPanel(
                                       condition = "input.scoreCases == 'upload'",
                                       fileInput(inputId = "upload.score.cases", NULL, multiple = FALSE)
                                     )),
                                     
                                     wellPanel(
                                       h4(HTML("<b> Perform Scoring </b>")),
                                       br(),
                                     actionButton(inputId = "score.Btn", label = "Run", icon = icon("gears")),
                                     actionButton(inputId = "resetBtn", label = "Reset", styleclass = "warning")
                        ), 
                        wellPanel(h4(HTML("<b> Save Scoring Data </b>")),
                                  br(),
                                  textInput(inputId = "save.score.as", "Enter file name below"),
                                  downloadButton("download.score.data", "Save"))
                        ))),
                        mainPanel(
                          tabsetPanel(id = "score.Tabset",
                                      #tabPanel("View Raw Data", dataTableOutput("raw_data")),
                                      tabPanel("score.Upload.Data", dataTableOutput("score.prep.data")),
                                      tabPanel("score.summary", verbatimTextOutput("score.summary"))
                                      
                          )
                        )
                      ) # end sidebarLayout
             ), # end tabPanel("Score Estimation"
             
  ))


############################################################################################################
##                                             SERVER SECTION                                             ##
############################################################################################################


server <- function(input, output, session) {
  # Define variables
  saveData <- NULL # prepared data
  uploaded_data <- NULL # uploaded data
  fit.model.result <- NULL # fit.model data
  LoadedPrepared_data <- NULL # Loaded prepared data
  score.result <- NULL # score estimation
  fit.saved <- NULL # saved fit data for scoring
  score.loadedPrepared_data <- NULL # Loaded prepared data for scoring
  score.saved <- NULL # saved scoring data
  
  # docs <- "Guidence for Preparing data..."
  
  #output$caption <- renderText({
  #  docs
  # })
  
  # Using available datasets in bspam
  datasetInput <- reactive({
    switch(input$dataset,
           "none" = NULL,
           "passage2" = passage2)
  })
  
  # get data set columns name
  observeEvent(datasetInput(), {
    updateList(datasetInput())
    # choices_list = colnames(datasetInput())
    # updateSelectInput(inputId = "person.id", choices = choices_list)
    # updateSelectInput(inputId = "occasion", choices = choices_list)
    # updateSelectInput(inputId = "group", choices = choices_list)
    # updateSelectInput(inputId = "task.id", choices = choices_list)
    # updateSelectInput(inputId = "max.counts", choices = choices_list)
    # updateSelectInput(inputId = "obs.counts", choices = choices_list)
    # updateSelectInput(inputId = "time", choices = choices_list)
    
    #   output$summary <- renderPrint({
    #
    #      if (!is.null(datasetInput())) {
    #        output$caption <- renderText({
    #          NULL
    #        })
    #        dataset <- datasetInput()
    #        #browser()
    #        if (class(dataset)[1] == "fit.model") {
    #          dataset %>% summary()
    #        } else {
    #          summary(dataset)
    #       }
    #
    #      } else {
    #          output$caption <- renderText({
    #            docs
    #         })
    #    }
    # })
    output$raw_data <- renderDataTable({
      datasetInput()
    })
    
  })
  
  
  # uploading a dataset for the prepdata tab!
  Upload_data <- reactive({
    req(input$upload.Data)
    ext <- tools::file_ext(input$upload.Data$name)
    if (ext == "csv") {
      df <- read.csv(input$upload.Data$datapath, header=TRUE)
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$upload.Data$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$upload.Data$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$upload.Data$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }
    
    updateList(df)
    
    output$raw_data <- renderDataTable({
      df
    })
    return (df)
  })
  
  # getUpload button
  observeEvent(input$getUploadBtn, {
    uploaded_data <<- Upload_data()
    
    updateList(Upload_data())
    #output$summary <- renderPrint({
    #  summary(Upload_data())
    #})
    output$raw_data <- renderDataTable({
      Upload_data()
    })
    
  })
  
  output$resetUploadInput <- renderUI({
    
    fileInput(inputId = "upload.Data", NULL, multiple = FALSE)
  })
  
  # reset button
  observeEvent(input$resetBtn, {
    
    # reset upload
    output$resetUploadInput <- renderUI({
      
      fileInput(inputId = "upload.Data", NULL, multiple = FALSE)
    })
    
    updateSelectInput(session,inputId = "dataset", selected = "none")
    updateSelectInput(session,inputId = "person.id", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "occasion", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "group", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "task.id", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "max.counts", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "obs.counts", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "time", choices = "", selected = character(0))
    output$raw_data <- renderText({ "" })
    output$prep_data <- renderText({ "" })
    output$summary <- renderText({ "" })
    
    updateTextInput(session, "saveas", value = "")
    # reset variable
    saveData <<- NULL
    uploaded_data <<- NULL
  })
  
  # run button
  observeEvent(input$runBtn, {
    #browser()
    if (!is.null(datasetInput())) {
      data.name <- datasetInput()
    } else {
      if (!is.null(uploaded_data)) {
        data.name <- uploaded_data
      }
    }
    if (exists("data.name")) {
      if (input$person.id == "" |
          input$occasion == "" |
          input$group == "" |
          input$task.id == "" |
          input$max.counts == "" |
          input$obs.counts == "" |
          input$time == "" ) {
        showModal(modalDialog(
          title = "Error",
          "Please set all arguments!",
          easyClose = TRUE
        ))
        return()
      }
      small_data <- prep(data = data.name,
                         person.id = input$person.id,
                         occasion = input$occasion,
                         group = input$group,
                         task.id = input$task.id,
                         max.counts = input$max.counts,
                         obs.counts = input$obs.counts,
                         time = input$time)
      
      saveData <<- small_data
      
      output$summary <- renderPrint({
        summary(small_data$data.long %>% select(-person.id, -task.id))
      })
      output$prep_data <- renderDataTable({
        small_data$data.long
      })
      
      # update tabletpanel
      updateTabsetPanel(session, "prepareTabset", selected = "View Prepared Data")
      
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please select dataset or upload a datafile!",
        easyClose = TRUE
      ))
      #   runjs('
      #   document.getElementById("caption").scrollIntoView();
      # ')
    }
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$saveas, ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(saveData, file)
    }
  )
  
  updateList <- function(df) {
    choices_list = colnames(df)
    updateSelectInput(inputId = "person.id", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "occasion", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "group", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "task.id", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "max.counts", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "obs.counts", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "time", choices = choices_list, selected = character(0))
    
  }
  
  
  ######=================== action for model fitting ==================
  
  # load data
  load_preparedData <- reactive({
    req(input$upload.prepared)
    ext <- tools::file_ext(input$upload.prepared$name)
    if (ext == "csv") {
      df <- read.csv(input$upload.prepared$datapath, header=TRUE)
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$upload.prepared$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$upload.prepared$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$upload.prepared$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }
    
    #reset summary area
    output$fit.model.summary <- renderText({ "" })
    
    # output$raw_data <- renderDataTable({
    #   df
    # })
    # return (df[[1]]) for test
    # if (input$est == "mcem") {
    #   output$prep.data <- renderDataTable({
    #     df[[1]]
    #   })
    # } else { # bayes
    #   output$prep.data <- renderDataTable({
    #     df
    #   })
    # }
    
    
    updateTabsetPanel(session, "fit.model.Tabset", selected = "Upload.Data")
    return (df)
    
  })
  
  # load prepared button
  observeEvent(input$getPrepared.Btn, {
    LoadedPrepared_data <<- load_preparedData()
    
    # output$prep.data <- renderDataTable({
    #   load_preparedData()[[1]]
    # })
    if (input$est == "mcem") {
      output$prep.data <- renderDataTable({
        load_preparedData()[[1]]
      })
    } else { # bayes
      output$prep.data <- renderDataTable({
        load_preparedData()
      })
      # get columns list
      fit.model.updateList(load_preparedData())
    }
    
  })
  
  #update list
  fit.model.updateList <- function(df) {
    choices_list = colnames(df)
    updateSelectInput(inputId = "fit.model.person.id", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "fit.model.task.id", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "fit.model.max.counts", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "fit.model.obs.counts", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "fit.model.time", choices = choices_list, selected = character(0))
  }
  
  # fit.model button
  observeEvent(input$fit.model.Btn, {
    #browser()
    # get data
    target.data <- NULL
    # fit.model.result <- NULL # test
    
    # Validate input
    # will do later
    
    # validate(
    #   need(input$in1, 'Check at least one letter!'),
    #   need(input$in2 != '', 'Please choose a state.')
    # )
    
    if (input$fit_dat == "yes") { # Default to use prepared data
      target.data <- saveData
    } else { # use uploaded prepared data
      # showModal(modalDialog( # for debug
      #   title = "good",
      #   "here",
      #   easyClose = TRUE
      # ))
      target.data <- LoadedPrepared_data
    }
    
    if (length(target.data) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please prepared your data first!",
        easyClose = TRUE
      ))
      return()
    } else {
      
      # update tabletpanel
      updateTabsetPanel(session, "fit.model.Tabset", selected = "fit.model.summary")
      
      
      output$fit.model.summary <- renderText({ "" })
      
      fit.model.result <- NULL
      
      # #Create 0-row data frame which will be used to store data
      dat <- data.frame(x = numeric(0), y = numeric(0))
      
      withProgress(message = 'Running fit.model...', value = 0, {
        
        #   # Number of times we'll go through the loop
        n <- 10
        #
        for (i in 1:n) {
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = "Please wait...")
          
          if (i == 5) {
            if (length(fit.model.result) == 0) {
              if (input$est == "mcem") { # mcem
                # showModal(modalDialog( # for debug
                #   title = "good",
                #   "mcem",
                #   easyClose = TRUE
                # ))
                fit.model.result <- fit.model(data=target.data$data.wide,
                                              person.data = target.data$data.long, #DO WE NEED THE PERSON DATA FOR CALIBRATION?
                                              est = input$est,
                                              verbose=input$verbose,
                                              se=input$se)
                
                
              } else {
                # showModal(modalDialog( # for debug
                #   title = "good",
                #  "bayes",
                #   easyClose = TRUE
                # ))
                #test
                fit.model.result <- fit.model(#data=target.data$data.wide,
                  person.data=target.data$data.long, #Corrected this part for bayes estimator..
                  person.id = "person.id",
                  task.id = "task.id",
                  max.counts = "max.counts",
                  obs.counts = "obs.counts",
                  time = "time",
                  est = "bayes")
              }
              
              fit.saved <<- fit.model.result
            } else {
              break
            }
          }
          
        }
      })
      
      output$fit.model.summary <- renderPrint({
        fit.model.result %>% summary()
      })
      
    }
  }) # end  observeEvent(input$fit.model.Btn
  
  output$score.resettableInput <- renderUI({
    
    fileInput(inputId = "upload.prepared", NULL, multiple = FALSE)
  })
  
  observe({ #reset conditions
    sel_set <- input$parSet
    use_set <- input$fit_dat
    # showModal(modalDialog(
    #   title = "radio",
    #   sel_set,
    #   easyClose = TRUE
    # ))
    if (sel_set == "1") { # Default parameters
      # reset parameters
      updateSliderInput(session, "k.in", value = c(5))
      updateSliderInput(session, "rep.in", value = c(2))
      updateRadioButtons(session, "est", selected = input$est) #input$est
      updateRadioButtons(session, "se", selected = "none")
      updateRadioButtons(session, "verbose", selected = FALSE)
      fit.model.result <<- NULL
      updateTextInput(session, "save.fit.model.as", value = "")
    }
    if (use_set == "yes") { # Default data
      # will reset all input
      # reset upload
      LoadedPrepared_data <- NULL
      output$resettableInput <- renderUI({
        
        fileInput(inputId = "upload.prepared", NULL, multiple = FALSE)
      })
      # reset output area
      output$prep.data <- renderDataTable({
        NULL
      })
      output$fit.model.summary <- renderText({ "" })
    }
    
  })
  
  # save fit.model.data
  output$download.fit.model.data <- downloadHandler(
    filename = function() {
      paste(input$save.fit.model.as, ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(fit.saved, file)
    }
  )
  
  
  ######=================== action for score estimating ==================
  output$score.resettableInput <- renderUI({
    
    fileInput(inputId = "score.upload.prepared", NULL, multiple = FALSE)
  })
  
  # load person-level formerly prepared data 
  score.load_preparedData <- reactive({
    req(input$score.upload.prepared)
    ext <- tools::file_ext(input$score.upload.prepared$name)
    if (ext == "csv") {
      df <- read.csv(input$score.upload.prepared$datapath, header=TRUE)
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$score.upload.prepared$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$score.upload.prepared$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$score.upload.prepared$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }
    
    #reset summary area
    output$fit.model.summary <- renderText({ "" })
    
    updateTabsetPanel(session, "score.Tabset", selected = "score.prep.data")
    return (df)
    
  })
  
  # load data button
  observeEvent(input$score.getPrepared.Btn, {
    score.loadedPrepared_data <<- score.load_preparedData()
    
    output$score.prep.data <- renderDataTable({
      score.load_preparedData()[[1]]
    })
    # get columns list
  })  #end observe
  
  # scoring button
  observeEvent(input$score.Btn, {
    #browser()
    # get data
    calib.data <- NULL
    person.data <- NULL
    
    # showModal(modalDialog(
    #   title = "Default",
    #   print(input$scoreUseData),
    #   easyClose = TRUE
    # ))
    
    if (input$scoreUseData == "1" & input$calibUseData=="1") { # Default to use prepared and fit.model data
      calib.data <- fit.saved
      person.data <- saveData[[1]] }
    else if (input$scoreUseData == "1" & input$calibUseData=="2"){
      calib.data <- fit.saved
      person.data <- LoadedPrepared_data[[1]]
    }
      

## ----------------------    
   
    if (length(person.data) == 0) {
      showModal(modalDialog(
        title = "Error-scoring",
        "Please prepared your data first!",
        easyClose = TRUE
      ))
      return()
    } else {
      
      # update tabletpanel
      updateTabsetPanel(session, "score.Tabset", selected = "score.summary")
      
      output$score.summary <- renderText({ "" })
      
      score.result <- NULL
      
      # #Create 0-row data frame which will be used to store data
      dat <- data.frame(x = numeric(0), y = numeric(0))
      
      withProgress(message = 'Running score estimating...', value = 0, {
        
        #   # Number of times we'll go through the loop
        n <- 10
        #
        for (i in 1:n) {
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = "Please wait...")
          
          if (i == 5) {
            if (length(score.result) == 0) {
              if (input$scoreEst == "bayes") { # bayes
                # showModal(modalDialog( # for debug
                #   title = "good",
                #   "mcem",
                #   easyClose = TRUE
                # ))
                score.result <- scoring(calib.data=calib.data,
                                        person.data = person.data, # person.data$data.long,
                                        est = input$scoreEst,
                                        failsafe = input$score.failsafe,
                                        bootstrap = input$score.bootstrap,
                                        se=input$score.se,
                                        type = input$score.type
                                        
                )
                
              } else { # the others
                showModal(modalDialog( # for debug
                  title = "good",
                  input$scoreEst,
                  easyClose = TRUE
                ))
                #test
                score.result <- scoring(calib.data=calib.data,
                                        person.data = person.data, # person.data$data.long,
                                        est = input$scoreEst,
                                        failsafe = input$score.failsafe,
                                        bootstrap = input$score.bootstrap,
                                        se=input$score.se,
                                        type = input$score.type
                                        
                )
              }
              score.saved <<- score.result
            } else {
              break
            }
          }
          
        }
      })
      
      output$score.summary <- renderPrint({
        score.result %>% summary()
      })
      
    }
  }) # end observeEvent(input$score.Btn
  
  # save scoring result
  output$download.score.data <- downloadHandler(
    filename = function() {
      paste(input$save.score.as, ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(score.saved, file)
    }
  )
  
}
shinyApp(ui = ui, server = server)


