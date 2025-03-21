
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(readr)
library(bspam)
library(plotly)
library(DT)
library(bslib)
library(shinyWidgets)

############################################################################################################
##                                  USER INTERFACE SECTION                                             
############################################################################################################

ui <- fluidPage(
  useShinyjs(), # Set up shinyjs
  tags$style(".shiny-notification-close {display: none}"), # not to show close button for process
  
  #style for status bars...
  tags$head(
    tags$style(
      HTML("/* App window sizing*/
             .body{
             min-width: 1200px;
             margin: auto;
             overflow: auto;
             }
            .shiny-notification {
              height: 100px;
              width: 800px;
              font-size: 30px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }
            # .tabbable > .nav > li > a                  {color:black}
            # .tabbable > .nav > li[class=active]    > a {color:purple}
            a[data-value='Testlet ON'] {
              color: red  !important; 
              pointer-events: none;
              cursor: default;
            }
           "
      )
    )
  ),
  # 027bff
  navbarPage("bspam Shiny App", id = "bspam", theme = shinytheme("lumen"),

             navbarMenu("Welcome",
       ## ------- This is for Welcome page ------- ####
                        tabPanel("Welcome Page", fluid = TRUE,
                                 fluidRow(
                                   column(12,
                                          h2(p("Welcome to bspam Shiny App!!")),
                                          br(),
                                          h4(p("This is the interactive dashboard for bspam R package. bspam stands for
                                               `binomial log-normal speed-accuracy modeling.' Use of this app does not
                                               require any knowledge of R. All tasks can be completed interactively by
                                               following the directions provided under the menus/tabs.")),
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
                                               package. Users can upload their datasets in various formats, including `rds` (R data serialization) and 
                                               `csv (comma-separated values)`.
                                               Once the data are loaded to the app, the next step is assigning the relevant columns to required
                                               type of variables for fitting the model. This is done by using the dropdown selection
                                               menus. Users can explore their raw and prepared dataets under the relevant view tabs.
                                               A descriptive summary of the prepared dataset is also provided under the Summary Statistics
                                               tab."
                                          )),
                                          h3(p("Model Fitting")),
                                          h4(p("This page has the options for performing model fitting, namely, estimation of task
                                               parameters. Users can select the desired options for the estimation. All non-mandatory options
                                               are pre-selected as the default options as in the relevant bspam function." )),
                                          h3(p("Score Estimation")),
                                          h4(p("This page has the options for performing score estimation, namely, estimation of person
                                          parameters and model-based scores (in the scale of number of successful tasks per minute). 
                                          Users can select the desired options for score estimation.
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
             navbarMenu("Start here",
                        nav_item(h4("Toggle the switch to ON to active the testlet model options")),
                        nav_item(switchInput(inputId = "TestletFlag",value = 0)), # default is task level
                        
             ),
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
                                                conditionalPanel(condition = "input.TestletFlag == 1",
                                                  selectInput(inputId = "dataset",
                                                              label = HTML("Select"),
                                                              choices = c("none","passage2")),
                                                ),
                                                conditionalPanel(condition = "input.TestletFlag == 0",
                                                                 selectInput(inputId = "dataset",
                                                                             label = HTML("Select"),
                                                                             choices = c("none","sentence2")),
                                                ),
                                                
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

                                                h5(HTML("* required")),
                                                
                                                selectInput(inputId = "person.id",
                                                            label = "person.id*", choices = NULL),
                                                selectInput(inputId = "occasion",
                                                            label = "occasion (optional)", choices = NULL),
                                                selectInput(inputId = "group",
                                                            label = "group (optional)", choices = NULL),
                                                selectInput(inputId = "task.id",
                                                            label = "task.id*", choices = NULL),
                                                selectInput(inputId = "max.counts",
                                                            label = "max.counts*", choices = NULL),
                                                selectInput(inputId = "obs.counts",
                                                            label = "obs.counts*", choices = NULL),
                                                selectInput(inputId = "time",
                                                            label = "time*", choices = NULL),
                                                
                                                #  hr(style = "border-top: 2px solid #D3D3D3;"),
                                                
                                                # Input: Numeric entry for number of obs to view ----
                                                #      numericInput(inputId = "obs",
                                                #                   label = "Number of observations to view:",
                                                #                   value = 10),
                                              ),
                                              wellPanel( 
                                                h4(HTML("<b> Prepare Your Data </b>")),
                                                actionButton(inputId = "runBtn", label = "Run", icon = icon("cogs")), #gears
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
                                      tabPanel("View Raw Data", DT::DTOutput("raw_data")),
                                      tabPanel("View Prepared Data", DT::DTOutput("prep_data")),
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
                                                             # c("mcem" = "mcem",
                                                             #   "bayes" = "bayes")),
                                                              c("mcem" = "mcem")),
                                                
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
                                                actionButton(inputId = "fit.model.Btn", label = "RUN MODEL FITTING"),
                                                actionButton(inputId = "fit.resetBtn", label = "Reset", styleclass = "warning")
                                              ),
                                              
                                              # hr(style = "border-top: 2px solid #D3D3D3;"),
                                              wellPanel(
                                                h4(HTML("<b> Save Model Fitting (Calibration) Data </b>")),
                                                textInput(inputId = "save.fit.model.as", "Enter file name below"),
                                                downloadButton("download.fit.model.data", "Save"))
                                              
                                       ))),
                        mainPanel(
                          tabsetPanel(id = "fit.model.Tabset",

                                      tabPanel("View Uploaded Data", DT::DTOutput("prep.data")),
                                      tabPanel("View Model Fitting Summary", verbatimTextOutput("fit.model.summary"))
                                      
                          )
                        )
                      ) # end sidebarLayout
             ), # end tabPanel("Model Fitting"
     
       ######################################################========Visualization TAB============#########################################
       
       tabPanel("Visualization", fluid = TRUE, icon = icon("chart-simple"),
                sidebarLayout(
                  sidebarPanel(style="background: #0033A0", width = 2,
                               fluidRow(
                                 column(width = 12,
                                        wellPanel(
                                          h4(HTML("<b> Which task calibration data will be used? </b>")),
                                          br(),
                                          radioButtons(inputId = "VcalibUseData", label = NULL, inline = F, 
                                                       c("Use the stored task parameters from the prior process." = "1",
                                                         "Upload previously calibrated task parameters." = "2")),
                                          conditionalPanel(condition = "input.VcalibUseData == '2'", 
                                                           uiOutput('Vcalib.resettableInput'),
                                                           actionButton(inputId = "Vcalib_load.Btn", label = "Load Calibration data"),
                                                           
                                          ),
                                          br(),
                                          # For plot information
                                          # h6(HTML("<b> Please set theta and tau for information plot: </b>")),
                                          # sliderInput(inputId = "theta.information", label = "theta:", value = c(5), min = -3, max = +3),
                                          # sliderInput(inputId = "tau.information", label = "tau:", value = c(2), min = -1, max = +1),
                                          actionButton(inputId = "plot.information.Btn", label = "Plot.Information", icon = icon("chart-line")),
                                          br(),
                                          h6(HTML("<b> Only two parameters can be visualized at a time! </b>")),
                                          checkboxGroupInput("plotTaskParameter", "Select parameter:",
                                                             c("a" = "a",
                                                               "b" = "b",
                                                               "alpha" = "alpha",
                                                               "beta" = "beta")),
                                          HTML("Sort condition:"),
                                          checkboxInput(inputId = "plotTaskSort", label = "Sort", value = FALSE),
                                          textInput(inputId = "plotTaskid", label = "Input task id:", value = ""),
                                          br(),
                                          actionButton(inputId = "plot.task.Btn", label = "Plot.Task", icon = icon("chart-line")),
                                          ),
                                        wellPanel(id = "plot2", 
                                                        
                                          h4(HTML("<b> Which person data will be used? </b>")),
                                          br(),
                                          radioButtons(inputId = "VscoreUseData",
                                                       label = NULL,
                                                       inline = F,
                                                       choices = c("Use the stored scoring data from the prior process." = "1", 
                                                                   "Upload previously prepared scoring data." ="2")),
                                          
                                          conditionalPanel(
                                            condition = "input.VscoreUseData == '2'",
                                            uiOutput('Vscore.resettableInput'),
                                            actionButton(inputId = "Vscore.Data.Btn", label = "Load Scoring data"),
                                          ),
                                          br(),
                                          checkboxGroupInput("plotPersonParameter", "Select parameter:",
                                                             c("theta" = "theta",
                                                               "tau" = "tau",
                                                               "wcpm" = "wcpm")),
                                          HTML("Show and Sort condition:"),
                                          checkboxInput(inputId = "plotPersonShowSE", label = "Show SE", value = TRUE),
                                          checkboxInput(inputId = "plotPersonSort", label = "Sort", value = FALSE),
                                          textInput(inputId = "plotPersonid", label = "Input person id:", value = ""),
                                          actionButton(inputId = "plot.person.Btn", label = "Plot.Person", icon = icon("chart-line")),
                                          ), 
                                 )
                               )
                  ), 
                  mainPanel(
                    tabsetPanel(id = "visual.Tabset",
                                tabPanel("Plot.Information", plotlyOutput("visual.information")),
                                tabPanel("Plot.Task", plotlyOutput("visual.task")),
                                tabPanel("Plot.Person", plotlyOutput("visual.person")),
                    )
                  )
                ) # end sidebarLayout
       ), # end tabPanel("Visulization"
  ))

############################################################################################################
##                                             SERVER SECTION                                             ##
############################################################################################################

server <- function(input, output, session) {
  
  # Define session-global variables
  values <- reactiveValues(testlet.fit.model.result = NULL, # testlet result
                           testlet.loaded_data = NULL,
                           censor.loaded_data = NULL,
                           scoring.passage.result = NULL, # scoring with testlet
                           saveData = NULL, # prepared data
                           fit.saved = NULL, # saved fit data for scoring
                           uploaded_data = NULL, # uploaded data
                           fit.model.result = NULL, # fit.model data
                           LoadedPrepared_data = NULL, # Loaded prepared data
                           score.result = NULL, # score estimation
                           score.saved = NULL, # saved scoring data
                           score.calib.loaded_data = NULL,
                           score.loaded.external.Data = NULL, # loaded external
                           Vcalib.loaded.Data = NULL, # Loaded Visual calib data
                           Vscore.loaded.Data = NULL # Loaded Visual score data
                           )
  
  #**********************************************************************************
  #*
  # (maybe need to redefine all following variables to the above session-global one)
  # Define variables
  #**********************************************************************************
  # saveData <- NULL # prepared data
  # uploaded_data <- NULL # uploaded data
  # fit.model.result <- NULL # fit.model data
  # LoadedPrepared_data <- NULL # Loaded prepared data
  # score.result <- NULL # score estimation
  # fit.saved <- NULL # saved fit data for scoring
  # score.loadedPrepared_data <- NULL # Loaded prepared data for scoring
  
  # score.calib.loaded_data <- NULL
  
  # score.saved <- NULL # saved scoring data
  # score.loaded.external.Data <- NULL # loaded external
  
  # Vcalib.loaded.Data <- NULL # Loaded Visual calib data
  # Vscore.loaded.Data <- NULL # Loaded Visual score data
  
  # testlet.loaded_data <- NULL
  # censor.loaded_data <- NULL 
  # testlet.fit.model.result <- NULL
  
  # docs <- "Guidence for Preparing data..."
  
  #output$caption <- renderText({
  #  docs
  # })
  
  # First Hide tabs
  # hideTab(inputId = "bspam", target = "Data Preparation")
  # hideTab(inputId = "bspam", target = "Model Fitting")
  # hideTab(inputId = "bspam", target = "Score Estimation")
  # hideTab(inputId = "bspam", target = "Testlet")
  
  observeEvent(input$TestletFlag, {
    if (input$TestletFlag == TRUE) { # sub task-level
      
      # hide plot_person_panel
      shinyjs::hide("plot2")
      # shinyjs::disable("plot.information.Btn")
      shinyjs::hide("plot.information.Btn")
      hideTab(inputId = "visual.Tabset", target = "Plot.Person")
      hideTab(inputId = "visual.Tabset", target = "Plot.Information")
      
      removeTab(inputId = "bspam", target = "Data Preparation") #
      removeTab(inputId = "bspam", target = "Model Fitting") #
      removeTab(inputId = "bspam", target = "Score Estimation") #
      removeTab(inputId = "bspam", target = "Testlet.scoring")#"Score Estimation (Testlet)") #
      removeTab(inputId = "bspam", target = "Testlet.fit.model") #Testlet, Score Estimation (Testlet)
      
      ######################################################========TESTLET DATA PREPARATION TAB============#########################################
      
      insertTab(inputId = "bspam", 
                tabPanel("Data Preparation", fluid = TRUE, icon = icon("database"),
                         #tags$style(button_color_css),
                         # Sidebar layout with a input and output definitions
                         sidebarLayout(
                           #sidebarPanel(titlePanel("Please Select Dataset")),
                           
                           sidebarPanel(style = "background: #0033A0", width = 3,
                                        fluidRow(
                                          column(width = 12,
                                                 wellPanel(
                                                   h4(HTML("<b> Try bspam Datasets </b>")),
                                                   # Input: Selector for choosing dataset with Testlet ----
                                                    selectInput(inputId = "dataset",
                                                                label = HTML("Select"),
                                                                choices = c("none","sentence.level.data","sentence.cens.high","sentence.cens.low")),

                                                   
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
                                                   h5(HTML("* required")),
                                                   
                                                   selectInput(inputId = "person.id",
                                                               label = "person.id*", choices = NULL),
                                                   selectInput(inputId = "occasion",
                                                               label = "occasion (optional)", choices = NULL),
                                                   selectInput(inputId = "group",
                                                               label = "group (optional)", choices = NULL),
                                                   selectInput(inputId = "task.id",
                                                               label = "task.id*", choices = NULL),
                                                   selectInput(inputId = "sub.task.id",
                                                               label = "sub.task.id*", choices = NULL),
                                                   selectInput(inputId = "max.counts",
                                                               label = "max.counts*", choices = NULL),
                                                   selectInput(inputId = "obs.counts",
                                                               label = "obs.counts*", choices = NULL),
                                                   selectInput(inputId = "time",
                                                               label = "time*", choices = NULL),
                                                   h6(HTML("<b> Do your task data involve censoring? </b>")),
                                                   # checkboxInput(inputId = "useCens", label = "Use Censoring", value = FALSE),
                                                   radioButtons(inputId = "radioCens",label="", inline = FALSE,
                                                                c("No" = "1",
                                                                  "Yes" = "2")),
                                                   ##=== Check if using censoring
                                                   conditionalPanel(
                                                     condition = "input.radioCens == '2'",
                                                     h6(HTML("<b> Please specify your censoring column: </b>")),
                                                     selectInput(inputId = "testlet.cens",
                                                                 label = "cens*", choices = NULL),
                                                   )
                                                   #  hr(style = "border-top: 2px solid #D3D3D3;"),
                                                   
                                                   # Input: Numeric entry for number of obs to view ----
                                                   #      numericInput(inputId = "obs",
                                                   #                   label = "Number of observations to view:",
                                                   #                   value = 10),
                                                 ),
                                                 wellPanel( 
                                                   h4(HTML("<b> Prepare Your Data </b>")),
                                                   actionButton(inputId = "runBtn", label = "Run", icon = icon("cogs")), #gears
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
                                         tabPanel("View Raw Data", DT::DTOutput("raw_data")),
                                         tabPanel("View Prepared Data", DT::DTOutput("prep_data")),
                                         tabPanel("Summary Statistics", verbatimTextOutput("summary"))
                                         
                             )
                           )
                           
                         )
                ), # end tabPanel("Data Preparation"
                
                target = "Start here"
      )
      ######################################################========Testlet Model Fitting TAB============#########################################
      
      insertTab(inputId = "bspam", 
                # tabPanel("Testlet", fluid = TRUE, icon = icon("ruler"), id = "Testlet",
                tabPanel("Model Fitting", fluid = TRUE, icon = icon("ruler"), id = "Testlet.fit.model",
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
                                                   h4(HTML("<b> Run Model Fitting </b>")),
                                                   actionButton(inputId = "fit.model.Btn", label = "RUN MODEL FITTING"),
                                                   actionButton(inputId = "fit.resetBtn", label = "Reset", styleclass = "warning"),
                                                   # actionButton(inputId = "runTestletBtn", label = "Model Fitting", icon = icon("cogs")), #gears
                                                   # actionButton(inputId = "resetTestletBtn", label = "Reset", styleclass = "warning"),
                                                   br(),
                                                   # h4(HTML("<b> Save Model Fitting (Calibration) Data</b>")),
                                                   # textInput(inputId = "saveTestletModel", "Enter file name below"),
                                                   # downloadButton("downloadTMData", "Save")
                                                   h4(HTML("<b> Save Model Fitting (Calibration) Data </b>")),
                                                   textInput(inputId = "save.fit.model.as", "Enter file name below"),
                                                   downloadButton("download.fit.model.data", "Save")
                                                   )#,
                                                 
                                          ))), # end sidepanel
                           mainPanel(

                             tabsetPanel(id = "fit.model.Tabset",

                                         tabPanel("View Uploaded Data", DT::DTOutput("prep.data")),
                                         tabPanel("View Model Fitting Summary", verbatimTextOutput("fit.model.summary"))
                                         
                             )
                           )
                           
                         ) # end sidebarLayout
                ), # end tabPanel("Testlet"
                
                target = "Data Preparation"
      )
      ######################################################======== Testlet Scoring TAB============#########################################
      
      # insert tab - Score Estimation
      insertTab(inputId = "bspam", 
                tabPanel("Score Estimation", fluid = TRUE, icon = icon("chart-bar"), id = "Testlet.scoring",
                         sidebarLayout(
                           sidebarPanel(style="background: #0033A0", width = 2,
                                        fluidRow(
                                          column(width = 12,
                                                 wellPanel(
                                                   h4(HTML("<b> Which task calibration data will be used? </b>")),
                                                   br(),
                                                   radioButtons(inputId = "calibUseData", label = NULL, inline = F, 
                                                                c("Use the stored task parameters from model fitting tab." = "1",
                                                                  "Upload previously calibrated task parameters." = "2")),
                                                   
                                                   
                                                   conditionalPanel(condition = "input.calibUseData == '2'", 
                                                                    uiOutput('score.calib.resettableInput'),
                                                                    actionButton(inputId = "calib_load.Btn", label = "Load Calibration data"),
                                                                    
                                                   )),
                                                 
                                                 wellPanel(
                                                   h4(HTML("<b> Which person data will be used? </b>")),
                                                   br(),
                                                   radioButtons(inputId = "scoreUseData",
                                                                label = NULL,
                                                                inline = F,
                                                                choices = c("Use the stored person data from model fitting tab." = "1", 
                                                                            "Upload person data." ="2")
                                                   ),
                                                   
                                                   
                                                   conditionalPanel(
                                                     condition = "input.scoreUseData == '2'",
                                                     uiOutput('score.person.resettableInput'),
                                                     actionButton(inputId = "score.getPrepared.Btn", label = "Load Person data"),
                                                   ),
                                                   h6(HTML("<b> Do your task data involve censoring? </b>")),
                                                   # checkboxInput(inputId = "useCens", label = "Use Censoring", value = FALSE),
                                                   radioButtons(inputId = "radioCensTestlet",label="", inline = FALSE,
                                                                c("No" = "1",
                                                                  "Yes" = "2")),
                                                   ##=== Check if using censoring
                                                   # conditionalPanel(
                                                   #   condition = "input.radioCensP == '2'",
                                                   #   h6(HTML("<b> Please specify your censoring column: </b>")),
                                                   #   selectInput(inputId = "scoring.testlet.cens",
                                                   #               label = "cens*", choices = NULL),
                                                   # )
                                                   ),

                                                 wellPanel(
                                                   h4(HTML("<b> Run Scoring </b>")),
                                                   actionButton(inputId = "score.Btn", label = "Scoring", icon = icon("cogs")), #gears
                                                   actionButton(inputId = "score.resetBtn", label = "Reset", styleclass = "warning"),
                                                   br(),
                                                   
                                                   # h4(HTML("<b> Save Scoring </b>")),
                                                   # radioButtons(inputId = "save.SP.type", label = "File Type",
                                                   #              c("csv file" = "csv",
                                                   #                "rds file" = "rds"), inline = TRUE),
                                                   # textInput(inputId = "saveScoringPassage", "Enter file name below"),
                                                   # downloadButton("downloadSPData", "Save")
                                                   
                                                 ),# end of wellPanel
                                                 wellPanel(h4(HTML("<b> Save Scoring Data </b>")),
                                                           br(),
                                                           radioButtons(inputId = "save.score.type", label = "File Type",
                                                                        c("csv file" = "csv",
                                                                          "rds file" = "rds"), inline = TRUE),
                                                           textInput(inputId = "save.score.as", "Enter file name below"),
                                                           downloadButton("download.score.data", "Save"))
                                                 
                                          ))),
                           mainPanel(
                             tabsetPanel(id = "score.Tabset",

                                         tabPanel("View Calibration Data", verbatimTextOutput("calib.data")),
                                         tabPanel("View Uploaded Person Data", DT::DTOutput("score.prep.data")),
                                         # try dataTable
                                         tabPanel("View Scoring Summary", DT::DTOutput("score.summary"))
                                         # tabPanel("View Scoring Summary", verbatimTextOutput("score.summary"))                                     
                             )
                           )
                         ) # end sidebarLayout
                ), # end tabPanel("Score Estimation"
                
                target = "Model Fitting"
      ) # end of insert tab - Score Estimation
      insertTab(inputId = "bspam", 
                tabPanel("Testlet ON", fluid = TRUE), target = "Visualization"
      )
      
      
    } else { # passage-level
      # show plot_person_panel

      showTab(inputId = "visual.Tabset", target = "Plot.Person")
      shinyjs::show("plot2")     
      shinyjs::show("plot.information.Btn")
      showTab(inputId = "visual.Tabset", target = "Plot.Information")
      
      # remove  
      removeTab(inputId = "bspam", target = "Data Preparation") #
      removeTab(inputId = "bspam", target = "Model Fitting") #
      removeTab(inputId = "bspam", target = "Score Estimation") #
      # removeTab(inputId = "bspam", target = "Score Estimation (Testlet)") #
      removeTab(inputId = "bspam", target = "Testlet.fit.model") #
      removeTab(inputId = "bspam", target = "Testlet.scoring") #
      removeTab(inputId = "bspam", target = "Testlet ON") #
      # removeTab(inputId = "bspam", target = "Testlet") #Testlet
      # insert tab - Data preperation
      
      ######################################################========DATA PREPARATION TAB============#########################################
      
      insertTab(inputId = "bspam", 
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
                                                   h5(HTML("* required")),
                                                   
                                                   selectInput(inputId = "person.id",
                                                               label = "person.id*", choices = NULL),
                                                   selectInput(inputId = "occasion",
                                                               label = "occasion (optional)", choices = NULL),
                                                   selectInput(inputId = "group",
                                                               label = "group (optional)", choices = NULL),
                                                   selectInput(inputId = "task.id",
                                                               label = "task.id*", choices = NULL),
                                                   selectInput(inputId = "max.counts",
                                                               label = "max.counts*", choices = NULL),
                                                   selectInput(inputId = "obs.counts",
                                                               label = "obs.counts*", choices = NULL),
                                                   selectInput(inputId = "time",
                                                               label = "time*", choices = NULL),
                                                   h6(HTML("<b> Do your task data involve censoring? </b>")),
                                                   # checkboxInput(inputId = "useCens", label = "Use Censoring", value = FALSE),
                                                   radioButtons(inputId = "radioCens",label="", inline = FALSE,
                                                                c("No" = "1",
                                                                  "Yes" = "2")),
                                                   ##=== Check if using censoring
                                                   conditionalPanel(
                                                     condition = "input.radioCens == '2'",
                                                     h6(HTML("<b> Please specify your censoring column: </b>")),
                                                     selectInput(inputId = "testlet.cens",
                                                                 label = "cens*", choices = NULL),
                                                   )
                                                   #  hr(style = "border-top: 2px solid #D3D3D3;"),
                                                   
                                                   # Input: Numeric entry for number of obs to view ----
                                                   #      numericInput(inputId = "obs",
                                                   #                   label = "Number of observations to view:",
                                                   #                   value = 10),
                                                 ),
                                                 wellPanel( 
                                                   h4(HTML("<b> Prepare Your Data </b>")),
                                                   actionButton(inputId = "runBtn", label = "Run", icon = icon("cogs")), #gears
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
                                         tabPanel("View Raw Data", DT::DTOutput("raw_data")),
                                         tabPanel("View Prepared Data", DT::DTOutput("prep_data")),
                                         tabPanel("Summary Statistics", verbatimTextOutput("summary"))
                                         
                             )
                           )
                           
                         )
                ), # end tabPanel("Data Preparation"
                
                target = "Start here"
      )
      ######################################################========MODEL FITTING TAB============#########################################
      
      # insert tab - Model Fitting
      insertTab(inputId = "bspam", 
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
                                                                # c("mcem" = "mcem",
                                                                #   "bayes" = "bayes")),
                                                                c("mcem" = "mcem")),
                                                   
                                                   
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
                                                   actionButton(inputId = "fit.model.Btn", label = "RUN MODEL FITTING"),
                                                   actionButton(inputId = "fit.resetBtn", label = "Reset", styleclass = "warning")
                                                 ),
                                                 
                                                 # hr(style = "border-top: 2px solid #D3D3D3;"),
                                                 wellPanel(
                                                   h4(HTML("<b> Save Model Fitting (Calibration) Data </b>")),
                                                   textInput(inputId = "save.fit.model.as", "Enter file name below"),
                                                   downloadButton("download.fit.model.data", "Save"))
                                                 
                                          ))),
                           mainPanel(
                             tabsetPanel(id = "fit.model.Tabset",
                                         #tabPanel("View Raw Data", dataTableOutput("raw_data")),
                                         # tabPanel("Upload.Data", dataTableOutput("prep.data")),
                                         # tabPanel("fit.model.summary", verbatimTextOutput("fit.model.summary"))
                                         tabPanel("View Uploaded Data", DT::DTOutput("prep.data")),
                                         tabPanel("View Model Fitting Summary", verbatimTextOutput("fit.model.summary"))
                                         
                             )
                           )
                         ) # end sidebarLayout
                ), # end tabPanel("Model Fitting"
                
                target = "Data Preparation"
      )
      ######################################################========Score Estimation TAB============#########################################
      
      # insert tab - Score Estimation
      insertTab(inputId = "bspam", 
                tabPanel("Score Estimation", fluid = TRUE, icon = icon("chart-bar"),
                         sidebarLayout(
                           sidebarPanel(style="background: #0033A0", width = 2,
                                        fluidRow(
                                          column(width = 12,
                                                 wellPanel(
                                                   h4(HTML("<b> Which task calibration data will be used? </b>")),
                                                   br(),
                                                   radioButtons(inputId = "calibUseData", label = NULL, inline = F, 
                                                                c("Use the stored task parameters from model fitting tab." = "1",
                                                                  "Upload previously calibrated task parameters." = "2")),
                                                   
                                                   
                                                   conditionalPanel(condition = "input.calibUseData == '2'", 
                                                                    uiOutput('score.calib.resettableInput'),
                                                                    actionButton(inputId = "calib_load.Btn", label = "Load Calibration data"),
                                                                    
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
                                                     uiOutput('score.person.resettableInput'),
                                                     actionButton(inputId = "score.getPrepared.Btn", label = "Load Person data"),
                                                   )),
                                                 
                                                 wellPanel(
                                                   h4(HTML("<b> Select Scoring Estimator </b>")),
                                                   br(),
                                                   radioButtons(inputId = "scoreEst", label = NULL, inline = FALSE,
                                                                # c("mle" = "mle",
                                                                #   "map" = "map",
                                                                #   "eap" = "eap",
                                                                #   "bayes" = "bayes"), selected = "bayes"),
                                                                c("mle" = "mle",
                                                                  "map" = "map",
                                                                  "eap" = "eap"), selected = "mle"),
                                                   selectInput("scoreParSet", "Estimator Options",
                                                               c("Default" = "1", "[Custom]" ="2")),
                                                   # conditionalPanel(
                                                   #   condition = "input.scoreEst == 'bayes'",
                                                   #   selectInput(inputId = "score.person.id",
                                                   #               label = "person.id", choices = NULL),
                                                   #   selectInput(inputId = "score..task.id",
                                                   #               label = "task.id", choices = NULL),
                                                   #   selectInput(inputId = "score.max.counts",
                                                   #               label = "max.counts", choices = NULL),
                                                   #   selectInput(inputId = "score.obs.counts",
                                                   #               label = "obs.counts", choices = NULL),
                                                   #   selectInput(inputId = "score.time",
                                                   #               label = "time", choices = NULL)
                                                   # ),
                                                   # 
                                                   # Only show this panel if not bayes
                                                   conditionalPanel(condition = "input.scoreParSet == '2' & input.scoreEst != 'bayes'",
                                                                    radioButtons(inputId = "scoreSe", label = "se:",
                                                                                 choices = c(Analytic = "analytic",
                                                                                             Bootstrap = "bootstrap"), inline = TRUE, selected="analytic"),
                                                                    conditionalPanel(
                                                                      condition = "input.scoreSe == 'bootstrap'",
                                                                      sliderInput(inputId = "score.failsafe", label = "failsafe:", value = c(0), min = 0, max = 50),
                                                                      sliderInput(inputId = "score.bootstrap", label = "bootstrp:", value = c(100), min = 50, max = 500)
                                                                    )
                                                   )
                                                 ),
                                                 
                                                 wellPanel(h4(HTML("<b> Other Options </b>")),
                                                           br(),
                                                           radioButtons(inputId = "scoreExtOption", 
                                                                        label = "Perform External Scoring",
                                                                        choices = c("no" = "no",
                                                                                    "yes" = "yes",
                                                                                    "upload" = "upload"), inline = TRUE, selected = "no"),
                                                           
                                                           conditionalPanel(condition = "input.scoreExtOption == 'yes'",
                                                                            textInput(inputId = "score.external", 
                                                                                      label = "Scoring based on external option", 
                                                                                      value = "")),
                                                           conditionalPanel(
                                                             condition = "input.scoreExtOption == 'upload'",
                                                             fileInput(inputId = "score.upload.external", NULL, multiple = FALSE),
                                                             actionButton(inputId = "externalload.Btn", label = "Load external"),
                                                           ),
                                                           radioButtons(inputId = "score.type", label = "Output Type",
                                                                        c("general" = "general",
                                                                          "orf" = "orf"), inline = TRUE),
                                                           h6(HTML("<b> Do your task data involve censoring? </b>")),
                                                           # checkboxInput(inputId = "useCens", label = "Use Censoring", value = FALSE),
                                                           radioButtons(inputId = "radioCensP",label="", inline = FALSE,
                                                                        c("No" = "1",
                                                                          "Yes" = "2")),
                                                           ##=== Check if using censoring
                                                           conditionalPanel(
                                                             condition = "input.radioCensP == '2'",
                                                             h6(HTML("<b> Please specify your censoring column: </b>")),
                                                             selectInput(inputId = "scoring.testlet.cens",
                                                                         label = "cens*", choices = NULL),
                                                           )
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
                                                     fileInput(inputId = "score.upload.case", NULL, multiple = FALSE),
                                                     actionButton(inputId = "caseload.Btn", label = "Load case"),
                                                   )),
                                                 
                                                 wellPanel(
                                                   h4(HTML("<b> Perform Scoring </b>")),
                                                   br(),
                                                   actionButton(inputId = "score.Btn", label = "Run", icon = icon("cogs")),
                                                   actionButton(inputId = "score.resetBtn", label = "Reset", styleclass = "warning")
                                                 ), 
                                                 wellPanel(h4(HTML("<b> Save Scoring Data </b>")),
                                                           br(),
                                                           radioButtons(inputId = "save.score.type", label = "File Type",
                                                                        c("csv file" = "csv",
                                                                          "rds file" = "rds"), inline = TRUE),
                                                           textInput(inputId = "save.score.as", "Enter file name below"),
                                                           downloadButton("download.score.data", "Save"))
                                          ))),
                           mainPanel(
                             tabsetPanel(id = "score.Tabset",

                                         tabPanel("View Calibration Data", verbatimTextOutput("calib.data")),
                                         tabPanel("View Uploaded Person Data", DT::DTOutput("score.prep.data")),
                                         # try dataTable
                                         tabPanel("View Scoring Summary", DT::DTOutput("score.summary"))
                                         # tabPanel("View Scoring Summary", verbatimTextOutput("score.summary"))                                     
                             )
                           )
                         ) # end sidebarLayout
                ), # end tabPanel("Score Estimation"
                
                target = "Model Fitting"
      )
    } # end else for condition of TestletFlag
    
    # reset all input
    ResetAll()

  })
  
  ######======= ResetAll =======
  ResetAll <- function() {
    
    # reset PREP =========================
    output$resetUploadInput <- renderUI({
      
      fileInput(inputId = "upload.Data", NULL, multiple = FALSE)
    })
    
    # reset elements
    updateSelectInput(session,inputId = "dataset", selected = "none")
    updateSelectInput(session,'person.id','person.id*', choices = character(0))
    updateSelectInput(session,'occasion', 'occasion (optional)', choices = character(0))
    updateSelectInput(session,inputId = "group", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "task.id", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "sub.task.id", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "max.counts", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "obs.counts", choices = "", selected = character(0))
    updateSelectInput(session,'time', 'time*', choices = character(0))
    updateSelectInput(session,inputId = "testlet.cens", choices = "", selected = character(0))
    
    # reset variable
    values$saveData <- NULL
    values$uploaded_data <- NULL
    
    # reset upload data
    # uploaded_data <- NULL
    output$raw_data <- DT::renderDT({
      datatable(values$uploaded_data) # NULL
    })
    
    output$prep_data <- DT::renderDT({
      datatable(values$saveData) # NULL
    })
    
    output$summary <- renderText({ "" })
    
    # update tabletpanel
    updateTabsetPanel(session, "prepareTabset", selected = "View Raw Data")
    
    updateTextInput(session, "saveas", value = "")
    
    # reset Fit.Model ==============================
    updateRadioButtons(session, "fit_dat", selected = "yes")
    
    updateSliderInput(session,inputId = "k.in", value = c(5))
    updateSliderInput(session,inputId = "rep.in", value = c(2))
    
    updateSelectInput(session, inputId = "parSet", selected = "1")
    
    updateRadioButtons(session, "est", selected = input$est) #input$est
    updateRadioButtons(session, "se", selected = "none")
    updateRadioButtons(session, "verbose", selected = FALSE)
    
    # reset output area
    output$prep.data <- DT::renderDT({
      NULL
    })
    output$fit.modle.summary <- renderText({
      ""
    })    
    
    # update tabletpanel
    updateTabsetPanel(session, "fit.model.Tabset", selected = "View Uploaded Data")
    
    # reset Scoring ========================
    # reset upload
    output$score.person.resettableInput <- renderUI({
      
      fileInput(inputId = "person.upload.prepared", NULL, multiple = FALSE)
    })
    output$score.calib.resettableInput <- renderUI({
      
      fileInput(inputId = "calib.upload.prepared", NULL, multiple = FALSE)
    })
    
    # reset data area
    updateRadioButtons(session, "calibUseData", selected = "1") 
    updateRadioButtons(session, "scoreUseData", selected = "1") 
    
    updateRadioButtons(session, "scoreEst", selected = "bayes") 
    updateSelectInput(session, inputId = "scoreParSet", selected = "1")
    updateRadioButtons(session, "scoreExtOption", selected = "no") 
    updateRadioButtons(session, "score.type", selected = "general")
    updateSliderInput(session,inputId = "score.failsafe", value = c(0))
    updateSliderInput(session,inputId = "score.bootstrap", value = c(100))
    updateRadioButtons(session, "scoreCases", selected = "default")    
    
    updateSelectInput(session, inputId = "radioCensTestlet", selected = "1")
    updateSelectInput(session, inputId = "radioCensP", selected = "1")
    # reset output area
    output$calib.data <- renderText({
      ""
    })
    output$score.prep.data <- renderDataTable({
      NULL
    })
    output$score.summary <- renderDataTable({
      NULL
    })
    
    # update tabletpanel
    updateTabsetPanel(session, "score.Tabset", selected = "View Calibration Data")
    
    # reset Visual ========================
    # reset data area
    updateRadioButtons(session, "VcalibUseData", selected = "1") 
    updateCheckboxGroupInput(session, "plotTaskParameter", selected = "")
    
    # reset output
    values$Vcalib.loaded.Data <- NULL
    values$fit.saved <- NULL
    values$score.calib.loaded_data <- NULL           
    
    output$visual.task <- renderPlotly({ NULL })
    output$visual.information <- renderPlotly({ NULL })
    output$visual.person <- renderPlotly({ NULL })
    updateTabsetPanel(session, "visual.Tabset", selected = "Plot.Task")
  }
  
  ######=================== action for data preparation ==================######
  
  # Using available datasets in bspam
  datasetInput <- reactive({
    if (input$TestletFlag == 0) {
      switch(input$dataset,
             "none" = NULL,
             "passage2" = passage2)     
    } else {
      switch(input$dataset,
             "none" = NULL,
             "sentence.level.data" = sentence.level.data,
             "sentence.cens.high" = sentence.cens.high,
             "sentence.cens.low" = sentence.cens.low
             )     
    }

  })
  
  # get data set columns name
  observeEvent(datasetInput(), {
    updateList(datasetInput())
    
    output$raw_data <- DT::renderDT({
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
    
    output$raw_data <- DT::renderDT({
      df
    })
    return (df)
  })
  
  # getUpload button
  observeEvent(input$getUploadBtn, {
    values$uploaded_data <- Upload_data()
    
    updateList(Upload_data())
    #output$summary <- renderPrint({
    #  summary(Upload_data())
    #})
    output$raw_data <- DT::renderDT({
      Upload_data()
    })
    
  })
  
  output$resetUploadInput <- renderUI({
    
    fileInput(inputId = "upload.Data", NULL, multiple = FALSE)
  })
  
  # prep reset button
  observeEvent(input$resetBtn, {
    
    # reset upload
    output$resetUploadInput <- renderUI({
      
      fileInput(inputId = "upload.Data", NULL, multiple = FALSE)
    })
    
    # reset elements
    updateSelectInput(session,inputId = "dataset", selected = "none")
    updateSelectInput(session,'person.id','person.id*', choices = character(0))
    updateSelectInput(session,'occasion', 'occasion (optional)', choices = character(0))
    updateSelectInput(session,inputId = "group", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "task.id", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "sub.task.id", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "max.counts", choices = "", selected = character(0))
    updateSelectInput(session,inputId = "obs.counts", choices = "", selected = character(0))
    updateSelectInput(session,'time', 'time*', choices = character(0))
    updateSelectInput(session,inputId = "testlet.cens", choices = "", selected = character(0))
    
    # reset variable
    values$saveData <- NULL
    values$uploaded_data <- NULL
    
    # reset upload data
    # uploaded_data <- NULL
    output$raw_data <- DT::renderDT({
      datatable(values$uploaded_data) # NULL
    })
    
    output$prep_data <- DT::renderDT({
      datatable(values$saveData) # NULL
    })
    
    output$summary <- renderText({ "" })
    
    # update tabletpanel
    updateTabsetPanel(session, "prepareTabset", selected = "View Raw Data")
    
    # output$raw_data <- renderText({ "" })
    # output$prep_data <- renderText({ "" })
    # output$summary <- renderText({ "" })
    
    updateTextInput(session, "saveas", value = "")

  })
  
  # fit reset button
  observeEvent(input$fit.resetBtn, {
    
    updateRadioButtons(session, "fit_dat", selected = "yes")
    
    updateSliderInput(session,inputId = "k.in", value = c(5))
    updateSliderInput(session,inputId = "rep.in", value = c(2))
    
    updateSelectInput(session, inputId = "parSet", selected = "1")
    
    updateRadioButtons(session, "est", selected = input$est) #input$est
    updateRadioButtons(session, "se", selected = "none")
    updateRadioButtons(session, "verbose", selected = FALSE)
    
    # reset output area
    output$prep.data <- DT::renderDT({
      NULL
    })
    output$fit.modle.summary <- renderText({
      ""
    })    
    
    # update tabletpanel
    updateTabsetPanel(session, "fit.model.Tabset", selected = "View Uploaded Data")
    
  })
  
  # run button
  observeEvent(input$runBtn, {
    #browser()
    if (!is.null(datasetInput())) {
      data.name <- datasetInput()
    } else {
      if (!is.null(values$uploaded_data)) {
        data.name <- values$uploaded_data
      }
    }
    if (exists("data.name")) {
      if (input$person.id == "" |
          # these two are not necessary
          # input$occasion == "" |
          # input$group == "" |
          input$task.id == "" |
          # input$sub.task.id == "" |
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

      if (input$TestletFlag == 0) { # Task level
        small_data <- prep(data = data.name,
                           person.id = input$person.id,
                           occasion = input$occasion,
                           group = input$group,
                           task.id = input$task.id,
                           max.counts = input$max.counts,
                           obs.counts = input$obs.counts,
                           cens = input$testlet.cens,
                           time = input$time)        
      } else { # sub task level with Testlet ON
        small_data <- prep(data = data.name,
                           person.id = input$person.id,
                           occasion = input$occasion,
                           group = input$group,
                           task.id = input$task.id,
                           sub.task.id = input$sub.task.id,
                           max.counts = input$max.counts,
                           obs.counts = input$obs.counts,
                           time = input$time,
                           cens = input$testlet.cens,
                           sentence_level = TRUE)  
      }

      
      values$saveData <- small_data
      
      output$summary <- renderPrint({
        summary(small_data$data.long %>% select(-person.id, -task.id))
      })
      
      output$prep_data <- renderDataTable({
        
        # small_data$data.long
        
        # check if show occation and group columns
          show_data <- small_data$data.long
          if (input$occasion == "") {
            show_data <- show_data %>% select(-occasion)  
          } 
          if (input$group == "") {
            show_data <- show_data %>% select(-group)  
          } 
          return(show_data)
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
      saveRDS(values$saveData, file)
    }
  )
  
  updateList <- function(df) {
    choices_list = colnames(df)
    updateSelectInput(inputId = "person.id", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "occasion", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "group", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "task.id", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "sub.task.id", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "max.counts", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "obs.counts", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "time", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "testlet.cens", choices = choices_list, selected = character(0))    
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
    
    # check
    if (input$TestletFlag == 1) { # testlet
      # check prepared data class
      if (class(df) != "prepared.sub.task") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct prepared data!",
          easyClose = TRUE
        ))
        return()
      }
    } else {
      # check prepared data class
      if (class(df) != "prepared.task") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct prepared data!",
          easyClose = TRUE
        ))
        return()
      }
    }
      
    #reset summary area
    output$fit.model.summary <- renderText({ "" })
    
    updateTabsetPanel(session, "fit.model.Tabset", selected = "Upload.Data") #Upload.Data
    return (df)
    
  })
  
  # load prepared button
  observeEvent(input$getPrepared.Btn, {
    values$LoadedPrepared_data <- load_preparedData()
    
    if (input$TestletFlag == 1) { # testlet
      # check prepared data class
      if (class(values$LoadedPrepared_data) != "prepared.sub.task") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct prepared data!",
          easyClose = TRUE
        ))
        return()
      }
      output$prep.data <- DT::renderDT({
        load_preparedData()[[1]]
      })        
    } else {
      # check prepared data class
      if (class(values$LoadedPrepared_data) != "prepared.task") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct prepared data!",
          easyClose = TRUE
        ))
        return()
      }
      # output$prep.data <- renderDataTable({
      #   load_preparedData()[[1]]
      # })
      if (input$est == "mcem") {
        output$prep.data <- DT::renderDT({
          load_preparedData()[[1]]
        })
      } else { # bayes
        output$prep.data <- DT::renderDT({
          load_preparedData()
        })
        # get columns list
        fit.model.updateList(load_preparedData())
      }
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

    
    if (input$fit_dat == "yes") { # Default to use prepared data
      target.data <- values$saveData
    } else { # use uploaded prepared data
      # showModal(modalDialog( # for debug
      #   title = "good",
      #   "here",
      #   easyClose = TRUE
      # ))
      target.data <- values$LoadedPrepared_data
    }
    
    if (length(target.data) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please prepared your data first!",
        easyClose = TRUE
      ))
      return()
    } else {
      
      # # print selected arguments
      # print(paste("Fit.Model Arguments:", input$est,input$k.in,
      #             input$rep.in,input$se,input$verbose))
      
      # update tabletpanel
      updateTabsetPanel(session, "fit.model.Tabset", selected = "View Model Fitting Summary") #fit.model.summary
      
      
      output$fit.model.summary <- renderText({ "" })
      
      values$fit.model.result <- NULL
      
      # #Create 0-row data frame which will be used to store data
      dat <- data.frame(x = numeric(0), y = numeric(0))
      
      tryCatch({
        withProgress(message = 'Running Model Fitting.', detail = 'This may take a while...', value = 0, {
          
          #   # Number of times we'll go through the loop
          n <- 10
          #
          for (i in 1:n) {
            
            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = "Please Wait...")
            if (i > 5) {
              Sys.sleep(0.2)   
              incProgress(1/n, detail = "Please Wait...")
            }
            if (i == 5) {
              if (length(values$fit.model.result) == 0) {
                if (input$est == "mcem") { # mcem
                  # showModal(modalDialog( # for debug
                  #   title = "good",
                  #   c(input$rep.in),
                  #   easyClose = TRUE
                  # ))
                  if (input$TestletFlag == 0) {
                    # print selected arguments
                    print(paste("Fit.Model Arguments:", input$est,input$k.in,
                                input$rep.in,input$se,input$verbose))
                    
                    values$fit.model.result <- fit.model(data=target.data,
                                                         # person.data = target.data$data.long, #DO WE NEED THE PERSON DATA FOR CALIBRATION?
                                                         # data = target.data$data.long, #DO WE NEED THE PERSON DATA FOR CALIBRATION?
                                                         est = input$est,
                                                         verbose=input$verbose,
                                                         k.in = as.numeric(input$k.in),
                                                         reps.in = as.numeric(input$rep.in),
                                                         se=input$se)                 
                  } else { # with Testlet
                    values$fit.model.result <- fit.model(data=target.data,
                                                         # person.data = target.data$data.long, #DO WE NEED THE PERSON DATA FOR CALIBRATION?
                                                         # data = target.data$data.long, #DO WE NEED THE PERSON DATA FOR CALIBRATION?
                                                         testlet=TRUE)
                  }
  
                  
                  
                } else { #bayes
                  # showModal(modalDialog( # for debug
                  #   title = "good",
                  #  "bayes",
                  #   easyClose = TRUE
                  # ))
                  #test
                  values$fit.model.result <- fit.model(#data=target.data$data.wide,
                    # person.data=target.data$data.long, #Corrected this part for bayes estimator..
                    data=target.data$data.long, #Corrected this part for bayes estimator..
                    person.id = "person.id",
                    task.id = "task.id",
                    max.counts = "max.counts",
                    obs.counts = "obs.counts",
                    time = "time",
                    est = "bayes")
                }
                
                values$fit.saved <- values$fit.model.result
              } else {
                break
              }
            }
            
          }
        })
        
        output$fit.model.summary <- renderPrint({
          values$fit.model.result %>% summary()
        })
        
      }, error=function(e) {
        showModal(modalDialog(
          title = "Error",
          paste0("Please check your data! Running with error: ", e), 
          easyClose = TRUE
        ))
      }, warning=function(w) {
        showModal(modalDialog(
          title = "Warning",
          paste0("Please check your data! Running with warning: ", w),
          easyClose = TRUE
        ))
      })
      
    }
  }) # end  observeEvent(input$fit.model.Btn
  
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
      values$fit.model.result <- NULL
      updateTextInput(session, "save.fit.model.as", value = "")
    }
    if (use_set == "yes") { # Default data
      # will reset all input
      # reset upload
      values$LoadedPrepared_data <- NULL
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
      saveRDS(values$fit.saved, file)
    }
  )
  
  ######=================== action for Testlet ==================######
  
  ##====================== Block for load Testlet data =============##
  # show upload button of testlet data
  output$testletDataInput <- renderUI({
    
    fileInput(inputId = "testlet.upload.data", NULL, multiple = FALSE)
  })
  # load testlet data 
  load.testlet.Data <- reactive({
    req(input$testlet.upload.data)
    ext <- tools::file_ext(input$testlet.upload.data$name)
    if (ext == "csv") {
      df <- read.csv(input$testlet.upload.data$datapath, header=TRUE)
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$testlet.upload.data$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$testlet.upload.data$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$testlet.upload.data$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }
    
    updateTabsetPanel(session, "testlet.fit.model.Tabset", selected = "View Uploaded Data") # Tabset name, tabID
    return (df)
    
  })
  

  updateTestletList <- function(df) {
    choices_list = colnames(df)
    updateSelectInput(inputId = "testlet.person.id", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "testlet.sub.task.id", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "testlet.obs.counts", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "testlet.time", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "testlet.task.id", choices = choices_list, selected = character(0))
    updateSelectInput(inputId = "testlet.max.counts", choices = choices_list, selected = character(0))
    
  }
  
  # load testlet data button
  observeEvent(input$uploadTestletBtn, {
    values$testlet.loaded_data <- load.testlet.Data()
    
    # output$testlet.data <- renderPrint({ # OK
    #   testlet.loaded_data
    # })
    updateTestletList(values$testlet.loaded_data)
    output$testlet.upload.data <- DT::renderDT({ # update testlet.upload.data view
      datatable(values$testlet.loaded_data)
    })
  })  #end observe testlet
  
  # runTestletBtn 
  observeEvent(input$runTestletBtn, {

    if (is.null(values$testlet.loaded_data)) {
      showModal(modalDialog(
        title = "Error",
        "Please make sure you correctly uploaded the data!",
        easyClose = TRUE
      ))
      return()
    } else {

      if (input$testlet.person.id == "" |
          input$testlet.task.id == "" |
          input$testlet.max.counts == "" |
          input$testlet.obs.counts == "" |
          input$testlet.time == "" |
          input$testlet.sub.task.id == "" ) {
        showModal(modalDialog(
          title = "Error",
          "Please set all arguments!",
          easyClose = TRUE
        ))
        return()
      }
      # update tabletpanel
      updateTabsetPanel(session, "testlet.fit.model.Tabset", selected = "View Model Fitting Summary") #tableset ID, tab title
      
      # output$fit.model.summary <- renderText({ "" })
      values$testlet.fit.model.result <- NULL
      withProgress(message = 'Running Testlet Model Fitting.', value = 0, {
        
        #   # Number of times we'll go through the loop
        n <- 10
        #
        for (i in 1:n) {
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = "Please Wait...")
          if (i > 5) {
            Sys.sleep(0.2)   
            incProgress(1/n, detail = "Please Wait...")
          }

          if (i == 5) {
            if (length(values$testlet.fit.model.result) == 0) {
              values$testlet.fit.model.result <- fit.model(data=values$testlet.loaded_data,
                                                            person.id=input$testlet.person.id, 
                                                            sub.task.id=input$testlet.sub.task.id,
                                                            obs.count=input$testlet.obs.counts,
                                                            time=input$testlet.time,
                                                            task.id=input$testlet.task.id, 
                                                            max.counts = input$testlet.max.counts,
                                                            testlet=TRUE)
            } else {
              break
            }
          }
          
        }
      })
      
      print.testlet.result <- function (df) {
        # z <- df[6:14]
        # tb <- as.data.frame(t(do.call(rbind, z)))
        # tb <- tb[,(1:4)]
        z <- df
        tb <- as.data.frame(t(do.call(rbind, z[[1]])))
        tb <- tb[,(1:4)]
        
        tt <- as.data.frame(sapply(lapply(tb, sprintf, fmt = "%6.3f"), as.numeric))
        
        print(tt, print.gap = 3L) 
        cat("\n====== Hyper Parameters ======\n")
        cat(paste("sigma", "      :     "))
        cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$sigma), "\n")) # CHANGE TO .3f
        cat(paste("gamma1"), "     :     ")
        cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$gamma1), "\n")) # CHANGE TO .3f
        cat(paste("gamma2"), "     :     ")
        cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$gamma2), "\n")) # CHANGE TO .3f
        cat(paste("rho.theta"), "  :     ")
        cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$rho.theta), "\n")) # CHANGE TO .3f
        cat(paste("rho.testlet"), ":     ")
        cat(paste(sprintf(fmt = "%6.3f", z$hyper.param$rho.testlet), "\n")) # CHANGE TO .3f
        
      }

      # update tabletpanel
      updateTabsetPanel(session, "testlet.fit.model.Tabset", selected = "View Model Fitting Summary") # Tabset name, tabID
     
      # print(values$testlet.fit.model.result)
      
      output$testlet.fit.model <- renderPrint({
        print.testlet.result(values$testlet.fit.model.result)
      })
      
      # in case for plot.task
      values$fit.saved <- values$testlet.fit.model.result
    }
  }) # end  observeEvent(input$runTestletBtn
  

  # show upload button of censor data
  output$censorDataInput <- renderUI({
    
    fileInput(inputId = "censor.upload.data", NULL, multiple = FALSE)
  })
  # load censor data 
  load.censor.Data <- reactive({
    req(input$censor.upload.data)
    ext <- tools::file_ext(input$censor.upload.data$name)
    if (ext == "csv") {
      df <- read.csv(input$censor.upload.data$datapath, header=TRUE)
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$censor.upload.data$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$censor.upload.data$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$censor.upload.data$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }
    
    # updateTabsetPanel(session, "TestletData.Tabset", selected = "View Testlet Data") # Tabset name, tabID
    return (df)
    
  })
  # load censor data button
  observeEvent(input$uploadCensorBtn, {
    values$censor.loaded_data <- load.censor.Data()
  })  #end observe censor
  

  # resetTestletBtn button 
  observeEvent(input$resetTestletBtn, {
    # reset data area
    # reset upload
    output$testletDataInput <- renderUI({

      fileInput(inputId = "testlet.upload.data", NULL, multiple = FALSE)
    })
    
    # reset output area   
    values$testlet.loaded_data <- NULL
    output$testlet.upload.data <- DT::renderDT({ # update testlet.upload.data view
      datatable(values$testlet.loaded_data)
    })
    
    # reset condition
    updateSelectInput(session, inputId = "testlet.person.id", choices = "", selected = character(0))
    updateSelectInput(session, inputId = "testlet.sub.task.id", choices = "", selected = character(0))
    updateSelectInput(session, inputId = "testlet.obs.counts", choices = "", selected = character(0))
    updateSelectInput(session, inputId = "testlet.time", choices = "", selected = character(0))
    updateSelectInput(session, inputId = "testlet.task.id", choices = "", selected = character(0))
    updateSelectInput(session, inputId = "testlet.max.counts", choices = "", selected = character(0))  
    
    # updateTextInput(session, "saveTestletas", value = "")
    values$testlet.loaded_data <- NULL
  })
  
  output$downloadTMData <- downloadHandler(
    filename = function() {
      paste(input$saveTestletModel, ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(values$testlet.fit.model.result, file)
    }
  )
  
  ##====================== Block for Testlet.scoring =============##
  
  # -------------- runSPBtn 
  observeEvent(input$runSPBtn, {
    # print(values$censor.loaded_data)
    if (is.null(values$testlet.fit.model.result)) {
      showModal(modalDialog(
        title = "Error",
        "Please run Fit.model.Testlet first!",
        easyClose = TRUE
      ))   
      return()
    } 
        
    if (is.null(values$censor.loaded_data)) {
      showModal(modalDialog(
        title = "Error",
        "Please make sure you correctly uploaded the data!",
        easyClose = TRUE
      ))
      return()
    } else {
      
      # update tabletpanel
      updateTabsetPanel(session, "score.Tabset", selected = "View Scoring") # Tabset name, tabID
      
      # get censoring data
      Cens_sentence_level <- data.matrix(values$censor.loaded_data %>% 
                                           select(id.student, id.passage, id.sentence,cens) %>% 
                                           mutate(id.seq = paste0(id.passage,id.sentence)) %>%
                                           select(id.student, id.seq, cens) %>% 
                                           pivot_wider(names_from = id.seq, values_from = cens) %>% 
                                           select(-id.student))
      # print(Cens_sentence_level)
      # 
      # print(values$testlet.fit.model.result)
      
      values$scoring.passage.result <- NULL
      withProgress(message = 'Running Scoring Passage with Testlet.', value = 0, {
        
        #   # Number of times we'll go through the loop
        n <- 10
        #
        for (i in 1:n) {
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = "Please Wait...")
          if (i > 5) {
            Sys.sleep(0.2)   
            incProgress(1/n, detail = "Please Wait...")
          }
          
          if (i == 5) {
            if (length(values$scoring.passage.result) == 0) {
              values$scoring.passage.result <- scoring.passage(Count=values$testlet.fit.model.result$Y, logT10=values$testlet.fit.model.result$logT10, 
                                                        N=values$testlet.fit.model.result$N,
                                                        #Passage,
                                                        a=values$testlet.fit.model.result$task.param$a, b=values$testlet.fit.model.result$task.param$b,
                                                        alpha=values$testlet.fit.model.result$task.param$alpha, beta=values$testlet.fit.model.result$task.param$beta,
                                                        sigma=values$testlet.fit.model.result$hyper.param$sigma, rho=values$testlet.fit.model.result$hyper.param$rho.testlet,
                                                        #rhoTestlet,
                                                        C=Cens_sentence_level)
            } else {
              break
            }
          }
          
        }
      })
      
      # print(values$scoring.passage.result)
      
      # format result
      temp.result <- as.data.frame(do.call(cbind, values$scoring.passage.result))  
      max_col <- dim(temp.result)[2] # the number of columns                             
      result <- as.data.frame(lapply(temp.result[,1:max_col],
                        sprintf, fmt = "%6.3f")) 
      colnames(result) <- colnames(temp.result)

      output$scoring.passage <- DT::renderDT({
        # datatable(do.call(cbind, values$scoring.passage.result))
        datatable(result)
      })
      
    }
  }) # end  observeEvent(input$runSPBtn
  
  # save scoring passage result
  output$downloadSPData <- downloadHandler(
    filename = function() {
      if (input$saveScoringPassage == "rds") { # rds file
        # paste(input$save.score.as, ".rds", sep = "")       
        paste0(input$saveScoringPassage, ".rds")    
      } else { #csv file
        paste0(input$saveScoringPassage, ".csv")
      }
    },
    content = function(file) {
      if (input$save.score.type == "rds") { # rds file
        saveRDS(values$scoring.passage.result, file)
      } else { #csv file
        write.csv(as.data.frame(do.call(cbind, values$scoring.passage.result)), file)   
      }
    }
  )
  
  
  ######=================== End --- action for Testlet ==================######
  
  
  ######=================== action for score estimating ==================

  ######====================== Block for load calib data =============
  
  #==================== show upload button of calib data =======
  output$score.calib.resettableInput <- renderUI({
    
    fileInput(inputId = "calib.upload.prepared", NULL, multiple = FALSE)
  })
  
  # load calib data 
  score.load.calib.Data <- reactive({
    req(input$calib.upload.prepared)
    ext <- tools::file_ext(input$calib.upload.prepared$name)
    if (ext == "csv") {
      df <- read.csv(input$calib.upload.prepared$datapath, header=TRUE)
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$calib.upload.prepared$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$calib.upload.prepared$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$calib.upload.prepared$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }
    
    updateTabsetPanel(session, "score.Tabset", selected = "View Calibration Data") # Tabset name, tabID
    return (df)
    
  })
  
  # load calib data button
  observeEvent(input$calib_load.Btn, {
    load.calib.data <- score.load.calib.Data()
    # Check upload data class
    if (input$TestletFlag == 1) { # testlet
      # check calib data class
      if (class(load.calib.data) != "fit.model.testlet") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct calib data of testlet!",
          easyClose = TRUE
        ))
        return()
      }
    } else {
      # check prepared data class
      if (class(load.calib.data) != "fit.model") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct calib data!",
          easyClose = TRUE
        ))
        return()
      }     
    }
    
    values$score.calib.loaded_data <- load.calib.data
    
    output$calib.data <- renderPrint({
      values$score.calib.loaded_data %>% summary()
    })
  })  #end observe calib
  
  
  ######====================== Block for load person data =============
  
  # show upload button of person data
  output$score.person.resettableInput <- renderUI({
    
    fileInput(inputId = "person.upload.prepared", NULL, multiple = FALSE)
  })
  
  # load person-level formerly prepared data 
  score.load.person.Data <- reactive({
    req(input$person.upload.prepared)
    ext <- tools::file_ext(input$person.upload.prepared$name)
    if (ext == "csv") {
      df <- read.csv(input$person.upload.prepared$datapath, header=TRUE)
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$person.upload.prepared$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$person.upload.prepared$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$person.upload.prepared$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }
    
    # check
    if (input$TestletFlag == 1) { # testlet
      # check prepared data class
      if (class(df) != "prepared.sub.task") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct prepared data!",
          easyClose = TRUE
        ))
        return()
      }
    } else {
      # check prepared data class
      if (class(df) != "prepared.task") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct prepared data!",
          easyClose = TRUE
        ))
        return()
      }
    }
    
    updateTabsetPanel(session, "score.Tabset", selected = "View Uploaded Person Data") # Tabset name, tabID
    return (df)
    
  })
  
  # load person data button
  observeEvent(input$score.getPrepared.Btn, {
    score.person.loaded_data <<- score.load.person.Data()
    if (input$TestletFlag == 1) { # testlet
      # check prepared data class
      if (class(score.person.loaded_data) != "prepared.sub.task") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct prepared data!",
          easyClose = TRUE
        ))
        output$score.prep.data <- renderDataTable({
          NULL
        })
        return()
      }
    } else {
      # check prepared data class
      if (class(score.person.loaded_data) != "prepared.task") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct prepared data!",
          easyClose = TRUE
        ))
        output$score.prep.data <- renderDataTable({
          NULL
        })
        return()
      }     
    }
    
    output$score.prep.data <- renderDataTable({
      score.person.loaded_data[[1]]
    })
    # load columns name
    scoring.updateList(score.person.loaded_data[[1]])
  })  #end observe person
  
  ##============ update cens list ===========
  scoring.updateList <- function(df) {
    choices_list = colnames(df)
    updateSelectInput(inputId = "scoring.testlet.cens", choices = choices_list, selected = character(0))
  }
  
  ##====================== Block for case data ====================
  
  # load cases
  score.load.case.data <- reactive({
    req(input$score.upload.case)
    ext <- tools::file_ext(input$score.upload.case$name)
    if (ext == "csv") {
      df <- read.csv(input$score.upload.case$datapath, header=TRUE, sep = ",") 
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$score.upload.case$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$score.upload.case$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$score.upload.case$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }
    
    return (df)
    
  })
  
  # load case data button
  observeEvent(input$caseload.Btn, {
    score.loaded.case.Data <<- score.load.case.data()
    # print(score.loaded.case.Data)
    showModal(modalDialog(
      title = "Load Case",
      print(paste("Loaded cases:",toString(score.loaded.case.Data[[1]]))),
      easyClose = FALSE
    ))
  })  #end load case data
  
  # load external
  score.load.external.data <- reactive({
    req(input$score.upload.external)
    ext <- tools::file_ext(input$score.upload.external$name)
    if (ext == "csv") {
      df <- read.csv(input$score.upload.external$datapath, header=TRUE, sep = ",") 
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$score.upload.external$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$score.upload.external$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$score.upload.external$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }
    
    return (df)
    
  })
  # load external button
  observeEvent(input$externalload.Btn, {
    values$score.loaded.external.Data <- score.load.external.data()
    # print(values$score.loaded.external.Data)
    showModal(modalDialog(
      title = "Load External",
      print(paste("Loaded externals:",toString(values$score.loaded.external.Data[[1]]))),
      easyClose = TRUE
    ))
  })  #end load external
  
  ##========= scoring button for Testlet or task level ======
  observeEvent(input$score.Btn, {
    #browser()
    # get data
    calib.data <- NULL
    person.data <- NULL
    case.data <- NULL
    external.option <- NULL
    censoring <- NULL
    
    # get censoring set
    # if (input$radioCensP == 2 && input$scoring.telst.cens != "") {
    if (input$radioCensP == 2) {  
      censoring <- TRUE
    } else {
      censoring <- FALSE
    }
    
    if (input$calibUseData == "1") { # Default to use prepared and fit.model data
      # print(fit.saved)
      calib.data <- values$fit.saved
    } else { # when "2", upload calib data
      calib.data <- values$score.calib.loaded_data
    }
    
    if (input$scoreUseData == "1") { # Default to use prepared and fit.model data
      # calib.data <- fit.saved
      if (class(values$LoadedPrepared_data)[[1]] == "list") { # PREPARED DATA
        if (censoring && input$TestletFlag == 0) { # Passage and censoring
          person.data <- values$LoadedPrepared_data
        } else {
          person.data <- values$LoadedPrepared_data[[1]]          
        }

        # showModal(modalDialog(
        #   title = "Default",
        #   print("here and get person.data"),
        #   easyClose = FALSE
        # ))
      } else {
        #print(saveData)
        if (censoring && input$TestletFlag == 0) { # Passage and censoring
          if (!is.null(values$saveData)) {
            person.data <- values$saveData
          } else {
            person.data <- values$LoadedPrepared_data
          }
        } else {
          if (!is.null(values$saveData)) {
            person.data <- values$saveData$data.long    
          } else {
            person.data <- values$LoadedPrepared_data$data.long   
          }
   
        }
      }
    } else { # when "2",
      # upload person data
      if (censoring && input$TestletFlag == 0) { # Passage and censoring
        person.data <- score.person.loaded_data      
      } else {
        person.data <- score.person.loaded_data[[1]]        
      }

    }
    
    # for test 
    #print(person.data)

    if (length(person.data) == 0) {
      showModal(modalDialog(
        title = "Error-scoring",
        "Please prepared your data first!",
        easyClose = TRUE
      ))
      return()
    } else {
    
      # external option
      if (input$scoreExtOption == "yes" & length(input$score.external) != 0) {
        external.option <- c(strsplit(input$score.external, split=","))[[1]] #c(input$score.external)
      }
      # if uploaded
      if (input$scoreExtOption == "upload" & length(values$score.loaded.external.Data[[1]]) != 0) {
        print("upload.external")
        external.option <- values$score.loaded.external.Data[[1]]
      }

      #  get case
      if (input$scoreCases == "default") { # without case data
        print("without case")
      } else {
        print("with case")
        if (input$scoreCases == "input") {
          # get input case
          print(input$input.score.cases)
          cases <- as.data.frame(unlist(strsplit((input$input.score.cases), split=",")))
          colnames(cases) <- c("cases")
          case.data <- cases
        } else { # upload
          # get uploaded case 
          case.data <- data.frame(cases = score.loaded.case.Data[[1]]) 
        }
      }
      
      if (input$scoreSe == "bootstrap" & length(case.data) == 0) {
        showModal(modalDialog(
          title = "Error-bootstrap",
          print("Please upload or input case data for bootstrap"),
          easyClose = FALSE
        ))
        return()
      }
      
      # # print selected arguments
      # print(paste("Scoring Arguments:", input$scoreEst,input$score.failsafe,
      #             input$score.bootstrap,input$scoreSe,input$score.type, 
      #             input$scoreCases, "external=", input$scoreExtOption))
      
      # update tabletpanel
      updateTabsetPanel(session, "score.Tabset", selected = "View Scoring Summary")
      
      output$score.summary <- renderText({ "" })
      
      values$score.result <- NULL
      
      # #Create 0-row data frame which will be used to store data
      dat <- data.frame(x = numeric(0), y = numeric(0))
      
      tryCatch({
        withProgress(message = 'Running Score Estimating.', value = 0, {
          
          #   # Number of times we'll go through the loop
          n <- 10
          #
          for (i in 1:n) {
            
            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = "Please Wait...")
            if (i > 5) {
              Sys.sleep(0.2)   
              incProgress(1/n, detail = "Please Wait...")
            }
            
            if (i == 5) {
              if (length(values$score.result) == 0) {
                if (input$scoreEst == "bayes") { # bayes
                  # print selected arguments
                  print(paste("Scoring Arguments:", input$scoreEst,input$score.failsafe,
                              input$score.bootstrap,input$scoreSe,input$score.type, 
                              input$scoreCases, "external=", input$scoreExtOption))
                  values$score.result <- scoring(calib.data = calib.data,
                                          data = person.data, # person.data$data.long,
                                          # person.data = person.data, # person.data$data.long,                                        
                                          est = input$scoreEst,
                                          se=input$scoreSe,
                                          type = input$score.type,
                                          cases = case.data,
                                          external = external.option
                                          
                  )
                  
                } else { # the others
  
                  
                  if (input$TestletFlag == 0) { # for task level
                    # print selected arguments
                    print(paste("Scoring Arguments:", input$scoreEst,input$score.failsafe,
                                input$score.bootstrap,input$scoreSe,input$score.type, 
                                input$scoreCases, "external=", input$scoreExtOption, "censoring=", censoring))
                    if (censoring) { # with censoring
                      values$score.result <- scoring(calib.data=calib.data,
                                                     # person.data = person.data, # person.data$data.long,
                                                     data = person.data, # person.data,                                        
                                                     est = input$scoreEst,
                                                     failsafe = as.numeric(input$score.failsafe),
                                                     bootstrap = as.numeric(input$score.bootstrap),
                                                     se=input$scoreSe,
                                                     type = input$score.type,
                                                     cases = case.data,
                                                     external = external.option,
                                                     censoring = TRUE)  
                      # when censoring, hide plot_person_panel
                      shinyjs::hide("plot2")
                      hideTab(inputId = "visual.Tabset", target = "Plot.Person")
                    } else { # without censoring
                      values$score.result <- scoring(calib.data=calib.data,
                                                     # person.data = person.data, # person.data$data.long,
                                                     data = person.data, # person.data$data.long,                                        
                                                     est = input$scoreEst,
                                                     failsafe = as.numeric(input$score.failsafe),
                                                     bootstrap = as.numeric(input$score.bootstrap),
                                                     se=input$scoreSe,
                                                     type = input$score.type,
                                                     cases = case.data,
                                                     external = external.option)   
                      showTab(inputId = "visual.Tabset", target = "Plot.Person")
                      shinyjs::show("plot2")     
                    }
  
                  } else {
                    if (input$radioCensTestlet == 1) { # with user selected no censoring
                      person.data$cens <- 0 # set cens to 0  
                    }
                    values$score.result <- scoring(calib.data=calib.data, 
                                              data = person.data,
                                              person.id = "person.id",
                                              task.id = "task.id",
                                              sub.task.id = "sub.task.id",
                                              max.counts = "max.counts",
                                              obs.counts = "obs.counts",
                                              time = "time",
                                              cens = "cens",
                                              censoring = TRUE,
                                              testlet = TRUE
                                          )
                  }
                                            
                                      
                }
  
                values$score.saved <- values$score.result
              } else {
                break
              }
            }
            
          }
        })
      }, error=function(e) {
        showModal(modalDialog(
          title = "Error",
          paste0("Please check your data! Running with error: ", e), 
          easyClose = TRUE
        ))
      }, warning=function(w) {
        showModal(modalDialog(
          title = "Warning",
          paste0("Please check your data! Running with warning: ", w),
          easyClose = TRUE
        ))
      })  
      
      # output$score.summary <- renderPrint({
      #   if (class(score.result)[1] != "scoring") { # for bootstrap
      #     score.result %>% summary()
      #   } else {
      #     score.result %>% summary(show = "short")
      #   }
      # })
      
      # try dataTable
      output$score.summary <- renderDataTable({
        if (length(values$score.result)[[1]] != 0) {
          if (class(values$score.result)[1] == "bootstrap") { # for bootstrap
            temp.result <- as.data.frame(values$score.result %>% summary())
            max_col <- dim(temp.result)[2] # the number of columns                             
            result <- cbind(temp.result[,1:6],as.data.frame(lapply(temp.result[,7:max_col],
                                                                 sprintf, fmt = "%6.3f"))) 
            colnames(result) <- colnames(temp.result)
            result
          } else if (class(values$score.result)[1] == "scoring") { # for scoring
            temp.result <- as.data.frame(values$score.result %>% summary(show = "short"))  
            max_col <- dim(temp.result)[2] # the number of columns                             
            result <- cbind(temp.result[,1],as.data.frame(lapply(temp.result[,2:max_col],
                                                                 sprintf, fmt = "%6.3f"))) 
            colnames(result) <- colnames(temp.result)
            result
          } else {
            as.data.frame(lapply(as.data.frame(do.call(cbind, values$score.result)), sprintf, fmt = "%6.3f"))
          }

        }

      })

    }
  }) # end observeEvent(input$score.Btn
  
  # save scoring result
  output$download.score.data <- downloadHandler(
    filename = function() {
      if (input$save.score.type == "rds") { # rds file
        # paste(input$save.score.as, ".rds", sep = "")       
        paste0(input$save.score.as, ".rds")    
      } else { #csv file
        paste0(input$save.score.as, ".csv")
      }
    },
    content = function(file) {
      if (input$save.score.type == "rds") { # rds file
        saveRDS(values$score.saved, file)
      } else { #csv file
        write.csv(as.data.frame(do.call(cbind, values$score.saved)), file)   
      }
    }
  )
  
  # score reset button 
  observeEvent(input$score.resetBtn, {
    
    # reset upload
    output$score.person.resettableInput <- renderUI({
      
      fileInput(inputId = "person.upload.prepared", NULL, multiple = FALSE)
    })
    output$score.calib.resettableInput <- renderUI({
      
      fileInput(inputId = "calib.upload.prepared", NULL, multiple = FALSE)
    })
    
    # reset data area
    updateRadioButtons(session, "calibUseData", selected = "1") 
    updateRadioButtons(session, "scoreUseData", selected = "1") 
    
    updateRadioButtons(session, "scoreEst", selected = "bayes") 
    updateSelectInput(session, inputId = "scoreParSet", selected = "1")
    updateRadioButtons(session, "scoreExtOption", selected = "no") 
    updateRadioButtons(session, "score.type", selected = "general")
    updateSliderInput(session,inputId = "score.failsafe", value = c(0))
    updateSliderInput(session,inputId = "score.bootstrap", value = c(100))
    updateRadioButtons(session, "scoreCases", selected = "default")    
    
    # reset output area
    output$calib.data <- renderText({
      ""
    })
    output$score.prep.data <- renderDataTable({
      NULL
    })
    output$score.summary <- renderDataTable({
      NULL
    })
    
    # update tabletpanel
    updateTabsetPanel(session, "score.Tabset", selected = "View Calibration Data")
    
  })
  
  
  ######=================== action for Visualization ==================
  
  ##====================== Block for load Vcalib data =============##
  
  # show upload button of Vcalib data
  output$Vcalib.resettableInput <- renderUI({
    
    fileInput(inputId = "Vcalib.upload.prepared", NULL, multiple = FALSE)
  })
  
  # load Vcalib data 
  Vload.calib.Data <- reactive({
    req(input$Vcalib.upload.prepared)
    ext <- tools::file_ext(input$Vcalib.upload.prepared$name)
    if (ext == "csv") {
      df <- read.csv(input$Vcalib.upload.prepared$datapath, header=TRUE)
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$Vcalib.upload.prepared$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$Vcalib.upload.prepared$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$Vcalib.upload.prepared$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }
    
    return (df)
    
  })
  
  # load Vcalib data button
  observeEvent(input$Vcalib_load.Btn, {
    
    loaded.data <- Vload.calib.Data()
    # Check upload data class
    if (input$TestletFlag == 1) { # testlet
      # check calib data class
      if (class(loaded.data) != "fit.model.testlet") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct calib data of testlet!",
          easyClose = TRUE
        ))
        return()
      }
    } else {
      # check prepared data class
      if (class(loaded.data) != "fit.model") {
        showModal(modalDialog(
          title = "Error",
          "Please upload a correct calib data!",
          easyClose = TRUE
        ))
        return()
      }     
    }
    values$Vcalib.loaded.Data <- loaded.data
    
  })  #end observe Vcalib
  
  ##====================== Block for load Vscored data =============##
  # show upload button of score data
  output$Vscore.resettableInput <- renderUI({
    
    fileInput(inputId = "Vscore.upload.prepared", NULL, multiple = FALSE)
  })
  
  # load scoring formerly prepared data 
  Vscore.load.Data <- reactive({
    req(input$Vscore.upload.prepared)
    ext <- tools::file_ext(input$Vscore.upload.prepared$name)
    if (ext == "csv") {
      df <- read.csv(input$Vscore.upload.prepared$datapath, header=TRUE)
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$Vscore.upload.prepared$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$Vscore.upload.prepared$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$Vscore.upload.prepared$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }
    
#    updateTabsetPanel(session, "score.Tabset", selected = "View Uploaded Person Data") # Tabset name, tabID
    return (df)
    
  })
  
  observeEvent(input$Vscore.Data.Btn, {
    values$Vscore.loaded.Data <- Vscore.load.Data()
  })
  
  ##---------- plot.information button -------
  observeEvent(input$plot.information.Btn, {
    
    infor_result <- NULL
    # check data
    if (input$VcalibUseData == "1") { # Default to use fit.model data in prior process
      if (!is.null(values$fit.saved)) { # if saved fit.model data
        calib.data <- values$fit.saved        
      } else { # else
        calib.data <- values$score.calib.loaded_data           
      }
      
      
    } else { # when "2", upload Vcalib data
      calib.data <- values$Vcalib.loaded.Data
    }
    
    if (!is.null(calib.data)) { #values$Vcalib.loaded.Data
      
      withProgress(message = 'Running Information', value = 0, {
        
        #   # Number of times we'll go through the loop
        n <- 10
        #
        for (i in 1:n) {
          

          # if (i > 5) {
          #   Sys.sleep(0.2)   
          #   incProgress(1/n, detail = "Please Wait...")
          # } 
          if (i == 2) {
              if (is.null(infor_result)) {
                output$visual.information <- renderPlotly({
                  infor_result <- plot.information(calib_data=calib.data)
                })                
              } else {
                break
              }
          } else {
            # Increment the progress bar, and update the detail text.
            Sys.sleep(0.2) 
            incProgress(1/n, detail = "Please Wait...")
          }
        }
          
      })
        
      # output$visual.information <- renderPlotly({
      #   information_func(calib_data=values$Vcalib.loaded.Data)
      # })
      updateTabsetPanel(session, "visual.Tabset", selected = "Plot.Information") # show plot.information
      
    } else {
      showModal(modalDialog(
        title = "Error-plotting",
        "Please select correct calibration data!",
        easyClose = TRUE
      ))   
    }
  })
  
  #---------- plot.task button -------
  observeEvent(input$plot.task.Btn, {
    # check data
    if (input$VcalibUseData == "1") { # Default to use fit.model data in prior process
      if (!is.null(values$fit.saved)) { # if saved fit.model data
        calib.data <- values$fit.saved        
      } else { # else
          calib.data <- values$score.calib.loaded_data           
      }


    } else { # when "2", upload Vcalib data
      calib.data <- values$Vcalib.loaded.Data
    }
    
    if (!is.null(calib.data)) {
      # check
      if (length(input$plotTaskParameter) == 0) { # no select
        showModal(modalDialog(
          title = "Error-plotting",
          "Please select at least one parameter for plotting!",
          easyClose = TRUE
        ))   
        return()
      }
      # get input parameters
      inputParam <- strsplit(input$plotTaskParameter,split=" ") #list
      
      if (length(inputParam) > 2) { # error
        showModal(modalDialog(
          title = "Error-plotting",
          "Only two parameters can be visualized at a time!",
          easyClose = TRUE
        ))  
        return()
      } else {
        # prepare parameter
        if (length(inputParam) == 1) {
          taskParam <- c(unlist(inputParam[1]))
        } else {
          taskParam <- c(unlist(inputParam)[1], unlist(inputParam)[2])
        }
      }

        output$visual.task <- renderPlotly({
  
        if (!(input$plotTaskid ==  "")) { # with task id
            inputTaskid <- strsplit(input$plotTaskid,split=",") #list
            task_Ids <- c(unlist(inputTaskid[1]))
            plot.task(object = calib.data, parameter = taskParam, sort = input$plotTaskSort, task = task_Ids) 
          } else { # without task id
            plot.task(object = calib.data, parameter = taskParam, sort = input$plotTaskSort)
          }
        })
        updateTabsetPanel(session, "visual.Tabset", selected = "Plot.Task") # show plot.task
    } else {
        showModal(modalDialog(
          title = "Error-plotting",
          "Please select your data first!",
          easyClose = TRUE
        ))
    }
  })
  
  #---------- plot.person button -------
  observeEvent(input$plot.person.Btn, {
    
    Vscore.data <- NULL
    plot.obj <- NULL
    personParam <- NULL
    
    # check data
    if (input$VscoreUseData == "1") { # Default to use scoring data in prior process
      if (!is.null(values$score.saved)) { # if saved fit.model data
        Vscore.data <- values$score.saved        
      } 
    } else { # when "2", upload Vscore data
      print(values$Vscore.loaded.Data)
      Vscore.data <- values$Vscore.loaded.Data
    }
    if (!is.null(Vscore.data)) {
      # get input parameters
      inputPersonParam <- strsplit(input$plotPersonParameter,split=" ") #list
      if (length(inputPersonParam) > 2) { # error
        showModal(modalDialog(
          title = "Error-plotting",
          "Only two parameters can be visualized at a time!",
          easyClose = TRUE
        ))  
        return()
      } else {
        # prepare parameter
        if (length(inputPersonParam) == 1) {
          personParam <- c(unlist(inputPersonParam[1]))
        } else { # when two parameters, input$plotPersonShowSE should always be true
          if (input$plotPersonShowSE == FALSE) {
            showModal(modalDialog(
              title = "Warning",
              "You should check Show SE if you select two parameters!", 
              easyClose = TRUE
            ))  
            return()
          }
          personParam <- c(unlist(inputPersonParam)[1], unlist(inputPersonParam)[2])
        }
      }
      
      tryCatch({
        if (!(input$plotPersonid ==  "")) { # with task id
          inputPersonid <- strsplit(input$plotPersonid,split=",") #list
          person_Ids <- c(unlist(inputPersonid[1]))
          plot.obj <- plot.person(object = Vscore.data, parameter = personParam, show.se = input$plotPersonShowSE, sort = input$plotPersonSort, person = person_Ids) 
        } else { # without task id
          plot.obj <- plot.person(object = Vscore.data, parameter = personParam, show.se = input$plotPersonShowSE, sort = input$plotPersonSort)
        }
        output$visual.person <- renderPlotly({
          # if (!(input$plotPersonid ==  "")) { # with task id
          #   inputPersonid <- strsplit(input$plotPersonid,split=",") #list
          #   person_Ids <- c(unlist(inputPersonid[1]))
          #   plot.person(object = Vscore.data, parameter = personParam, show.se = input$plotPersonShowSE, sort = input$plotPersonSort, person = person_Ids) 
          # } else { # without task id
          #   plot.person(object = Vscore.data, parameter = personParam, show.se = input$plotPersonShowSE, sort = input$plotPersonSort)
          # }
          plot.obj
        })
        updateTabsetPanel(session, "visual.Tabset", selected = "Plot.Person") # show plot.person
      }, error=function(e) {
        showModal(modalDialog(
          title = "Error",
          paste0("Please check your data! Running with error: ", e), 
          easyClose = TRUE
        ))
      }, warning=function(w) {
        showModal(modalDialog(
          title = "Warning",
          paste0("Please check your data! Running with warning: ", w),
          easyClose = TRUE
        ))
      })
      

      # output$visual.person <- renderPlotly({
      #   # if (!(input$plotPersonid ==  "")) { # with task id
      #   #   inputPersonid <- strsplit(input$plotPersonid,split=",") #list
      #   #   person_Ids <- c(unlist(inputPersonid[1]))
      #   #   plot.person(object = Vscore.data, parameter = personParam, show.se = input$plotPersonShowSE, sort = input$plotPersonSort, person = person_Ids) 
      #   # } else { # without task id
      #   #   plot.person(object = Vscore.data, parameter = personParam, show.se = input$plotPersonShowSE, sort = input$plotPersonSort)
      #   # }
      #   
      # })
      # updateTabsetPanel(session, "visual.Tabset", selected = "Plot.Person") # show plot.person
    } else {
      showModal(modalDialog(
        title = "Error-plotting",
        "Please select your data first!",
        easyClose = TRUE
      ))
    }
    
  })
  ######===================End for visualization=====================
  

}

shinyApp(ui = ui, server = server)
# Run in a dialog within R Studio
# runGadget(ui, server, viewer = dialogViewer("ShinyBspam", width = 1500, height = 1800))

# Run in Viewer pane
# runGadget(ui, server, viewer = paneViewer(minHeight = 1200))
# 
# # Run in browser
#runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))
