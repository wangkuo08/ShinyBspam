library(shiny)
#library(shinyjs)
library(shinythemes)
library(tidyverse)
library(readr)
library(bspam)


## Define UI for dataset viewer app ----
ui <- fluidPage(
  
  navbarPage("bspam Shiny App", theme = shinytheme("lumen"),
             # add welcome tab
             navbarMenu("Welcome", 
                        tabPanel("Welcome Page", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h2(p("Welcome to bspam Shiny App!!")),
                                          br(),
                                          h4(p("This is the interactive dashboard for bspam R package. bspam stands for 
                                               `binomial log-normal speed-accruacy modeling'. Use of this app does not 
                                               require any knowledge of R. All tasks can be completed interactively by
                                               followign the direcitons provided under the menus/tabs.")), 
                                          h4(p("bspam package has 
                                               functions to fit the speed-accuracy psychometric model for count outcome data 
                                               (Potgieter, Kamata & Kara, 2017; Kara, Kamata, Potgieter & Nese, 2020), 
                                               where the accuracy is modeled by a binomial count latent variable model. 
                                               For example, the use of this modeling technique allows model-based calibration 
                                               and scoring for oral reading fluency (ORF) assessment data.
                                               For a more detailed information about the bspam package, please ",
                                               a("click here.",
                                                 href = "https://github.com/kamataak/bspam")),
                                          ),
                                          h4(p("bspam Shiny App has three main tabs: Data Preparation, Passage Calibration, and Score 
                                               Estimation.")),
                                          h3(p("Data Preparation")),
                                          h4(p("Data preparation allows users to prepare their data for the analyses. For demonstration 
                                          purposes, data preparation tab provised access to sample datasets available in the bspam 
                                               package. Users can uplaod their datasets in various formats including `.rds` and `.csv`. 
                                               Once the data are loaded to the app, the next is assigning the relevant columns to required 
                                               type of variables for fitting the models. This is done by using the dropdown selection 
                                               menus. Users can explore their raw and prepared dataets under the relevant view tabs. 
                                               A descriptivev summary of the prepared dataset is also provided under the Summary Statistics
                                               tab." 
                                               )),
                                          h3(p("Passage Calibration")),
                                          h4(p("This page has the options for performing passage calibration, namely, estimation of passage
                                               paramters. User can select the desired options for the estimation. All non-mandatory options
                                               are pre-selected as the default options as in the relevant bspam function." )), 
                                          h3(p("Score Estimation")),
                                          h4(p("This page has the options for performing score estimation, namely, estimation of person
                                          parameters and model-based scores. User can select the desired options for score estimation. 
                                          All non-mandatory options are pre-selected as the default options as in the relevant bspam function." )), 
                                          
                                          
                                   ))

                        ),

                        tabPanel("About", fluid = TRUE, icon = icon("info-circle"),
                                 fluidRow(
                                   column(6,
                                          #br(),
                                          h4(p("About the Project")),
                                          h5(p("Refer to bspam page????")),
                                          br(),
                                          h5(p("add more text!")),
                                          br(),
                                          h5(p("and more here!"),
                                             p("The source code for this Shiny app is available ", a("on github", href = "add link to here!"), "."))

                                          #hr(),

                                   ),
                                 )
                        )
             ),
             # end  navbarMenu
             tabPanel("Data Preparation", fluid = TRUE, icon = icon("database"),
                      #tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        #sidebarPanel(titlePanel("Please Select Dataset")),
                      
                        sidebarPanel(
       hr(style = "border-top: 2px solid #D3D3D3;"), 
                          # App title ----
                          titlePanel("Try bspam datasets"),

                              # Input: Selector for choosing dataset ----
                          selectInput(inputId = "dataset",
                                      label = HTML("<b> Select </b>"),
                                      choices = c("none","passage")),
                          # selectizeInput(
                          #   'dataset', 'Dataset',
                          #   choices = c("passage"),
                          #   options = list(
                          #     placeholder = HTML("<b>Choose your dataset:</b>"),
                          #     onInitialize = I('function() { this.setValue(""); }')
                          #   )
                          # ),
                          
         hr(style = "border-top: 2px solid #D3D3D3;"), 
                          titlePanel("Load your dataset"), 
                          
                          selectInput(inputId = "datafile",
                                      label = HTML("<b> Select your file type </b>"),
                                      choices = c("rds | rda | rdata", "csv | tsv")),

                          fileInput(inputId = "upload", NULL, multiple = FALSE),
                          actionButton(inputId = "getUploadBtn", label = "Cick to load"),
                          
         hr(style = "border-top: 2px solid #D3D3D3;"),
                          
                          titlePanel("Assign column names"),

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
                         
          hr(style = "border-top: 2px solid #D3D3D3;"), 

                          # Input: Numeric entry for number of obs to view ----
                    #      numericInput(inputId = "obs",
                    #                   label = "Number of observations to view:",
                    #                   value = 10),
       titlePanel("Prepare your data"),
                          actionButton(inputId = "runBtn", label = "Run", icon = icon("gears")),
                          actionButton(inputId = "resetBtn", label = "Reset", styleclass = "warning"),
       hr(style = "border-top: 2px solid #D3D3D3;"),
       titlePanel("Save prepared data"),
                          textInput(inputId = "saveas", "Name your dataset"),
                          downloadButton("downloadData", "Save")
#                          actionButton(inputId = "saveBtn", label = "Save", icon = icon("floppy-disk"))


                          ),

                          # Main panel for displaying outputs ----
                          mainPanel(
                            # useShinyjs(),
                            # style = "overflow-y:scroll; max-height: 800px; position:relative; align: centre",
                            
                            # Output: Formatted text for caption ----
                           
                            
                            # Output: Verbatim text for data summary ----
                            
                            
                            # Output: HTML table with requested number of observations ----
                            tabsetPanel(
                              tabPanel("View Raw Data", dataTableOutput("raw_data")),
                              tabPanel("View Prepared Data", dataTableOutput("prep_data")),
                              tabPanel("Summary Statistics", verbatimTextOutput("summary"))
                              
                            )
                          )

                        )
            ),
            tabPanel("Passage Calibration", fluid = TRUE, icon = icon("ruler"),
                      #tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          # App title ----
                          titlePanel("Set Parameters"),

                          # Input: Selector for choosing functions ----
                          selectInput(inputId = "selected_func_name",
                                      label = "Choose a function:",
                                      choices = c("fit.model", "scoring", "prep")),

                          # Copy the line below to make a checkbox
                          checkboxInput("checkbox", label = "Choice A", value = TRUE),

                          # Input: Numeric entry for number of obs to view ----
                          numericInput(inputId = "obs",
                                       label = "Number of observations to view:",
                                       value = 10)

                        ),
                        # Main panel for displaying outputs ----
                        mainPanel(

                        )

                      )
            ),
            tabPanel("Score Estimation", fluid = TRUE, icon = icon("chart-bar"),
                     #tags$style(button_color_css),
                     # Sidebar layout with a input and output definitions
                     sidebarLayout(
                       sidebarPanel(
                         # App title ----
                         titlePanel("Set Parameters"),

                         # Input: Selector for choosing functions ----
                         selectInput(inputId = "selected_func_name",
                                     label = "Choose a function:",
                                     choices = c("fit.model", "scoring", "prep")),

                         # Copy the line below to make a checkbox
                         checkboxInput("checkbox", label = "Choice A", value = TRUE),

                         # Input: Numeric entry for number of obs to view ----
                         numericInput(inputId = "obs",
                                      label = "Number of observations to view:",
                                      value = 10)

                       ),
                       # Main panel for displaying outputs ----
                       mainPanel(

                       )

                     )
            ),

       )
  )



# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  # Define variables
  saveData <- NULL
  uploaded_data <- NULL

 # docs <- "Guidence for Preparing data..."

 #output$caption <- renderText({
  #  docs
 # })

  # Using available datasets in bspam
  datasetInput <- reactive({
    switch(input$dataset,
           "none" = NULL,
           "passage" = passage)
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


  # uploading a dataset
  Upload_data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    if (ext == "csv") {
      df <- read.csv(input$upload$datapath, header=TRUE)
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$upload$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$upload$datapath)
    } else if (ext == "rda" | ext == "RData" | ext == "rdata") {
      tf <- load(file=input$upload$datapath)
      df <- get(tf)
      rm(tf) # delete temp data
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }

    # first try simple one
#    df <- readRDS(input$upload$datapath)

    updateList(df)

    output$caption <- renderText({
      NULL
    })
    #output$summary <- renderPrint({
    #  summary(df)
    #})
    output$raw_data <- renderDataTable({
      df
    })
    return (df)
  })

  # run button
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

  # observeEvent(Upload_data(), {
  #
  #   output$summary <- renderPrint({
  #     summary(small_data)
  #   })
  #   output$view <- renderTable({
  #     head(small_data$data.long, n = input$obs)
  #   })
  # })

  # reset button
  observeEvent(input$resetBtn, {

    updateSelectInput(session,inputId = "dataset", selected = "none")
    updateSelectInput(session,inputId = "person.id", selected = character(0))
    updateSelectInput(session,inputId = "occasion", selected = character(0))
    updateSelectInput(session,inputId = "group", selected = character(0))
    updateSelectInput(session,inputId = "task.id", selected = character(0))
    updateSelectInput(session,inputId = "max.counts", selected = character(0))
    updateSelectInput(session,inputId = "obs.counts", selected = character(0))
    updateSelectInput(session,inputId = "time", selected = character(0))
    output$prep_data <- NULL
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

  # observeEvent(input$SaveBtn, {
  #   filename <- renderText({
  #     paste0(input$saveas, ".rds")
  #   })
  #   saveRDS(saveData, file = filename())
  # })
  # getSavedata <- function() {
  #   if (!is.na(saveData)) {
  #     return (saveData)
  #   } else {
  #     validate("No data to save")
  #   }
  # }

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


  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes

  # output$summary <- renderPrint({
  #   dataset <- datasetInput()
  #   #browser()
  #   if (class(dataset)[1] == "fit.model") {
  #     dataset %>% summary()
  #   } else {
  #     summary(dataset)
  #   }
  #
  # })

  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed



}



# Create Shiny app ----
shinyApp(ui, server)
