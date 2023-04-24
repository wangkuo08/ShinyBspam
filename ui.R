library(shiny)
library(shinythemes)
library(tidyverse)
library(readr)
library(bspam)


# Define UI for dataset viewer app ----
ui <- fluidPage(
  navbarPage("BSpam Shiny", theme = shinytheme("lumen"),
             # add welcome tab
             navbarMenu("Welcome", icon = icon("info-circle"),
                        tabPanel("Welcome Page", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h4(p("Shiny bspam")),
                                          h5(p("US News and World Report uses four categories of schools for their rankings system:"),
                                             p("National universities are those that offer a “full range” of undergraduate majors, while also offering graduate programs, including at the doctoral level.  Intercollegiate sports, including swimming, are generally pursued by undergrads, or occasionally students in master’s degree programs, so a university having nor not having doctoral programs isn’t directly relevant.  That said, doctoral programs and faculty research go hand-in-hand, so faculty at national universities are nearly always active in research, in addition to their teaching duties.  National universities are usually, though not always, large.  Most state flagship universities would fall under this category."),
                                             p("Regional universities are similar to national universities in that they have a wide range of undergrad programs, and some master’s programs as well.  They generally do not have large doctoral programs, and correspondingly less faculty research."),
                                             p("National liberal arts colleges are undergraduate focused, with few graduate programs.  They award the majority of their degrees in arts and sciences, and may or may not have other undergraduate programs, like engineering or professional studies."),
                                             p("Regional colleges are also undergraduate focused institutions, but do not award the majority of their degrees in arts and/or sciences.  These colleges may have a particular focus, like technology or agriculture, or they may be primarily two year institutions that also grant some four year degrees.")
                                          )
                                   ),
                                   column(6,
                                          h4(p("US News Rankings")),
                                          h5(p("Every year the US News and World Report issues a set of rankings for US colleges and universities.  They are a used in this setting as a guideline, and a general comparative device, but can often be misinterpreted or overvalued.  The major component of a given school’s rankings are graduation and retention rates, academic reputation (basically name recognition), and faculty resources (class size, faculty salary etc.).  Each school is given a score, and then placed in order.  That said the scored differences between schools of different rank can be quite small, so take the rankings with a grain of salt.
                                    The full methodology for the US News and World report college rankings can be found ",
                                               a("here.",
                                                 href = "https://www.usnews.com/education/best-colleges/articles/ranking-criteria-and-weights"))
                                          )
                                   ))

                        ),

                        tabPanel("About", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          #br(),
                                          h4(p("About the Project")),
                                          h5(p("This project is intended to facilitate useful comparisons between colleges in the NCAA, based on swimming performance, location, and academic information.  Here a prospective student-athlete, or anyone else with an interest can find schools fitting a particular set of criterion relevant to them, for example, schools close to home, with times in a particular range, and of a specified academic profile.")),
                                          br(),
                                          h5(p("The project began as an attempt to combine my interest in swimming with a need to practice R, a programming language used primarily for analyzing and reporting data.  It has two components.  The first is this app, which queries a dataset to return information in the form of plots, data tables etc.  The second is the dataset itself, which I assembled by tying together information from the sources below.")),
                                          br(),
                                          h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at gpilgrim2607@gmail.com"),
                                             p("The source code for this Shiny app is available ", a("on github", href = "https://github.com/gpilgrim2670/SwimMap"), "."))

                                          #hr(),

                                   ),
                                 )
                        )
             ),
             # end  navbarMenu
             tabPanel("Prepare Data", fluid = TRUE, icon = icon("globe-americas"),
                      #tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          # App title ----
                          titlePanel("Please Select Dataset"),

                              # Input: Selector for choosing dataset ----
                          selectInput(inputId = "dataset",
                                      label = HTML("<b>Choose your dataset:</b>"),
                                      choices = c("MCEM", "passage", "passage2", "cars")),
                          selectInput(inputId = "datafile",
                                      label = HTML("<b>Load data of type:</b>"),
                                      choices = c("rds | rda | rdata", "csv | tsv")),

                          fileInput(inputId = "upload", NULL, multiple = FALSE),
                          actionButton(inputId = "getUploadBtn", label = "ReadUploadData"),

                          # person.id = "id.student",
                          # occasion = "occasion",
                          # group = "grade",
                          # task.id = "id.passage",
                          # max.counts = "numwords.pass",
                          # obs.counts = "wrc",
                          # time = "sec"
                          h4("Set arguments:"),

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

                          # Input: Numeric entry for number of obs to view ----
                          numericInput(inputId = "obs",
                                       label = "Number of observations to view:",
                                       value = 10),

                          actionButton(inputId = "runBtn", label = "Run", icon = icon("gears")),
                          actionButton(inputId = "resetBtn", label = "Reset", styleclass = "warning"),
                          textInput(inputId = "saveas", "Save result as:"),
                          downloadButton("downloadData", "Save Prepared Data")
#                          actionButton(inputId = "saveBtn", label = "Save", icon = icon("floppy-disk"))


                          ),
                          # Main panel for displaying outputs ----
                          mainPanel(

                            # Output: Formatted text for caption ----
                            h3(textOutput("caption", container = span)),

                            # Output: Verbatim text for data summary ----
                            verbatimTextOutput("summary"),

                            # Output: HTML table with requested number of observations ----
                            tableOutput("view")

                          )

                        )
            ),
            tabPanel("Fit Model", fluid = TRUE, icon = icon("chart-bar"),
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

  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "MCEM" = MCEM,
           "passage" = passage,
           "passage2" = passage2,
           "cars" = cars)
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

    output$summary <- renderPrint({
      dataset <- datasetInput()
      #browser()
      if (class(dataset)[1] == "fit.model") {
        dataset %>% summary()
      } else {
        summary(dataset)
      }

    })
    output$view <- renderTable({
      head(datasetInput(), n = input$obs)
    })
  })

  # upload file
  Upload_data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    if (ext == "csv") {
      df <- read.csv(input$upload$datapath, header=TRUE)
    } else if (ext == "tsv") {
      df <- vroom::vroom(input$upload$datapath, delim = "\t")
    } else if (ext == "rds") {
      df <- readRDS(input$upload$datapath)
    } else {
      validate("Invalid file; Please upload a file with correct extension name")
    }

    # first try simple one
#    df <- readRDS(input$upload$datapath)
#    print(df)
    updateList(df)
    output$summary <- renderPrint({
      summary(df)
    })
    output$view <- renderTable({
      head(df, n = input$obs)
    })
    return (df)
  })

  # run button
  observeEvent(input$getUploadBtn, {
    updateList(Upload_data())
    output$summary <- renderPrint({
      summary(Upload_data())
    })
    output$view <- renderTable({
      head(Upload_data(), n = input$obs)
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

  # run button
  observeEvent(input$runBtn, {
    #browser()
    small_data <- prep(data = datasetInput(),
                       person.id = input$person.id,
                       occasion = input$occasion,
                       group = input$group,
                       task.id = input$task.id,
                       max.counts = input$max.counts,
                       obs.counts = input$obs.counts,
                       time = input$time)

    saveData <<- small_data

    output$summary <- renderPrint({
      summary(small_data)
    })
    output$view <- renderTable({
       head(small_data$data.long, n = input$obs)
    })


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
    updateSelectInput(inputId = "person.id", choices = choices_list)
    updateSelectInput(inputId = "occasion", choices = choices_list)
    updateSelectInput(inputId = "group", choices = choices_list)
    updateSelectInput(inputId = "task.id", choices = choices_list)
    updateSelectInput(inputId = "max.counts", choices = choices_list)
    updateSelectInput(inputId = "obs.counts", choices = choices_list)
    updateSelectInput(inputId = "time", choices = choices_list)

  }
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  # output$caption <- renderText({
  #   input$caption
  # })

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
#  output$view <- renderTable({
#    head(datasetInput(), n = input$obs)
#  })

}

# Create Shiny app ----
shinyApp(ui, server)
