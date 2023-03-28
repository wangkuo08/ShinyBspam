library(shiny)
library(shinythemes)
library(tidyverse)
library(bspam)
# Define UI for dataset viewer app ----
ui <- fluidPage(
  navbarPage("bspam Shiny App", theme = shinytheme("lumen"),
             # add welcome tab
             navbarMenu("Welcome", icon = icon("info-circle"),
                        tabPanel("Welcome Page", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h4(p("Welcome to bspam Shiny App!!")),
                                          h5(p("This is the interactive dasboard for bspam R package to perform data preparation, passage calibration, and WCPM score estimation. For a more detailed information about the bspam package, please ",
                                               a("click here.",
                                                 href = "https://github.com/kamataak/bspam"))
                                          )
                                   ))

                        ),

                        tabPanel("About", fluid = TRUE,
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
                        sidebarPanel(
                          # App title ----
                          titlePanel("Please Select Dataset"),

                              # Input: Selector for choosing dataset ----
                              selectInput(inputId = "dataset",
                                          label = "Choose your dataset:",
                                          choices = c("rock", "pressure", "cars")),


                              # Input: Numeric entry for number of obs to view ----
                              numericInput(inputId = "obs",
                                           label = "Number of observations to view:",
                                           value = 10)

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
server <- function(input, output) {

  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })

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
  output$caption <- renderText({
    input$caption
  })

  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })

  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })

}

# Create Shiny app ----
shinyApp(ui, server)
