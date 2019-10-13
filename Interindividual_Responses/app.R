#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
source("../interindividual_response_functions.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
   
   # Application title
    "Statistics for Sports Science",
   
    # Nav bar layout for different functions
     
     tabPanel("Typical Error",
        p("This component will calculate typical error for a test-retest dataset."),
        p("The input should be a comma-separated values file (csv)."),
          sidebarLayout(
            sidebarPanel(
              fileInput(inputId = "TE_data", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
              selectInput(inputId = "var1", label = "Variable 1", choices = ""),
              selectInput(inputId = "var2", label = "Variable 2", choices = ""),
              actionButton(inputId = "updateTE", label = "Update")),
            mainPanel(
              h3("Results"),
              tableOutput(outputId="TE_table"),
              plotlyOutput(outputId = "TE_plot")
            )
      )
  ), # closes tabPanel
  
  tabPanel("Change Scores",
           p("This component will calculate change scores."),
           p("The input should be a comma-separated values file (csv)."),
           sidebarLayout(
             sidebarPanel(
               fileInput(inputId = "CS_data", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
               selectInput(inputId = "var1", label = "Variable 1", choices = ""),
               selectInput(inputId = "var2", label = "Variable 2", choices = ""),
               actionButton(inputId = "updateCS", label = "Update")), # press to run functions on server
             mainPanel(
               h3("Results")
             )
           )
  ), # closes tabPanel
  
  tabPanel("Proportion of Response",
           p("This component will calculate proportion of response."),
           p("The input should be a comma-separated values file (csv)."),
           sidebarLayout(
             sidebarPanel(
               fileInput(inputId = "PR_data", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
               selectInput(inputId = "var1", label = "Variable 1", choices = ""),
               selectInput(inputId = "var2", label = "Variable 2", choices = ""),
               actionButton(inputId = "updatePR", label = "Update")),
             mainPanel(
               h3("Results")
             )
           )
  ) # closes tabPanel
) # closes navbar

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # TE #####################
  # get data
  TE_reactive <- reactive({
    inFile <- input$TE_data
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  
  # get variables
  observe({
    updateSelectInput(session, "var1", choices = names(TE_reactive()))
    updateSelectInput(session, "var2", choices = names(TE_reactive()))
  })
  
  # on update button
  observeEvent(input$updateTE, {
    if(!is.null(input$TE_data)){
      
      df <- read.csv(input$TE_data$datapath, header = TRUE, sep = ",")
      var1 <- df[, which(colnames(df) == input$var1)]
      var2 <- df[, which(colnames(df) == input$var2)]
      dat <- data.frame(var1 = var1, var2 = var2)
      TEResult <- te(t1 = var1, t2 = var2)
      
      # Table
      rownames(TEResult) <- "Typical Error Results"
      output$TE_table <- renderTable(TEResult, rownames = TRUE)
      
      # Plot
      x <- seq(from = -4*TEResult[,2], to= 4*TEResult[,2], by = 0.1) # generate potential diff scores
      y <- dnorm(x, 0, TEResult[,2]) # remember we're assuming zero mean!
      density_df <- data.frame(x=x, y=y)
      dens_p <- ggplot(density_df, aes(x=x)) + stat_density(position="identity", geom="line") # plot of normally distributed difference scores
      dens_p <- ggplotly(dens_p)
      output$TE_plot <- renderPlotly(dens_p)
    }
}) # close observe event
  
}
# Run the application 
shinyApp(ui = ui, server = server)

