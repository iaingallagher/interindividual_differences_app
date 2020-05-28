#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    rsconnect::deployApp('Interindividual_Responses')

library(shiny)
library(plotly)
library(ggplot2)
theme_set(theme_bw())
source("interindividual_response_functions.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
  
  #### MAIN PAGE ####
  "Statistics for Sports Science",
  mainPanel(img(src='swinton_2018.png', align = "left", width=400)),
  
  p("Swinton, Paul A., Ben Stephens Hemingway, Bryan Saunders, Bruno Gualano, and Eimear Dolan. 2018. ‘A Statistical Framework to Interpret Individual Response to Intervention: Paving the Way for Personalized Nutrition and Exercise Prescription’. Frontiers in Nutrition 5. https://doi.org/10.3389/fnut.2018.00041.
"),
  
  p("Typical error can be calculated from"),
  p("i) individual test data over many trials, ii) group test retest data or iii) from a coefficient of variation."),  
  p("The calculations used by this webapp are from Swinton et al (2018) and Swinton et al (MS in prep)."),
  p("Select the method you would like to use from the menu above."),
  
  #### TYPICAL ERROR COMPONENTS ####
  
  navbarMenu("Typical Error",
      
  #### 1 - INDIVIDUAL TE METHOD ####       
      tabPanel("Individual typical error from test retest data",
               
        h3("Typical Error from Individual Test Data"),
        p("This component will calculate typical error from individual test retest data."),
        p("The input should be a comma-separated values file (csv). Data for each individual (n>10 tests) should be in columns."),
        p("The confidence intervals from the individual TE are calculated using the t-distribution irrespective of the number of observations for each individual.s"),
            
          sidebarLayout(
            sidebarPanel(
              h4("Calculate typical error from individual test retest data."),
              fileInput(inputId = "indiv_TE_data", label="Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
              numericInput(inputId = "indiv_te_ci", label="CI Level", value=0.95, min=0.5, max=1, step=0.05),
              actionButton(inputId = "update_indiv_TE", label = "Calculate TE")
            ), # closes sidebarPanel
              
            mainPanel(
              h3("Results"),
              p("The table below shows the mean for each individual over all tests and the typical error estimated for each individual."),
              p("Two confidence values are shown for the TE. The moderated confidence interval takes account of the number of tests carried out on each individual whilst the unmoderated confidence interval is wider because it does not accont for the extra data collected for each individual through the repeated tests."),
              p("The data are shown on a plot below the table."),
              tableOutput(outputId = "indiv_TE_table"),
              downloadButton("downloadData", "Download Table"),
                plotlyOutput(outputId = "indiv_TE_plot", width="50%", height="75%")
            ) # close mainPanel
          ) # close sidebarLayout
        ), # close tabPanel
      
  #### 2 - GROUP TEST RETEST DATA METHOD #### 
    tabPanel("Typical Error from group test retest data",
               
      h3("Typical Error from Group Test-Retest Data"),
      p("This component will calculate typical error of a procedure based on group test-retest data."),
      p("The input should be a comma-separated values file (csv) of test (col 1) and retest (col 2) data."),
        sidebarLayout(
          sidebarPanel(
            fileInput(inputId = "TE_data", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
            selectInput(inputId = "test", label = "Variable 1", choices = ""),
            selectInput(inputId = "retest", label = "Variable 2", choices = ""),
            numericInput(inputId = "te_ci", label="CI Level", value=0.95, min=0.5,max=1, step=0.05),
                   
            actionButton(inputId = "updateTE", label = "Calculate TE")
          ), # close sidebarPanel
                 
          mainPanel(
            h3("Results"),
            tableOutput(outputId="TE_table"),
            plotlyOutput(outputId = "TE_plot")
          ) # closes main panel
        ) # closes sidebarLayout
      ),# closes tabPanel for group test retest calculations

  #### 3 - TE FROM LITERATURE ####
      tabPanel("Typical error from a literature derived coefficient of variation",
               
        h3("Typical Error from literature data"),
        p("This component will calculate typical error from coefficient of variation data from literature or other sources."),
            
        sidebarLayout(
          sidebarPanel(
            numericInput(inputId = "obs_score", label = "Obs Score", value = 0),
            numericInput(inputId = "cov", label = "Enter CoV", value = 0),
            numericInput(inputId = "lit_te_ci", label="CI Level", value=0.95, min=0.5,max=1, step=0.05),
            actionButton(inputId = "update_cov_TE", label = "Calculate TE")
          ),
            
          mainPanel(
            h3("Results"),
            tableOutput(outputId = "cov_TE_table")
          ) # close main panel for CoV
        ) # close sidebarLayout
      ) # close CoV tabPanel
  ), # close navbarMenu
  
  
  
  #### CHANGE SCORE COMPONENTS ####
  
  navbarMenu("Change Scores",
             
             #### 1 - SINGLE INDIVIDUAL CHANGE SCORE ####
             tabPanel("CI for Individual Change Score",
                      
                      h3("Individual Change Score CI"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          numericInput(inputId = "pre", label = "Pre Score", value = 0),
                          numericInput(inputId = "post", label = "Post Score", value = 0),
                          numericInput(inputId = "te", label = "Typical Error for Procedure", value = 0),
                          numericInput(inputId = "indiv_swc", label = "Desired SWC", value = 0),
                          numericInput(inputId = "ci", label="CI Level", value=0.95, min=0.5, max=1, step=0.05),
                          
                          actionButton(inputId = "update_indiv_CS", label = "Calculate")
                        ), 
                        
                        mainPanel(
                          
                          h3("Results"),
                          tableOutput(outputId="indiv_CS_table"),
                          plotlyOutput(outputId = "indiv_CS_plot")
                          
                        ) 
                      )  
             ),
             
             
             #### 2 - GROUP OF CHANGE SCORES ####
             tabPanel("CI for Several Individual Change Scores",
                      sidebarLayout(
                        sidebarPanel(
                          # read in file & enter vars, te & ci
                          fileInput(inputId = "CS_data", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
                          
                          selectInput(input = "id", label = "Indiv ID", choices = ""),
                          selectInput(inputId = "multiple_pre", label = "Pre", choices = ""),
                          selectInput(inputId = "multiple_post", label = "Post", choices = ""),
                          numericInput(inputId = "multiple_te", label="TE for Procedure", value=0),
                          numericInput(inputId = "swc", label = "Desired SWC", value = 0),
                          numericInput(inputId = "multiple_ci", label="Desired CI", value=0.95, min=0.5,max=1, step=0.05),
                          
                          actionButton(inputId = "update_group_CS", label = "Calculate")
                        ),
                        
                        # display table of change ci's & plot
                        mainPanel(
                          h3("Results"),
                          tableOutput(outputId="group_CS_table"),
                          downloadButton("downloadData_CS", "Download Table"),
                          plotlyOutput(outputId = "group_CS_plot")
                        )
                      )
             ),
             
             #### 3 - SMALLEST WORTHWHILE CHANGE ####
             tabPanel("Smallest Worthwhile Change") # place holder
  )
)

#### END UI PART #### 

# SERVER LOGIC ####

server <- function(input, output, session) {
  
  # INDIV TE ###############
  indiv_TE_reactive <- reactive({
    inFile <- input$indiv_TE_data
    
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  }) # closes reactive
  
    # on update button
    observeEvent(input$update_indiv_TE, {
      
    if(!is.null(input$indiv_TE_data)){
      
      df <- read.csv(input$indiv_TE_data$datapath, header = TRUE, sep = ",") # dataframe in
      var <- input$indiv_te_ci # user chosen CI
      
      indiv_TEResult <- indiv_te_t(df=df, ci=var) # apply function

      # Table
      output$indiv_TE_table <- renderTable(indiv_TEResult, rownames = FALSE)
      
      # download data
      output$downloadData <- downloadHandler(
        filename = "indiv_TEResult.csv",
        content = function(file){
          write.csv(indiv_TEResult, file, row.names=FALSE) })
      
      # Plot
      indiv_te_plot <- ggplot() + geom_pointrange(data=indiv_TEResult, aes(x=ID, y=`Indiv Test Means`, ymin=`Lower CI Limit`, ymax=`Upper CI Limit`), alpha=0.2, size=1) + scale_x_discrete(limits=indiv_TEResult$ID)
      
      indiv_te_plot <- indiv_te_plot + geom_pointrange(data=indiv_TEResult, aes(x=`ID`, y=`Indiv Test Means`, ymin=`Moderated Lower CI Limit`, ymax=`Moderated Upper CI Limit`), colour='chocolate', size=1.2)
      
      indiv_te_plot <- indiv_te_plot + coord_flip()
    
      # output as plotly
      output$indiv_TE_plot <- renderPlotly(indiv_te_plot)
      } # closes if statement
      
    }) # closes observeEvent

  
    
    
  # TEST-RETEST TE #####################
  # get data
  TE_reactive <- reactive({
    inFile <- input$TE_data
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  
  # get variables into R session
  observe({
    updateSelectInput(session, "test", choices = names(TE_reactive()))
    updateSelectInput(session, "retest", choices = names(TE_reactive()))
  })
  
  # on update button
  observeEvent(input$updateTE, {
    if(!is.null(input$TE_data)){
      
      df <- read.csv(input$TE_data$datapath, header = TRUE, sep = ",")
      var1 <- df[, which(colnames(df) == input$test)]
      var2 <- df[, which(colnames(df) == input$retest)]
      var3 <- input$te_ci
      
      # dat <- data.frame(var1 = test, var2 = retest)
      TEResult <- TE(t1 = var1, t2 = var2, ci=var3)
      
      # Table
      output$TE_table <- renderTable(TEResult, rownames = TRUE)
      
      # Plot
      x <- seq(from = -3*TEResult[,2], to= 3*TEResult[,2], by = 0.1) # generate potential diff scores
      # create plot ASSUMING ZERO MEAN
      density_df <- data.frame(x=x)
      # plot
      dens_p <- ggplot(density_df, aes(x=x)) + stat_function(fun=dnorm, n=101, args=list(mean=0, sd=TEResult[,2])) # plot of normally distributed difference scores
      dens_p <- dens_p + labs(title="Distribution of difference scores")
      dens_p <- ggplotly(dens_p)
      output$TE_plot <- renderPlotly(dens_p)
    }
  }) # close observe event
  
  
  
  
  
  # COV TE ####
  
  # on update button
  observeEvent(input$update_cov_TE,{
    var1 <- input$cov
    var2 <- input$obs_score
    var3 <- input$lit_te_ci
    cov_TEResult <- cov_te(cv=var1, os=var2, ci=var3)
    
    output$cov_TE_table <- renderTable(cov_TEResult, rownames = FALSE)
  })
  
  # INDIVIDUAL CHANGE SCORES ###
  
  
  
  # INDIV CHANGE SCORES ####
    observeEvent(input$update_indiv_CS, {
      
      # create dataframe to display
      pre <- input$pre
      post <- input$post
      te <- input$te
      ci <- input$ci
      
      indiv_cs_data <- cs_ci(pre = pre, post = post, te = te, ci = ci)
      output$indiv_CS_table <- renderTable(indiv_cs_data)
      
      # if swc is zero plot a line at zero
      if(input$indiv_swc == 0){
     
      # create plot
      ci_val <- ci * 100
      ax_lab <- paste("Mean Difference +/- ", ci_val, "% CI", sep = "")
     
      indiv_cs_plot <- ggplot() + geom_pointrange(data = indiv_cs_data, aes(x = 1, y = Change, ymin=`Lower CI Limit`, ymax=`Upper CI Limit`), colour='chocolate', size=1.2) + theme(axis.ticks = element_blank(), axis.text.y = element_blank())
      indiv_cs_plot <- indiv_cs_plot + labs (x = "", y = ax_lab) + geom_hline(yintercept = 0, colour = "cadetblue", size = 1, linetype = "dashed", alpha = 0.5) + coord_flip()
     
      output$indiv_CS_plot <- renderPlotly(indiv_cs_plot)
      }
      
      # if swc != 0 plot a line at swc & zero
      else{
        
        # create plot
        ci_val <- ci * 100
        ax_lab <- paste("Mean Difference +/- ", ci_val, "% CI", sep = "")
        
        indiv_cs_plot <- ggplot() + geom_pointrange(data = indiv_cs_data, aes(x = 1, y = Change, ymin=`Lower CI Limit`, ymax=`Upper CI Limit`), colour='chocolate', size=1.2) + theme(axis.ticks = element_blank(), axis.text.y = element_blank())
        indiv_cs_plot <- indiv_cs_plot + labs (x = "", y = ax_lab) + geom_hline(yintercept = c(0,input$indiv_swc), colour = "cadetblue", size = 1, linetype = "dashed", alpha = 0.5) + coord_flip()
        
        output$indiv_CS_plot <- renderPlotly(indiv_cs_plot)
        
      }
   }) # close observe event
  
  
  

  # GROUP OF CHANGE SCORES ####
  
  # get the group data
  CS_reactive <- reactive({
    inFile <- input$CS_data
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  
  # get pre & post scores
  observe({
    updateSelectInput(session, "id", choices = names(CS_reactive()))
    updateSelectInput(session, "multiple_pre", choices = names(CS_reactive()))
    updateSelectInput(session, "multiple_post", choices = names(CS_reactive()))
  })
  
  # on update button
  observeEvent(input$update_group_CS, {
    if(!is.null(input$CS_data)){
      
      df <- read.csv(input$CS_data$datapath, header = TRUE, sep = ",")
      ids <- df[, which(colnames(df) == input$id)]
      ids <- paste('Subject:', ids, sep = ' ')
      pre <- df[, which(colnames(df) == input$multiple_pre)]
      post <- df[, which(colnames(df) == input$multiple_post)]
      te <- input$multiple_te
      ci <- input$multiple_ci
      
      CSResult <- cs_ci(pre, post, te, ci)
      CSResult <- cbind(ids, CSResult) # add subj ids
      colnames(CSResult)[1] <- 'ID'
      # Table for output
      output$group_CS_table <- renderTable(CSResult)
      
      # download data
      output$downloadData_CS <- downloadHandler(
        filename = "Group_Change_Score_Result.csv",
        content = function(file){
          write.csv(CSResult, file, row.names=FALSE) })
      
      
      # Plot
      if(input$swc == 0){
        group_CS_plot <- ggplot() + geom_pointrange(data = CSResult, aes(x = ids, y=`Change`, ymin=`Lower CI Limit`, ymax=`Upper CI Limit`), size=2, colour = "chocolate2") # basic plot
        group_CS_plot <- group_CS_plot + scale_x_discrete(limits = CSResult$ID) # subj labels
        group_CS_plot <- group_CS_plot + geom_hline(yintercept = 0, colour = "cadetblue", size = 1, linetype = "dashed", alpha = 0.5) + coord_flip() # swc and flip axes
        output$group_CS_plot <- renderPlotly(group_CS_plot)
      }
      
      else{
        group_CS_plot <- ggplot() + geom_pointrange(data = CSResult, aes(x = ids, y=`Change`, ymin=`Lower CI Limit`, ymax=`Upper CI Limit`), size=2, colour = "chocolate2") # basic plot
        group_CS_plot <- group_CS_plot + scale_x_discrete(limits = CSResult$ID) # subj labels
        group_CS_plot <- group_CS_plot + geom_hline(yintercept = c(0, input$swc), colour = "cadetblue", size = 1, linetype = "dashed", alpha = 0.5) + coord_flip() # swc and flip axes
        output$group_CS_plot <- renderPlotly(group_CS_plot)
      }
      
    } # closes if statement for data
  }) # close observe event
  
  
  # SWC ####
  # placeholder
  
  
} # close server block

# Run the application 
shinyApp(ui = ui, server = server)