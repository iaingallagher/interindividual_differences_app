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
  title = "Statistics for Sports Science",
  sidebarLayout(
    sidebarPanel(
      img(src="swinton_2018.png", align = "center", width=400),
      p("Swinton, Paul A., Ben Stephens Hemingway, Bryan Saunders, Bruno Gualano, and Eimear Dolan. 2018. ‘A Statistical Framework to Interpret Individual Response to Intervention: Paving the Way for Personalized Nutrition and Exercise Prescription’. Frontiers in Nutrition 5. ",  a(href = "https://doi.org/10.3389/fnut.2018.00041", "https://doi.org/10.3389/fnut.2018.00041"),".")
    ),
 
  mainPanel(
    h1("Instructions"),
      p("This web app carries out calculations for typical error and generating confidence intervals for group and individual change scores as detailed in Swinton et al, 2018. There are three main components dealing with typical error, change scores and proportion of responders. The subcomponents associated with each of these are available through the tabs at the top of this page."),
    
      h2("Typical Error components"),
    
        h3("Typical error for a procedure with n>10 tests on individual(s)."),
          p("This is a rare scenario but may be useful. Select Typical Error -> Individual TE from multiple repeated measures (n>10). Select a data file containing the test data. For a group this should be a comma-separated file with test occasions as rows and individuals in columns. Enter the desired confidence level for teh true score and press the Calculate TE button. Example data is available ", a(href = 'https://github.com/iaingallagher/interindividual_response_app/blob/master/Interindividual_Responses/individual_TE_data.csv', 'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end")),
      
        h3("Typical error from group test-retest data."),
          p("This is a more usual scenario. Select a data file to upload. Select the columns that represent the test and retest data for your group. Select the required confidence level for the true score and press the Calculate TE button. Example data is available " , a(href = 'https://github.com/iaingallagher/interindividual_response_app/blob/master/Interindividual_Responses/orig_paper_data_only.csv', 'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end"), "To reproduce the muscle carnitine example from the spreadsheet accompanying the paper select MCARN_test as test and MCARN_restest as retest."),
      
        h3("Typical error from a coefficient of variation."),
          p("Sometimes there are no data available to calculate typical error and thus a robust confidence interval around a single observed score. Coefficient of variaion can be used instead. See section 1.2 of Swinton et al, 2018 for details. Simply enter a single observed score, a literature derived coefficient of variation and a desird confidence interval for the observed score and press the Calculate TE button. To reproduce the example in section 1.2 of the original paper CoV would be 4.94, the observed score would be 43.0 and the confidence level would be 0.75."),
      
        h3("Typical error from a coefficient of variation with scores from a group."),
          p("Although not detailed explicitly in the paper confidence intervals for a group obsered scores can also be generated using a literature derived CoV (see the tab labelled SF-S6. TE from CV in the spreadsheet accompanying the orginal paper). For this component simply enter a file, the desired column of observed scores, the literature derived CoV and a desired confidence interval for the observed scores. To reproduce the example from the spreadsheet accompanying the paper load the example data and select CCT110_baseline as the variable, 4.94 as the typical error and choose a confidence level."),
    
    h2("Change Score components"),
      
      h3("CI for Individual Change Score"),
        p("This component can be used to calculate confidence intervals around an observed score for a single individual. Enter the pre- and post-intervention scores, the typical error for the procedure used, the smallest worthwhile change (optional, will default to zero) and the confidence level required. To reproduce the example from the paper using muscle carnosine content enter 22.89 for the pre-score, 27.26 for post-score, 0.52 for typical error and 0.5 for desired confidence interval. These data are from subject 8 in the example file available " , a(href = 'https://github.com/iaingallagher/interindividual_response_app/blob/master/Interindividual_Responses/orig_paper_data_only.csv', 'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end")),
    
      h3("CI for Group Change Scores"),
        p("This component can be used to calculate confidence intervals around observed scores for a group of individuals. Choose a file and select the column that represent the subject identifiers, the pre- and post-intervention scores, the typical error for the procedure, the smallest worthwhile change (optional, will default to zero) and the desired confidence level. To generate 90% confidence intervals for muscle carnosine pre- and post-intervention select the example file from ", a(href = 'https://github.com/iaingallagher/interindividual_response_app/blob/master/Interindividual_Responses/orig_paper_data_only.csv', 'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end"), ", select the Participant column as subject ID, select the MCARN_baseline and MCARN_post variables as pre- and post-scores respectively, enter 0.52 as the typical error, leave the Desired SWC at 0 and enert 0.9 as the desired confidence level."),
    
      h3("Smallest Worthwhile Change"),
        p("This component will calculate the smallest worthwhile change accounting for both the typical error and a desired effect size. Select the file containing the data and the data in the file containing measurements of interest. Enert the effect size considered meaningful and the typical error for the procedure. To reproduce the CCT110 example from the paper select the example ", a(href = 'https://github.com/iaingallagher/interindividual_response_app/blob/master/Interindividual_Responses/orig_paper_data_only.csv', 'data', .noWS = "outside"), ", the CCT110_baseline column, enter 0.2 for the effect size and 2.2 for the typical error."),
    
    h2("Proportion of responders components"),
    
      h3("Intervention standrd deviation"),
        p("This component will calculate a the chnage in score variability due to the intervention. These calculations require a control group. Select a file and the variables representing the pre- and post-intervention measures in that file. Select the variable indicating the control and intervention groups and finally enter the labels for these groups. To reproduce the example in the paper select the example ", a(href = "https://github.com/iaingallagher/interindividual_response_app/blob/master/Interindividual_Responses/orig_paper_data_only.csv", "data", .noWS = "outside") , ", MCARN_baseline as the pre-intervention measures, MCARN_post as the post-intervention measures, Group as the grouping indicator and finally enter the labels for each group (B-A for intervention group and PLA for control group."),
    
      h3("Responder proportion"),
        p("This component will calculate the proportion of responders according to whether the confidence interval for an observed score overlaps with a defined smallest worthwhile change. To reporduce the example in the paper enter 10.2 as the mean change score for the intervention, 5.07 as the standard deviation due to the intervention, a desired efect size of 0.2 and leave the direction for proportion set at 'Above'.")
      )
  ),
  
  #### TYPICAL ERROR COMPONENTS ####
  
  navbarMenu("Typical Error",
      
  #### 1 - INDIVIDUAL TE METHOD ####     
  
      tabPanel("Individual TE from multiple repeated measures (n>10)",
               
        h3("Typical Error for an individual"),
        
            
          sidebarLayout(
            sidebarPanel(
              p("Enter data below to calculate typical error from individual test-retest data."),
              p("The input data should be comma-delimited with data for each individual (ideally n>10 tests) arranged in columns."),
              p("The confidence intervals from the individual TE are calculated using the t-distribution irrespective of the number of observations for each individual.s"),
              
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
    tabPanel("Test Typical Error from group test retest data",
               
      p("Enter data to calculate typical error from group Test-Retest Data."),
      p("The input data should be comma-delimited with test and retest data in two columns."),
        sidebarLayout(
          sidebarPanel(
            fileInput(inputId = "TE_data", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
            selectInput(inputId = "test", label = "Test", choices = ""),
            selectInput(inputId = "retest", label = "Retest", choices = ""),
            numericInput(inputId = "te_ci", label="CI Level", value=0.95, min=0.5,max=1, step=0.05),
                   
            actionButton(inputId = "updateTE", label = "Calculate TE")
          ), # close sidebarPanel
                 
          mainPanel(
            h3("Results"),
            p("The results are shown in table form below."),
            tableOutput(outputId="TE_table"),
            
            p("The figure below shows the theoretical distribution of difference scores. This should be centered at zero and have a standard deviation equal to the TE multiplied by the square root of 2. See Swinton, 2018 (Sect 1.1) for details."),
            plotlyOutput(outputId = "TE_plot")
          ) 
        ) 
      ),# closes tabPanel for group test retest calculations

  #### 3 - TE FROM LITERATURE ####
      tabPanel("Typical error from a literature derived coefficient of variation with individual datapoint",
               
        p("Typical Error for a score using coefficient of variation."),
        p("Enter values to calculate typical error using coefficient of variation data from literature or other sources."),
            
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
            # ADD A PLOT WITH OBS SCORE AND CI
          ) 
        )
      ),
  
  #### 4 - TE FROM LITERATURE WITH GROUP DATA ####
      tabPanel("Typical error from a literature derived coefficient of variation with group data",
           
           p("Typical Error for a score using coefficient of variation."),
           p("Enter values to calculate typical error using coefficient of variation data from literature or other sources."),
           
           sidebarLayout(
             sidebarPanel(
               fileInput(inputId = "grp_TE_CV_data", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
               selectInput(inputId = "grp_values", label = "Select test values", choices = NULL),
               numericInput(inputId = "grp_lit_cv", label="Enter CoV", value=0),
               numericInput(inputId = "grp_lit_ci", label="CI Level", value=0.95, min=0.5,max=1, step=0.05),
               actionButton(inputId = "grp_update_cov_TE", label = "Calculate TE")
             ),
             
             mainPanel(
               h3("Results"),
               tableOutput(outputId = "grp_cov_TE_table")
               # ADD A PLOT WITH SCORES AND CI
             ) 
           )
        )
    ),
  
  #### CHANGE SCORE COMPONENTS ####
  
  navbarMenu("Change Scores",
             
  #### 1 - SINGLE INDIVIDUAL CHANGE SCORE ####
             tabPanel("CI for Individual Change Score",
                      
                      h3("Individual Change Score with CI"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          p("Enter data below to calculate a change score with confidence interval for an individual."),
                          
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
                          p("Enter data below to calculate change scores with CI for a group of individuals."),
                          p("Data should be comma-delimited and include a subject ID and pre and post measures for analysis."),
                          fileInput(inputId = "GRP_CS_data", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
                          
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
  
             tabPanel("Smallest Worthwhile Change",
                      
              sidebarLayout(
                sidebarPanel(
                  
                  p("Enter data below to calculate the smallest worthwhile change."),
                  p("Data should either be a single column for one variable or comma-delimited with baseline measures of variables to choose from."),
                  
                  # read in file & enter vars, te & ci
                  fileInput(inputId = "SWC_data", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
                  
                  selectInput(input = "swc_variable", label = "Select Variable", choices = ""),
                  numericInput(input = "eff_size", label = "Enter effect size for variable", value = 0),
                  numericInput(input = "swc_te", label = "Enter TE for procedure", value = 0),
                  
                  actionButton(inputId = "calc_swc", label = "Calculate SWC")
                ),
                
                mainPanel(
                  
                  h3("Results"),
                  tableOutput(outputId="SWC_table")
                )
              )
            ) # close tabPanel
  ),
  
  
  #### RESPONDER PROPORTION COMPONENTS ####
  
  navbarMenu("Proportion of Responders",  
             
  #### 1 - INTERVENTION SD ####           
    tabPanel("Intervention standard deviation",
         
      sidebarLayout(
        sidebarPanel(
          p("Enter data below to calculate the varibility due to an intervention."),
          p("Data should be comma separated and in long format."),
             
          # read in file & enter vars
          fileInput(inputId = "int_var_data", label = "Choose a file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
             
          selectInput(input = "pre_variable", label = "Select pre intervention measures", choices = NULL),
          selectInput(input = "post_variable", label = "Select post intervention measures", choices = NULL),
          selectInput(input = "grouping_variable", label = "Select grouping variable", choices = NULL),
          textInput(input = "ctrl_ind", label = "Control group label"), 
          textInput(input = "int_ind", label = "Intervention group label"),
             
          actionButton(inputId = "calc_var", label = "Calculate")
        ),
           
        mainPanel(
             
          h3("Results"),
          p("Variability due to intervention"),
          tableOutput(outputId="Int_Var_data")
        )
      )
    ),
  
    
  #### 2 - RESPONDER PROPORTION ####
    tabPanel("Responder Proportion",
      sidebarLayout(
        sidebarPanel(
          p("Enter data below to calculate the proportion of responders."),
          
          # get required vars; prob from INTERVENTION SD above
          numericInput(input = "int_cs_mn", label = "Enter mean change score for intervention", value = 0, step = 0.1),
          numericInput(input = "int_sd", label = "Enter standard deviation due to intervention", value = 0, step = 0.1),
          numericInput(input = "prop_swc_cutoff", label = "Enter desired effect size for SWC calculation", value = 0, step=0.1),
          selectInput(input = "direction", label = "Enter direction for proportion (above/below SWC)", choices = c("Above", "Below"), selected = "Above"),
          
          actionButton(inputId = "calc_prop_resp", label = "Calculate")
          
        ),
          
        mainPanel(
          h3("Results"),
          # table
          p("Proportion of Responders"),
          tableOutput(outputId="prop_resp_table"),
          # plot
          plotlyOutput(outputId = "prop_plot")
            
        )
      )
    )
  )
)
    

#### END UI PART #### 

# SERVER LOGIC ####

server <- function(input, output, session) {
  
  # EXAMPLE DATA
  
  # INDIV TE #####
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
      if(nrow(df) == 1){
        output$indiv_TE_table <- renderText("Cannot calculate TE with one observation.")
      }
      else{
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
         } # closes if n == 1
      } # closes if file loaded
      
    }) # closes observeEvent

  
    
    
  # TEST-RETEST TE ####
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
  
  
  
  
  
  # SINGLE COV TE ####
  
  # on update button
  observeEvent(input$update_cov_TE,{
    var1 <- input$cov
    var2 <- input$obs_score
    var3 <- input$lit_te_ci
    cov_TEResult <- cov_te(cv=var1, os=var2, ci=var3)
    
    output$cov_TE_table <- renderTable(cov_TEResult, rownames = FALSE)
  })
  
  # GROUP COV TE ####
  # get data
  GRP_CV_reactive <- reactive({
    inFile <- input$grp_TE_CV_data
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  
  # get variables into R session
  observe({
    updateSelectInput(session, "grp_values", choices = names(GRP_CV_reactive()))
  })
  
  # generate output
  observeEvent(input$grp_update_cov_TE, {
    if(!is.null(input$grp_TE_CV_data)){
      
      df <- read.csv(input$grp_TE_CV_data$datapath, header = TRUE, sep = ",")
      baseline_vals <- df[, which(colnames(df) == input$grp_values)]
      
      grp_cov_TEResult <- grp_cov_te(input$grp_lit_cv, baseline_vals, input$grp_lit_ci)
      output$grp_cov_TE_table <- renderTable(grp_cov_TEResult, rownames = FALSE)
      
    }
  })
  
  
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
  GRP_CS_reactive <- reactive({
    inFile <- input$GRP_CS_data
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  
  # get pre & post scores
  observe({
    updateSelectInput(session, "id", choices = names(GRP_CS_reactive()))
    updateSelectInput(session, "multiple_pre", choices = names(GRP_CS_reactive()))
    updateSelectInput(session, "multiple_post", choices = names(GRP_CS_reactive()))
  })
  
  # on update button
  observeEvent(input$update_group_CS, {
    if(!is.null(input$GRP_CS_data)){
      
      df <- read.csv(input$GRP_CS_data$datapath, header = TRUE, sep = ",")
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
  # get the file path
  SWC_reactive <- reactive({
    inFile <- input$SWC_data
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  
  # get variable for SWC calc
  observe({
    updateSelectInput(session, "swc_variable", choices = names(SWC_reactive()))
  })
  
  # carry out the calculation & return data
  observeEvent(input$calc_swc, {
    # get data for calculation
    df <- read.csv(input$SWC_data$datapath, header = TRUE, sep = ",")
    input_var <- df[, which(colnames(df) == input$swc_variable)]
    
    # calculation & output
      var_sd <- sd(input_var)
      eff_size = input$eff_size
      te <- input$swc_te
      swc <- (var_sd * eff_size) + te
      
      swc_data <- data.frame(`Baseline SD` = var_sd, `Effect Size` = eff_size, `TE` = te, `SWC` = swc)
      output$SWC_table <- renderTable(swc_data)
  }
  )
  
  
  
  
  
  
  # INTERVENTION SD ####
  # get the file path
  PR_reactive <- reactive({
    inFile <- input$int_var_data
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  
  # get variables
  get_vars <- observe({
    updateSelectInput(session, "pre_variable", choices = names(PR_reactive()))
    updateSelectInput(session, "post_variable", choices = names(PR_reactive()))
    updateSelectInput(session, "grouping_variable", choices = names(PR_reactive()))
  })
  
  # carry out the calculation & return data
  observeEvent(input$calc_var, {
    # get data for calculation
    df <- read.csv(input$int_var_data$datapath, header = TRUE, sep = ",")
    
    # calculation
    int_data <- int_sd(df, input$pre_variable, input$post_variable, input$grouping_variable, input$ctrl_ind, input$int_ind)
    # render output
    output$Int_Var_data <- renderTable(int_data)
    
    # plot the intervention score normal distribution
    
  })

  # RESPONDER PROPORTION ####

  # on calculate button
  observeEvent(input$calc_prop_resp, {
    prop_responders <- prop_resp(mn = input$int_cs_mn, sd = input$int_sd, eff_sz = input$prop_swc_cutoff, direction = input$direction)
    colnames(prop_responders) <- c("Change Score Mean", "Intervention SD", "Responder Proportion")
    # render output
    output$prop_resp_table <- renderTable(prop_responders, rownames = FALSE)
    
    ## make plot ##
    # mean & sd
    cs_mn <- prop_responders[,1]
    intervntn_sd <- prop_responders[,2]
    prop <- prop_responders[,3]
    x <- seq(from = cs_mn-(3*intervntn_sd), to = cs_mn+(3*intervntn_sd), by = 0.1) # generate change scores
    y <- dnorm(x,cs_mn,intervntn_sd)
    # create plot
    density_df <- data.frame(x=x, y=y)
    
    # quantile cutoff
    if(input$direction == 'Above'){
      prop <- quantile(density_df$x, probs = 1-prop) # cutoff for plot
    }
    else{
      prop <- quantile(density_df$x, probs = prop) # cutoff for plot
    }
    # plot
    prop_plot <- ggplot(density_df, aes(x=x, y=y)) + geom_line() # plot of normally distributed scores
    if(input$direction == 'Above'){
      prop_plot <- prop_plot + geom_area(data = subset(density_df, x >= prop), fill = 'cadetblue', alpha = 0.5) # shade above
    }
    
    else{
      prop_plot <- prop_plot + geom_area(data = subset(density_df, x <= prop), fill = 'cadetblue', alpha = 0.5) # shade below
    }
    
    prop_plot <- prop_plot + labs(title="Distribution of change scores")
    prop_plot <- ggplotly(prop_plot)
    output$prop_plot <- renderPlotly(prop_plot)
  })



  
} # close server block

# Run the application 
shinyApp(ui = ui, server = server)