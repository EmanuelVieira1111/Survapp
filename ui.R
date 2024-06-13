source('global.R')
source("about.R")
# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    tags$script(src = "https://kit.fontawesome.com/<you>.js"),
    setBackgroundColor(
      color = c("#9bb7d4","white"),
      #   #  c("#F7FBFF", "#2171B5"),
      gradient = "linear",
      direction = c("top", "left")
    ),
    theme = shinytheme("sandstone")
    ,
    navbarPage(
      #  navlistPanel(
      "Survapp", 
      ui_about_module("about") #Call about module (contains the explanation of survapp that will be see by de user)
      ,
      
       
      tabPanel("Data.file", icon = icon("file", lib = "glyphicon"),
    
              add_busy_spinner(spin = "fading-circle"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("down_example", label = "Select an example dataset or upload your own",
                              choices = c("Example dataset" = "Example2", "Load my CSV file" = "load_my_own2"),
                             selected = "load_my_own2"),
                conditionalPanel("input.down_example == 'load_my_own2'",
                                fileInput("fileIn", "Choose CSV File",
                                           multiple = FALSE,
                                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                 tags$hr(),
                                   checkboxInput("header", "Header", TRUE),
                                   radioButtons("sep", "Separator",
                                                choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                               selected = ","),
                                 radioButtons("quote", "Quote",
                                             choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                                            selected = '"'),
                              uiOutput("colname_in"),
                             selectInput(inputId = "class",
                                        label = "Choose the class to change variable type",
                                       choices = c("", "factor", "numeric", "integer", "character"),
                                      selected = ""),
                         actionButton("change_class", "Change class")
       ),
        conditionalPanel("input.down_example == 'Example2'",
                        selectInput("examples", label = "Select an example dataset for survival analysis",
                                     choices = c("GBSG", "IR_diabetes", "Melanoma")),
                         p("GBSG has right censoring", style = "color:black"),
                         p("IR_diabetes has interval censorship", style = "color:black"),
                         p("Melanoma for competing risks analysis", style = "color:black"),
                        downloadButton("downloadData", "Download the data")
      )
    ),
       mainPanel(
          tabsetPanel(
            tabPanel("Data",
                       fluidRow(DT::dataTableOutput("tabela")),
                       fluidRow(verbatimTextOutput("str_dados"))
              ),
              tabPanel("Data summary", fluidRow(uiOutput('summaryTable')))
            )
          )
        )
      )
      ,
      navbarMenu("Survival analysis", icon = icon("heartbeat", lib = "font-awesome"),
                 tabPanel("Survival Curves", add_busy_spinner(spin = "fading-circle"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("censor", "Choose the type of censorship: right until random forests and interval until AFT model",
                                          choices = c("right", "interval")),
                              selectInput("kaplan_estra", label = "Select Kaplan-Meier (and Turnbull) or stratified Kaplan-Meier", 
                                          choices = c("Kaplan-Meier", "Stratified Kaplan-Meier"), selected = "Kaplan-Meier"),
                              conditionalPanel("input.down_example == 'load_my_own2'",
                                               conditionalPanel(condition = "input.censor == 'right'",
                                                                selectInput("variable1", "Select the survival time variable", character(0)),
                                                                selectInput("variable2", "Select the survival event variable", character(0)),
                                                                selectInput("variable3", "select a qualitative covariate", character(0)),
                                                                h5("If the selected covariate has more than 2 levels, you can obtain the respective clusters in 'Clusters of survival curves'")
                                               ),
                                               conditionalPanel(condition = "input.censor == 'interval'",
                                                                selectInput("variable4", "Select the survival time 1 variable", character(0)),
                                                                selectInput("variable5", "Select the survival time 2 variable", character(0)),
                                                                selectInput("variable6", "Select the covariate", character(0)),
                                                                h5("If the selected covariate has more than 2 levels, you can obtain the respective clusters in 'Clusters of survival curves'")
                                               )
                              ),
                              conditionalPanel("input.down_example == 'Example2'",
                                               conditionalPanel(condition = "input.examples == 'GBSG'",
                                                                selectInput("var1gbsg", "Select the survival time variable", character(0)),
                                                                selectInput("var2gbsg", "Select the survival event variable", character(0)),
                                                                selectInput("var3gbsg", "select a qualitative covariate", character(0))
                                               ),
                                               conditionalPanel(condition = "input.examples == 'IR_diabetes'",
                                                                selectInput("var1diabetes", "Select the survival time 1 variable", character(0)),
                                                                selectInput("var2diabetes", "Select the survival time 2 variable", character(0)),
                                                                selectInput("var3diabetes", "select a qualitative covariate", character(0))
                                               )
                              ),
                              numericInput("num", label = "Choose the estimation time", value = ""),
                              actionButton("kaplan", "Click to do Kaplan-Meier (or Turnbull) analysis"),
                              radioButtons(inputId = "choose", label = "Select the file type for Kaplan-Meier plot", choices = list("png", "pdf")),
                              downloadButton(outputId = "down", label = "Download the Kaplan-Meier (or Turnbull) plot"),
                              radioButtons(inputId = "choose1", label = "Select the file type for cumulative risk function plot", choices = list("png", "pdf")),
                              downloadButton(outputId = "down1", label = "Download the cumulative risk function plot"),
                              p(" "),
                              actionButton("compara", "Click to compare survival curves"),
                              radioButtons(inputId = "choose2", label = "Select the file type of Kaplan-Meier curves plot", choices = list("png", "pdf")),
                              downloadButton(outputId = "down2", label = "Download the Kaplan-Meier (or Turnbull) curves plot")
                            ),
                            mainPanel(
                              conditionalPanel(condition = "input.kaplan_estra == 'Kaplan-Meier'",
                                               tabsetPanel(
                                                 tabPanel("Kaplan-Meier", fluidRow(column(12, plotlyOutput("graf"))),
                                                          fluidRow(column(12, verbatimTextOutput("printkm"))),
                                                          fluidRow(column(12, h4("Summary for a specific time"), verbatimTextOutput("timesum"))))
                                               ,
                                               
                                                 tabPanel("Summary Survfit", fluidRow(column(12, verbatimTextOutput("summary1"))))
                                               ,
                                               
                                                 tabPanel("Cumulative risk function", plotlyOutput("suma"))
                                               
                              )),
                              conditionalPanel(condition = "input.kaplan_estra == 'Stratified Kaplan-Meier'",
                                               tabsetPanel(
                                                 tabPanel("Plot and tests", fluidRow(column(12, plotlyOutput("curves"))),
                                                         tabsetPanel(
                                                            tabPanel("Log-rank test", fluidRow(column(12, verbatimTextOutput("test")))),
                                                            tabPanel("Gehan-Wilcoxon test", fluidRow(column(12, verbatimTextOutput("testgehan")))),
                                                            tabPanel("Print of Survfit", fluidRow(column(12, verbatimTextOutput("print"))))
                                                          )
                                                 ),
                                                 tabPanel("Summary of Survfit", fluidRow(column(12, verbatimTextOutput("summary2"))))
                                               )
                              )
                            )
                          )
                 ),
      tabPanel("Clusters of survival curves", add_busy_spinner(spin = "fading-circle"),
               sidebarLayout(
                 sidebarPanel(
                   actionButton("cluster_page", "Start calculating"),
                   h5("This can take a few minutes."),
                   h5("To avoid overloading the application, please wait for the results to compile before navigating to another menu."),
                   radioButtons(inputId = "clust", label = "Select the file type", choices = list("png", "pdf")),
                   downloadButton(outputId = "down_clust", label = "Download the plot")
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Summary", verbatimTextOutput("cluster")),
                     tabPanel("Plot", plotOutput("clus"))
                   )
                 )
               )
      ),
    tabPanel("Cox PH Model", add_busy_spinner(spin = "fading-circle"),
             sidebarLayout(
               sidebarPanel(
                 actionButton("cox_page", "Do the analysis"),
                 conditionalPanel(condition = "input.down_example == 'load_my_own2'",
                                  checkboxGroupInput("cox_var", "Select the covariates")),
                 conditionalPanel(condition = "input.down_example == 'Example2'",
                                  conditionalPanel(condition = "input.examples == 'GBSG'",
                                                   checkboxGroupInput("cox_var_gbsg", "Select the covariates")),
                                  conditionalPanel(condition = "input.examples == 'IR_diabetes'",
                                                   checkboxGroupInput("cox_var_diabetes", "Select the covariate")
                                  )
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Model", fluidRow(column(12, verbatimTextOutput("cox"))),
                            fluidRow(column(12, verbatimTextOutput("aic_cox")))),
                   tabPanel("Graphical Test of Proportional Hazards", fluidRow(plotOutput("coxzph")))
                 )
               )
             )
    ),
tabPanel("AFT Model", add_busy_spinner(spin = "fading-circle"),
         sidebarLayout(
           sidebarPanel(
             conditionalPanel(condition = "input.down_example == 'load_my_own2'",
                              checkboxGroupInput("distribution", "Select the distribution"),
                              checkboxGroupInput("aft_variables", "Select the covariates")),
             conditionalPanel(condition = "input.down_example == 'Example2'",
                              conditionalPanel(condition = "input.examples == 'GBSG'",
                                               checkboxGroupInput("distribution_gbsg", "Select the distribution"),
                                               checkboxGroupInput("aft_var_gbsg", "Select the covariates")),
                              conditionalPanel(condition = "input.examples == 'IR_diabetes'",
                                               checkboxGroupInput("distribution_diabetes", "Select the distribution"),
                                               checkboxGroupInput("aft_var_diabetes", "Select the covariate")
                              )
             )
           ),
           mainPanel(
             fluidRow(column(12, verbatimTextOutput("aft_model")))
           )
         )
),
tabPanel("Regression Trees", add_busy_spinner(spin = "fading-circle"),
         sidebarLayout(
           sidebarPanel(
             conditionalPanel(condition = "input.down_example == 'load_my_own2'",
                              checkboxGroupInput("rpart_var", "Select the covariates")),
             conditionalPanel(condition = "input.down_example == 'Example2'",
                              conditionalPanel(condition = "input.examples == 'GBSG'",
                                               checkboxGroupInput("rpart_var_gbsg", "Select the covariates"))
             ),
             actionButton("page_rpart_reg", "Do the analysis")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Plot and print", fluidRow(column(12, plotOutput("rpart_regression"))),
                        fluidRow(column(12, verbatimTextOutput("rpart_regression_print")))),
               tabPanel("Summary", fluidRow(column(12, verbatimTextOutput("rpart_regression_summary"))))
             )
           )
         )
),
tabPanel("Random Forests", add_busy_spinner(spin = "fading-circle"),
         sidebarLayout(
           sidebarPanel(
             conditionalPanel(condition = "input.down_example == 'load_my_own2'",
                              checkboxGroupInput("varforests", "Select the covariates")),
             conditionalPanel(condition = "input.down_example == 'Example2'",
                              checkboxGroupInput("gbsgforests", "Select the covariates")),
             actionButton("page_forest", "Do the analysis")
           ),
           mainPanel(
             fluidRow(column(12, plotOutput("forestplot"), verbatimTextOutput("foresterror"), h5("Variable importance", style = "color:steelblue"), verbatimTextOutput("forestimp")))
           )
         )
),
tabPanel("Cumulative incidence", add_busy_spinner(spin = "fading-circle"),
         sidebarLayout(
           sidebarPanel(
             selectInput("indi_group", label = "Select cumulative incidence or cumulative incidence by group", 
                         choices = c("Cumulative incidence", "Cumulative incidence by group"), selected = "Cumulative incidence"),
             conditionalPanel(condition = "input.down_example == 'load_my_own2'",
                              selectInput("compe_var1", "Select the survival time variable", character(0)),
                              selectInput("compe_var2", "Select the survival event variable", character(0)),
                              selectInput("compe_var3", "Select the covariate from the imported database", character(0))),
             conditionalPanel(condition = "input.down_example == 'Example2'",
                              selectInput("melanoma_var1", "Select the time variable from Melanoma", character(0)),
                              selectInput("melanoma_var2", "Select the event variable from Melanoma", character(0)),
                              selectInput("melanoma_var3", "Select the covariate from Melanoma", character(0))
             ),
             numericInput("late", label = "Write the status level that represents censorship", value = ""),
             actionButton("page_inci", "Do the analysis for cumulative incidence"),
             radioButtons(inputId = "incitype1", label = "Select the file type for cumulative incidence plot", choices = list("png", "pdf")),
             downloadButton(outputId = "inci_down1", label = "Download the cumulative incidence plot"),
             actionButton("page_inci_group", "Do the analysis for cumulative incidence by group"),
             radioButtons(inputId = "incitype2", label = "Select the file type for cumulative incidence plot by group", choices = list("png", "pdf")),
             downloadButton(outputId = "inci_down2", label = "Download the cumulative incidence by group plot")
           ),
           mainPanel(
             conditionalPanel(condition = "input.indi_group == 'Cumulative incidence'",
                              tabsetPanel(
                                tabPanel("Cumulative incidence", plotOutput("comu_inci")),
                                tabPanel("Estimates and Variances", verbatimTextOutput("print_inci"))
                              )
             ),
             conditionalPanel(condition = "input.indi_group == 'Cumulative incidence by group'",
                              tabsetPanel(
                                tabPanel("Cumulative incidence by group", plotOutput("comu_inci_group")),
                                tabPanel("Tests Estimates and Variances", verbatimTextOutput("print_inci_group"))
                              )
             )
           )
         )
),
tabPanel("Competing risks regression", add_busy_spinner(spin = "fading-circle"),
         sidebarLayout(
           sidebarPanel(
             selectInput("regre", label = "Select the approach", choices = c("Subdistribution hazard approach" = "sub_haz", "Cause-specific hazards" = "speci_haz")),
             conditionalPanel(condition = "input.down_example == 'load_my_own2'",
                              checkboxGroupInput("compe_reg", "Select the covariates")),
             conditionalPanel(condition = "input.down_example == 'Example2'",
                              checkboxGroupInput("compe_reg_melanoma", "Select the covariates from Melanoma")
             ),
             conditionalPanel(condition = "input.regre == 'sub_haz'",
                              numericInput("cenlevel", label = "For subdistribution hazard approach choose censor level", value = ""),
                              numericInput("failcode", label = "Write the level of the variable of interest for which you want the results", value = "")
             ),
             conditionalPanel(condition = "input.regre == 'speci_haz'",
                              numericInput("text", label = "For cause-specific hazards write the status level", value = "")
             )
           ),
           mainPanel(
             verbatimTextOutput("risk_reg")
           )
         )
)
)
)
)