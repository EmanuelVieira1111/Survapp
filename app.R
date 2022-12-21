library(shiny)
library(summarytools)
library(DT)
library(ggplot2)
library(dplyr)
library(broom)
library(shinydashboard)
library(leaflet)
library(shinythemes)
library(plotly)
library(survival)
library(survminer)
library(vtable)
library(smoothHR)
library(grDevices)
library(periscope)
library(ggfortify)
library(coxinterval)
library(smoothHR)
library(rpart)
library(plotmo)
library(rpart.plot)
library(icenReg)
library(cmprsk)
library(clustcurv)
library(mvna)
library(mstate)
library(tree)
library(caret)
library(htmltools)
library(ranger)
library(MASS)
library(shinybusy)
library(shinyjs)
library(survidm)
library(shinyWidgets)
library(htmlwidgets)
library(htmltools)
#library(shinyauthr)
library(ggthemes)




data("IR_diabetes", package = "icenReg")
data(Melanoma, package = "MASS")
data("cancer", package = "survival")



ui <- fluidPage(
  tags$script(src = "https://kit.fontawesome.com/<you>.js"),
  


  setBackgroundColor(
    color = c("#9bb7d4","white"),	 
 #   #  c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = c("top", "left")
  ),
                  theme = shinytheme("sandstone"),
  

navbarPage("Survapp", 
           tabPanel("About",
            sidebarLayout(sidebarPanel(
          img(src = "ECUM.png"),
              
          br(),
                  
          br(),
                   
          p("Survapp was developed within the scope of the Master's
          dissertation in Statistics for Data Science at the University of Minho,
                    Portugal by", a(href= "https://www.linkedin.com/in/emanuel-vieira-3b519a177/" ,"Emanuel Vieira Monteiro da Silva"),
            "under the supervision of Professor" , a(href = "https://w3.math.uminho.pt/~lmachado/", "Luis Filipe Meira Machado"), ".",style = "color:black"),
                   
         br(),
                  
                   
                  #tags$video(src = "movie.mp4", width = "300px",height="100px", type = "video/mp4", control = "controls"),
                   
         p("This is a shiny application developed using RStudio, a free software integrated development environment for R.",style = "color:black"),
                                                             
         br(),
                   
        
        img(src = "posit.jpg",height = 70, width = 150),
                   
       br(),
                   
         p("Shiny and Rstudio are a product of ", 
                   
         span("posit", style = "color:blue"), style = "color:black")
                   
                   ),
                                                               
          mainPanel(
                                               
                                # tabPanel("pdf",
                                #               tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                                #                           src="multi-state.pdf")),
                                               #      tabPanel("Video",uiOutput("video")),
                                               tabPanel("Abstract",
                                           radioButtons("aboutmenu", label = h4("Choose the descriptive menu",style = "color:black"), 
                                                             
                                                                                          choices = c("Data.file",
                                                                                                           "Survival analysis")
                                                                                                           
                                                                                                             
                                                                                                             
                                                                                          ),
                                               
                            conditionalPanel("input.aboutmenu == 'Data.file'",
                                                                      
                                                        h4("Data.file",style = "color:steelblue"),
                   
                                                p("The data.file menu allows the user to import their respective database,
                                                              specifying if there is a header, which is the separator and quote, or they can use the example
                                                               databases contained in the application. Also in this section, it is possible
                                                               to visualize the data in table format, the summary and its structure.
                                                               The user can change the class of variables and choose whether the database
                                                               has right or interval censoring. It is also possible to download the examples databases.",style = "color:black"),
                                                             
                                                             p("For an analysis with only one type of event of interest, the application enables
                                                              both the classical approach and a machine learning approach. For situations with more than 2 failures from multiple causes.", style = "color:black"),         
                                                 
                                                             
                                                             p("By default, the menu considers right censoring but users can also consider
                                                               the option of interval censoring. This menu can also be used to change the
                                                               class of some variables. Qualitative variables should be considered as 
                                                               'factor'.",style = "color:black" )),
                        
                                                 conditionalPanel("input.aboutmenu == 'Survival analysis'",
                                                                  
                                          
                                                            
                                                      
                                      
                                               h4("Kaplan-Meier",style = "color:steelblue"),
                                                           #                                   
                                               p("The Kaplan-Meier estimator (Kaplan and Meier, 1958), is the standard method
                                                             used to estimate the survival function from lifetime data in the presence of
                                                             right censoring. In medical research, it is often used to measure the fraction
                                                             of patients living for a certain amount of time after enrolment in study.
                                                             In other fields, this estimator can be used to measure the length of time people
                                                             remain unemployed after a job loss, the time-to-failure of machine parts, etc. ",style = "color:black" ),  
                                                             
                                                           p("Kaplan, E. L. and Meier, P. (1958). Nonparametric estimation from incomplete
                                                             observations. J. Amer. Statist. Assoc. 53 (282): 457-481.",style = "color:black" ),
                                                         
                                               p("The first step to performing a survival analysis is to specify the variables of interest.
                                         This is done in the 'Kaplan-Meier' menu, in which the user must always indicate:
                                                                time-to-event variable and the corresponding indicator status (for right censoring);
                                       left time and right time (for interval censoring).",style = "color:black" ),
                                                         
                                                          p("For data with interval censoring the app will implement the Turnbull estimator."),
                                                          
                                                         p("Bruce W. Turnbull (1976) The Empirical Distribution Function with Arbitrarily Grouped,
                                                           Censored and Truncated Data. Journal of the Royal Statistical Society. Series B
                                                           (Methodological). Vol. 38, No. 3 (1976), pp. 290-295.",style = "color:black" ),
                                                         
                                                           h4("Comparison of survival curves (stratified Kaplan-Meier)",style = "color:steelblue"),
                                                           
                                                            
                                                        
                                                           p("Survival curves can be estimated for each group, considered separately, using the Kaplan-Meier
                                                             method, but they can also be compared statistically using the log rank test or
                                                              the Gehan-Wilcoxon test. These tests compare the entire survival experience
                                                              between groups and can be thought of as a test of whether the survival curves
                                                              are identical (the null hypothesis) or not. The log-rank method is the
                                                              traditional approach, whereas the Gehan-Wilcoxon test might be a good option
                                                              in situations that are more sensitive to nonproportional hazard.",style = "color:black" ),
                                                            
                                                            p("Here the user must choose a qualitative covariate to compare the different
                                                              curves among the groups of the covariate. Different tests are carried out
                                                              to test if there are significant differences.",style = "color:black" ),
                                                             
                                               h4("Clusters of Survival Curves", style = "color:steelblue"),
                                               
                                               p("For more than two curvival curves"),
                                               
                                               
                                               
                                                             h4("Cox PH Model",style = "color:steelblue"),
                                                             
                                                            p("The Cox proportional-hazards model (Cox, 1972) is the regression model
                                                               commonly used in medical research for investigating the association between
                                                               survival and one or more predictor variables. It assumes proportional hazards.",style = "color:black" ),
                                                             
                                                            p("The Cox model allows us to examine how specified factors influence the rate of
                                                              a particular event at a particular point in time. This rate is commonly referred
                                                              as the hazard rate. The Cox model is expressed by the hazard function denoted by
                                                              ",withMathJax("$$h(t)$$"),"Briefly, the hazard function can be interpreted as the risk of experiencing
                                                             the even at time t. It can be estimated as follow:",style = "color:black" ),
                                                            
                                                             p(withMathJax("$$h(t|x)=h_0(t)*exp(b_1x_1+b_2x_2+b_px_p)$$"),style = "color:black" ),
                                                            
                                                           p("The hazard function is determined by a set of p covariates", withMathJax("$$x1,x2,...,xp$$"),
                                                              "where the coefficients",withMathJax("$$b1,b2,...,bp$$"),"measure the impact (i.e., the effect size)
                                                              of covariates. The quantities exp(bi) are called hazard ratios (HR). A value of
                                                             a bi greater than zero, or equivalently a hazard ratio greater than one, indicates that as the value of
                                                             the ith covariate increases, the event hazard increases and thus the length of survival decreases.",style = "color:black" ),
                                                          
                                                            p("In this menu, the user chooses the covariates to be included in the Cox model.
                                                              It is also possible to see the graphs generated to test assumptions.",style = "color:black" ),
                                                           
                                                            p("Cox DR (1972). Regression models and life tables (with discussion).
                                                              J R Statist Soc B 34: 187-220",style = "color:black" ),
                                                            
                                                           h4("AFT Model",style = "color:steelblue"),
                                                            
                                                            p("An accelerated failure time model (AFT model) is a parametric model
                                                             that provides an alternative to the commonly used Cox proportional hazards
                                                             model. Whereas a proportional hazards model assumes that the effect of a
                                                             covariate is to multiply the hazard by some constant, an AFT model assumes
                                                             that the effect of a covariate is to accelerate or decelerate the life course
                                                              of a disease by some constant.",style = "color:black" ),
                                                 
                                                           p("In this menu, the user must specify the distribution he wants to use and the
                                                             covariates to be included in the accelerated failure time (AFT) model.
                                                             The final model and the distribution can be chosen based on the AIC
                                                          (Akaike's Information Criterium), for which lower values are preferred.",style = "color:black" ),
                                               
                                               h4("Classification Trees",style = "color:steelblue"),
                                               
                                               p("A single tree is built. A final pruned tree selected by cross-validation is
                                                               obtained. The splits are based on the covariates selected. Not very useful for survival analysis because don't use a survival object as response",style = "color:black" ),
                                               
                                              
                                               
                                               
                                               
                                               h4("Regression Trees",style = "color:steelblue"),
                                               
                                               p("In some cases, collected data is high-dimensional, heterogeneous,
                                                               and contains missing information. Those cases present challenges that
                                                               may be difficult to consider when considering traditional statistical analysis.
                                                               Machine learning models can be a good option for those situations. These models
                                                               can be used to predict survival time as well as to evaluate risks. ",style = "color:black" ),
                                               
                                               p("The Machine learning approach menu aims to apply some methodologies
                                                               in machine learning for the analysis of survival data.",style = "color:black" ),
                                               
                                              
                                               
                                               p("A single tree is built using the default settings in the rpart package
                                                               from the R software. A final pruned tree selected by cross-validation is
                                                               obtained. The splits are based on the covariates selected. For each terminal
                                                               node the Kaplan-Meier estimator is applied.",style = "color:black" ),
                                               
                                               p("Breiman, L., Friedman, J., Ohlsen, R., and Stone, C. (1984),
                                                               'Classification and regression trees'",style = "color:black" ),
                                               
                                               
                                               
                                               
                                               h4("Random Forests",style = "color:steelblue"),
                                               
                                               
                                               p("Random forest is used to obtain prediction of the survival curves.
                                                              It aggregates many regression trees. This method can
                                                             also be used to study the importance of the covariates selected by the used.",style = "color:black" ),
                                               
                                               p("Breiman, L. (2001), 'Random forests', Machine learning, 45(1), 5-32.",style = "color:black" ),
                                               
                                               h4("Competing risk analysis",style = "color:steelblue"),
                                               
                                               p("In some survival studies, patients may experience an event
                                                               other than the one of interest, which alters the probability
                                                               of experiencing the event of interest. Such events are known as 'competing risk
                                                               events'. Therefore, competing risk analysis refers to a special type of survival
                                                               analysis that aims to correctly estimate the marginal probability of an event in
                                                               the presence of competing events.",style = "color:black" ),
                                               
                                               h5("Cumulative incidence",style = "color:steelblue"),
                                               
                                               p("In competing risk analysis, one important goal is to calculate the
                                                              cumulative incidence of a specific event of interest. These can be obtained
                                                               through the estimation of the cumulative incidence functions. ",style = "color:black" ),
                                               
                                               p("Users must choose the variables referring to time and status
                                                              (0 - censored; k - Failure from cause K)",style = "color:black" ),
                                               
                                               p("Kalbfleisch JD, Prentice RL (1980) The Statistical Analysis of
                                                               Failure Time Data. New York: John Wiley and Sons",style = "color:black" ),
                                               
                                               h5("Cumulative incidence between groups",style = "color:steelblue"),
                                               
                                               p("One can perform a statistical test to compare the cumulative incidence between groups. 
                                                               Accordingly, users must choose de (qualitative) covariate.",style = "color:black" ),
                                               
                                               p("Gray RJ (1988) A class of k-sample tests for comparing the cumulative incidence of
                                                               a competing risk. Ann Stat 16: 1141 - 1154",style = "color:black" ),
                                               
                                               h5("Competing Risks regression",style = "color:steelblue"),
                                               
                                               p("Two types of analysis can be performed to analyze the event times
                                                               in a competing risk setting. One set up assumes only one event time
                                                               for each subject, with the event occurring due to one cause, while the
                                                               event times due to other causes are undefined. The key functions that arise
                                                               from this approach are the subdistribution hazard. A second approach assumes
                                                               that there exists a latent (possibly unobserved) event time, one for every
                                                               cause or type of event, but only the minimum of the latent event times is
                                                               observed. Under this approach, subjects that observe an event for a
                                                               particular cause are treated as censored in other competing events.
                                                               The use of a cause-specific hazard model based on the Cox proportional
                                                               hazard model, one for each event, is often used in this setting, which
                                                               assumes independence of each of the latent failure times.",style = "color:black" ),
                                               
                                               p("Fine JP, Gray RJ (1999) A proportional hazards model for the sub-distribution
                                                               of a competing risk. J Am Stat Assoc 94: 496509",style = "color:black" ))
                                               
                                               
                                               
                                                             
                                                
                                                                  
                           
                                          
                                                            
                                                 
                                                 
                            )))),
                                                                  
                                                                  
         
                   
                 
                   tabPanel("Data.file",icon = icon("file", lib = "glyphicon"),
                        #    tags$div(
                        #      tags$i(class = "fa-solid fa-user"),
                        #      tags$span("Users")),
                            
                            
                            add_busy_spinner(spin = "fading-circle"),
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                   
                     selectInput("down_example",label= "Select an example dataset or upload your own", 
                               choices = c("Example dataset"="Example2", "Load my CSV file" = "load_my_own2"), selected = "load_my_own2"),
                   
                     conditionalPanel("input.down_example == 'load_my_own2'",
                   
                                      fileInput("fileIn", "Choose CSV File",
                                                               multiple = FALSE,
                                                               accept = c("text/csv",
                                                                          "text/comma-separated-values,text/plain",
                                                                          ".csv")),
                                                     tags$hr(),
                                                     
                                                     
                                                     checkboxInput("header", "Header", TRUE),
                                      
                                                     radioButtons("sep", "Separator",
                                                                  choices = c(Comma = ",",
                                                                              Semicolon = ";",
                                                                              Tab = "\t"),
                                                                  selected = ","),
                                      
                                                     radioButtons("quote", "Quote",
                                                                  choices = c(None = "",
                                                                              "Double Quote" = '"',
                                                                              "Single Quote" = "'"),
                                                                  selected = '"'),
                     
                     
                     uiOutput("colname_in"),
                     
                     selectInput(inputId = "class",
                                 label = "Choose the class to change variable type",
                                 choices = c("", "factor", "numeric", "integer", "character"),
                                 selected = ""),
                     
                     actionButton("change_class","Change class")
                     
                     
                     ),
                     
                     
                   
                     conditionalPanel("input.down_example == 'Example2'",                                   
                                    selectInput("examples",label= "Select an example dataset for survival analysis", 
                                                choices = c("GBSG", "IR_diabetes","Melanoma")),
                                    
                                     p("GBSG has censorship on the right",style = "color:black"),
                                     p("IR_diabetes has interval censorship",style = "color:black"),
                                     p("Melanoma for competing risks analysis",style = "color:black"),
                                     
                                    
                                    downloadButton("downloadData", "Download the data"))
                   
                   
                                                     
                                                   
                                                     
                                                    )
                                                                                                                 
                 ,mainPanel(
                   
                   tabsetPanel(
                  
                    tabPanel("Data",
                             
                             fluidRow(DT::dataTableOutput("tabela")),
                             
                             fluidRow(verbatimTextOutput("str_dados"))),
                             
                               tabPanel("Data summary", fluidRow(uiOutput('summaryTable')))))
)),  
                
               navbarMenu("Survival analysis",icon = icon("heartbeat", lib = "font-awesome"),
                          
                          tabPanel("Kaplan-Meier", add_busy_spinner(spin = "fading-circle"),
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel( 
                                       
                                       selectInput("censor", "Choose the type of censorship: right until random forests and interval until AFT model",
                                                   choices = c("right", "interval")),
                                       
                                       selectInput("kaplan_estra",label= "Select Kaplan-Meier or stratified Kaplan-Meier", 
                                                   choices = c("Kaplan-Meier", "Stratified Kaplan-Meier"), selected = "Kaplan-Meier"),
                                       
                                       conditionalPanel("input.down_example == 'load_my_own2'",
                            
                              conditionalPanel(condition = "input.censor == 'right'",
                                                                                     
                               selectInput("variable1", "Select the survival time variable", character(0)),
               
                               selectInput("variable2", "Select the survival event variable", character(0)),
                               
                               selectInput("variable3", "select a qualitative covariate", character(0)),
                               h5("If the selected covariate have more than 2 levels the user can obtain the respective clusters in 'Clusters of survival curves'")
                               ),
              
                             conditionalPanel(condition = "input.censor == 'interval'",
                            
                                                  selectInput("variable4", "Select the survival time 1 variable", character(0)),
                                
                                              selectInput("variable5", "Select the survival time 2 variable", character(0)),
                                              
                                              
                                              selectInput("variable6", "Select the covariate", character(0)),
                                              h5("If the selected covariate have more than 2 levels the user can obtain the respective clusters in 'Clusters of survival curves'")
               
                                              )), conditionalPanel(condition = "input.down_example == 'Example2'",
                                    
                                    conditionalPanel(condition = "input.examples == 'GBSG'",
                                    
                                                      selectInput("var1gbsg", "Select the survival time variable", character(0)),
                                                     
                                                     selectInput("var2gbsg", "Select the survival event variable", character(0)),
                                                     
                                                     selectInput("var3gbsg", "select a qualitative covariate", character(0))
                                                     ),
                                    
                                    conditionalPanel(condition = "input.examples == 'IR_diabetes'",
                                                     
                                                     selectInput("var1diabetes", "Select the survival time 1 variable", character(0)),
                                                     
                                                     selectInput("var2diabetes", "Select the survival time 2 variable", character(0)),
                                                     
                                                     selectInput("var3diabetes", "select a qualitative covariate", character(0))
                                                     )),
               
                  p("This is the only time it is necessary to select the variables referring to the time and event of interest",style = "color:black"),
              
                numericInput("num", label = "Choose the estimation time", value = ""),
                
                actionButton("kaplan", "Click to do Kaplan-Meier analysis"),
              
               radioButtons(inputId = "choose", label = "Select the file type for Kaplan-Meier plot", choices = list("png","pdf")),
              
               downloadButton(outputId = "down", label = "Download the Kaplan-Meier plot"),
              
               radioButtons(inputId = "choose1", label = "Select the file type for cumulative risk function plot", choices = list("png","pdf")),
              
               downloadButton(outputId = "down1", label = "Download the comulative risk function plot"),
               
               p("                            "),
               
               actionButton("compara","Click to compare survival curves"),
               
               radioButtons(inputId = "choose2", label = "Select the file type of Kaplan-Meier curves plot", choices = list("png","pdf")),
               
               downloadButton(outputId = "down2", label = "Download the Kaplan-Meier curves plot")
               
               
               
               ),
                   
                    mainPanel(conditionalPanel(condition = "input.kaplan_estra == 'Kaplan-Meier'",
                      
                      tabsetPanel(
                      
                      tabPanel("Kaplan-Meier",fluidRow(column(12, plotOutput("graf"))),fluidRow(column(12,verbatimTextOutput("printkm"))),
                               
                               fluidRow(column(12,h4("Summary for a specific time"),verbatimTextOutput("timesum")))),
                                         
                      tabPanel("Summary Survfit",fluidRow(column(12,verbatimTextOutput("summary1")))),tabPanel("Cumulative risk function", plotOutput("suma")))
                   
                            
                        ),
                 
                 
            conditionalPanel(condition = "input.kaplan_estra == 'Stratified Kaplan-Meier'",
                     
                     tabsetPanel(
                     
                     tabPanel("Plot and tests",fluidRow(column(12,plotOutput("curves"))),
                              
                              tabsetPanel(
                                
                                tabPanel("Log-rank test",
                
                                                    fluidRow(column(12,verbatimTextOutput("test")))),
                                
                                tabPanel("Gehan-Wilcoxon test",
                                                                                                            
                                                    fluidRow(column(12,verbatimTextOutput("testgehan")))),
                
                                tabPanel("Print of Survfit",fluidRow(column(12,verbatimTextOutput("print")))))),
                
                     tabPanel("Summary of Survfit",fluidRow(column(12,verbatimTextOutput("summary2"))))
                     
                     
                     
                
                     ))))),
               
                 tabPanel("Clusters of survival curves", add_busy_spinner(spin = "fading-circle"),
               
                           sidebarLayout(
               
                             sidebarPanel(
               
                           actionButton("cluster_page", "Start calculating"), 
               
                           h5("This can take a few minutes."),
                           h5("In order not to overload the application, please wait for the results to be compiled before navigating to another menu."),
               
                           radioButtons(inputId = "clust", label = "Select the file type", choices = list("png","pdf")),
               
                           downloadButton(outputId = "down_clust", label = "Download the plot")  
               
               
                           ),
               
                           mainPanel(
               
                             tabsetPanel(
               
                               tabPanel("Summary",
               
                              verbatimTextOutput("cluster")),
               
                              tabPanel("Plot",
               
                                    plotOutput("clus"))
               
                              )
               
                          ))),
                  
                     tabPanel("Cox PH Model",add_busy_spinner(spin = "fading-circle"),
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            
                            conditionalPanel(condition = "input.down_example == 'load_my_own2'",
                                             
                                             
                        checkboxGroupInput("cox_var","Select the covariates")),
                        
                        conditionalPanel(condition = "input.down_example == 'Example2'",
                                         
                        conditionalPanel(condition = "input.examples == 'GBSG'",
                                         
                                         checkboxGroupInput("cox_var_gbsg", "Select the covariates")),
                        
                        conditionalPanel(condition = "input.examples == 'IR_diabetes'",
                                         
                                         checkboxGroupInput("cox_var_diabetes", "Select the covariate")
                                         
                                         )
                        
                        )
                       
                                                                    #,checkboxGroupInput("cox_pspline","Select the variables")),
                                 ), mainPanel(
                       
                                               tabsetPanel(
                                                 
                                                 tabPanel("Model",fluidRow(column(12, verbatimTextOutput("cox")))),
                                                 
                                                      tabPanel("Graphical Test of Proportional Hazards", fluidRow(plotOutput("coxzph"))))
                                               
                                               ))),
                 tabPanel("AFT Model",add_busy_spinner(spin = "fading-circle"),
                          
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
                                          
                                          fluidRow(column(12, verbatimTextOutput("aft_model")))))),
                
                
                
          #     navbarMenu("Trees and random forests for survival analysis",
                          
                          tabPanel("Regression Trees",add_busy_spinner(spin = "fading-circle"),
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel(
                                       
                                       conditionalPanel(condition = "input.down_example == 'load_my_own2'",
                                       
                                       checkboxGroupInput("rpart_var","Select the covariates")),
                                       
                                       conditionalPanel(condition = "input.down_example == 'Example2'",
                                                        
                                                        conditionalPanel(condition = "input.examples == 'GBSG'",
                                                                         
                                                              checkboxGroupInput("rpart_var_gbsg", "Select the covariates"))),
                                       
                                       actionButton("page_rpart_reg","Do the analysis")
                                       
                                                    ),
                                   
                                            mainPanel(tabsetPanel(tabPanel("Plot and print",
                                              
                                              fluidRow(column(12,plotOutput("rpart_regression"))),
                                              
                                            fluidRow(column(12,verbatimTextOutput("rpart_regression_print")))),
                                              
                                              tabPanel("Summary",fluidRow(column(12,verbatimTextOutput("rpart_regression_summary"))))
                                              
                                              )))),
                 
                          
                        tabPanel("Random Forests",add_busy_spinner(spin = "fading-circle"),
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              conditionalPanel(condition = "input.down_example == 'load_my_own2'",
                              
                              checkboxGroupInput("varforests", "Select the covariates")),
                              
                              conditionalPanel(condition = "input.down_example == 'Example2'",
                                               
                              checkboxGroupInput("gbsgforests", "Select the covariates")),
                              
                              actionButton("page_forest","Do the analysis")
                              
                            ),
                            
                            mainPanel(
                              
                              
                          fluidRow(column(12,plotOutput("forestplot"),verbatimTextOutput("foresterror"),h5("Variable importance",style = "color:steelblue"),verbatimTextOutput("forestimp")))
                              
                            
                          )
                          
                          )),
          #),

                
                       #    navbarMenu("Competing risk analysis",
                                    
                                        tabPanel("Cumulative incidence",add_busy_spinner(spin = "fading-circle"),
                                    
                                                 sidebarLayout(
                                                   
                                                   sidebarPanel(
                                                  selectInput("indi_group",label= "Select cumulative incidence or cumulative incidence by group", 
                                                                            choices = c("Cumulative incidence", "Cumulative incidence by group"), selected = "Cumulative incidence"),
                                                   
                                                  
                                                  
                                                  
                                                    conditionalPanel(condition = "input.down_example == 'load_my_own2'",
                                                                     
                                                                     selectInput("compe_var1", "Select the survival time variable", character(0)),
                                                                     
                                                                     selectInput("compe_var2", "Select the survival event variable", character(0)),
                                                                     
                                                                     selectInput("compe_var3", "Select the covariate from imported database", character(0))),
                                                    
                                                    conditionalPanel(condition = "input.down_example == 'Example2'",
                                                                     
                                                                     selectInput("melanoma_var1", "Select the time variable from Melanoma", character(0)),
                                                                     
                                                                     selectInput("melanoma_var2", "Select the event variable from Melanoma", character(0)),
                                                                     
                                                                     selectInput("melanoma_var3", "Select the covariate from Melanoma", character(0))
                                                                        
                                                                     ),
                                    
                                     numericInput("late", label = "Write the status level that represents the censorship", value = ""),
                                     
                                     actionButton("page_inci", "Do the analysis for cumulative incidence"),
                                                                                        
                                     radioButtons(inputId = "incitype1", label = "Select the file type for cumulative incidence plot",choices = list("png","pdf")),
                                                  
                                     downloadButton(outputId = "inci_down1", label = "Download the cumulative incidence plot"),
                                    
                                     actionButton("page_inci_group", "Do the analysis for cumulative incidence by group"),
                                     
                                     radioButtons(inputId = "incitype2", label = "Select the file type for cumulative incidence plot by group",choices = list("png","pdf")),
                                     
                                     downloadButton(outputId = "inci_down2", label = "Download the cumulative incidence by group plot")
                                     
                                                                                        
                                   ), mainPanel(conditionalPanel(condition = "input.indi_group == 'Cumulative incidence'",
                                     
                                     tabsetPanel(
                                       
                                       tabPanel("Cumulative incidence", plotOutput("comu_inci")),
                                       
                                       tabPanel("Estimates and Variances", verbatimTextOutput("print_inci")))),
                                   
                                   conditionalPanel(condition = "input.indi_group == 'Cumulative incidence by group'",
                                   
                                        tabsetPanel(
                                                   
                                                   tabPanel("Cumulative incidence by group", plotOutput("comu_inci_group")),
                                                   
                                                   tabPanel("Tests Estimates and Variances", verbatimTextOutput("print_inci_group")))))))
                                     
                                              ,
                                     
                                   tabPanel("Competing risks regression",add_busy_spinner(spin = "fading-circle"),
                                            
                                            sidebarLayout(
                                              
                                              sidebarPanel(
                                              
                                              selectInput("regre", label = "Select the approache", choices = c("Subdistribution hazard approach" = "sub_haz" ,"Cause-specific hazards" = "speci_haz")),
                                              
                                              conditionalPanel(condition = "input.down_example == 'load_my_own2'",
                                              
                                              checkboxGroupInput("compe_reg", "Select the covariates")),
                                              
                                              conditionalPanel(condition = "input.down_example == 'Example2'",
                      
                                              checkboxGroupInput("compe_reg_melanoma", "Select the covariates from Melanoma")
                                                               
                                              ),
                                              
                                              
                                              conditionalPanel(condition = "input.regre == 'sub_haz'",
                                              
                                              numericInput("cenlevel", label = "For subdistribution hazard approach choose censor level", value = ""),
                                              numericInput("failcode", label = "Write the level of the variable of interest for which you want the results", value = "")),
                                              
                                              conditionalPanel(condition = "input.regre == 'speci_haz'",
                                              
                                              numericInput("text", label = "For cause specific hazards write the status level", value = ""))
                                                           
                                              
                                              ),
                                              
                                              mainPanel(
                                                
                                                verbatimTextOutput("risk_reg"))
                                              
                                              ))
                                   
                            
                 )))

           



              



server <- function(input, output, session) {
  
 
 # reticulate::source_python('knn.py')
  
#  credentials <- shinyauthr::loginServer(
#    id = "login",
#    data = user_base,
#    user_col = user,
#    pwd_col = password,
#    sodium_hashed = TRUE,
#    log_out = reactive(logout_init())
#  )
  
#  logout_init <- shinyauthr::logoutServer(
#    id = "logout",
#    active = reactive(credentials()$user_auth)
#  )
  
  
  
  v = reactiveValues(path = NULL)

   observeEvent(input$fileIn,{
     
   
        
          req(input$fileIn)
          
          v$data <- read.csv(input$fileIn$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quote)
        
         })
  
   
  
   
    output$downloadData <- downloadHandler(
    
    filename = function() {
      
      paste("data", ".csv", sep = "")
    
      },
    
     content = function(file) {
      
        if(input$down_example == 'Example2'){
       
            if(input$examples == "GBSG"){
          
               write.csv(gbsg, file, row.names = F)
         
              }
           
                if(input$examples == "IR_diabetes"){ 
             
                  write.csv(IR_diabetes, file, row.names = F)
             
                }
          
                  if(input$examples == "Melanoma"){
                    
                    write.csv(Melanoma, file, row.names = F)
                  }
                    
                    if(input$examples == "colonIDM"){
                    
                      write.csv(colonIDM, file, row.names = F)
                    }
          
          if(input$examples == "iris"){
            
            write.csv(iris, file, row.names = F)
          }
          
          
          }else{
      
                    write.csv(v$data, file, row.names = F)}
    
       }
  
    )
  

 
    observeEvent(gbsg, {
      
      updateSelectInput(session, "var1gbsg", choices = names(gbsg), selected = "")
    
      updateSelectInput(session, "var2gbsg", choices = names(gbsg), selected = "")
    
      updateSelectInput(session, "var3gbsg", choices = names(gbsg), selected = "")
      
      updateCheckboxGroupInput(session,"cox_var_gbsg", choices = colnames(gbsg))
      
      updateCheckboxGroupInput(session, "aft_var_gbsg", choices = colnames(gbsg))
      
      updateCheckboxGroupInput(session,"distribution_gbsg", choices = c("weibull","exponential","lognormal","loglogistic"))
      
      updateCheckboxGroupInput(session,"rpart_var_gbsg", choices = colnames(gbsg))
      
      updateCheckboxGroupInput(session,"gbsgforests", choices = colnames(gbsg))
      
    })
    

    
    
    observeEvent(IR_diabetes, {
    
      updateSelectInput(session, "var1diabetes", choices = names(IR_diabetes), selected = "")
      
      updateSelectInput(session, "var2diabetes", choices = names(IR_diabetes), selected = "")
      
      updateSelectInput(session, "var3diabetes", choices = names(IR_diabetes), selected = "")
    
      updateCheckboxGroupInput(session, "cox_var_diabetes", choices = colnames(IR_diabetes))
      
      updateCheckboxGroupInput(session, "aft_var_diabetes", choices = colnames(IR_diabetes))
      
      updateCheckboxGroupInput(session,"distribution_diabetes", choices = c("weibull","exponential","loglogistic"))
      
      })
    
    
      observeEvent(Melanoma, {
      
      updateSelectInput(session, "melanoma_var1", choices = names(Melanoma), selected = "")
      
      updateSelectInput(session, "melanoma_var2", choices = names(Melanoma), selected = "")
      
      updateSelectInput(session, "melanoma_var3", choices = names(Melanoma), selected = "")
      
      updateCheckboxGroupInput(session, "compe_reg_melanoma", choices = colnames(Melanoma))
      
      })
    
    
      observeEvent(colonIDM, {
        
        colonIDM$time1 <- as.numeric(colonIDM$time1)
        
        updateSelectInput(session, "time1", choices = names(colonIDM), selected = "")
        
        updateSelectInput(session, "event1", choices = names(colonIDM), selected = "")
        
        updateSelectInput(session, "Stime", choices = names(colonIDM), selected = "")
        
        updateSelectInput(session, "event", choices = names(colonIDM), selected = "")
        
      })
      
      
      
        
output$str_dados <- renderPrint({
    
#  req(credentials()$user_auth)
  
    if(input$down_example == 'Example2'){
    
        if(input$examples == "GBSG"){
        
          gbsg$grade <- as.factor(gbsg$grade)
    
          gbsg$meno <- as.factor(gbsg$meno)
      
          gbsg$hormon <- as.factor(gbsg$hormon)
          
          str(gbsg)
      
          }
        
            if(input$examples == "IR_diabetes"){
           
              IR_diabetes$gender <- as.factor(IR_diabetes$gender)
              
              str(IR_diabetes)
            
            }
      
      if(input$examples == "Melanoma"){
        
        Melanoma$sex <- as.factor(Melanoma$sex)
        
        Melanoma$ulcer <- as.factor(Melanoma$ulcer)
        
        str(Melanoma)
      
      }
      
      if(input$examples == "colonIDM"){
        
        
        colonIDM$sex <- as.factor(colonIDM$sex)
        
        colonIDM$differ <- as.factor(colonIDM$differ)
        
        colonIDM$extent <- as.factor(colonIDM$extent)
        
        colonIDM$surg <- as.factor(colonIDM$surg)
        
        colonIDM$node4 <- as.factor(colonIDM$node4)
        
        str(colonIDM)
        
      }
      
      
      
    }
      
    else{
    
    str(v$data)
    
      }
  
    })
     
  
  output$colname_in <- renderUI({
    
     req(v$data)
    
    selectInput(inputId = "colname",
                label = "Choose column",
                choices = c("",colnames(v$data)),
                selected = "")
    
    })
  
  
  observeEvent(input$change_class, {
    
    v$data <- eval(parse(text = paste0('v$data %>% mutate(',
                                       input$colname,
                                       ' = as.',
                                       input$class,
                                       '(',
                                       input$colname,
                                       '))')
    
                         )
  
      )
    
  })
  
  
  output$tabela <- DT::renderDataTable({
    
 #   req(credentials()$user_auth)
    
    if(input$down_example == 'Example2'){
    
        if(input$examples == "GBSG"){
      
            datatable(gbsg)}
      
    else
      
          if(input$examples == "IR_diabetes"){
        
              datatable(IR_diabetes)
  
                  }
  
      
      else
        
        
      if(input$examples == "Melanoma"){
        
        datatable(Melanoma)
        
      }
      
      else
        
         
      if(input$examples == "colonIDM"){
        
        datatable(colonIDM)
        
      }    }else{
    
    datatable(v$data)
   
                 }

      })
  
  
  output$summaryTable <- renderUI({
    
  #  req(credentials()$user_auth)
    
    if(input$down_example == 'Example2'){
    
        if(input$examples == "GBSG"){
      
          gbsg$grade <- as.factor(gbsg$grade)
  
          gbsg$meno <- as.factor(gbsg$meno)
  
          gbsg$hormon <- as.factor(gbsg$hormon)
          
            out <- print(dfSummary(gbsg,graph.magnif = 0.8), style = 'grid', omit.headings = TRUE, method = 'render',bootstrap.css = FALSE) 
      
            }else
        
              if(input$examples == "IR_diabetes"){
          
                IR_diabetes$gender <- as.factor(IR_diabetes$gender)
                
                out <- print(dfSummary(IR_diabetes,graph.magnif = 0.8), style = 'grid', omit.headings = TRUE, method = 'render',bootstrap.css = FALSE) 
        
                }
      
      else
        
        if(input$examples == "Melanoma"){
          
          Melanoma$sex <- as.factor(Melanoma$sex)
          
          Melanoma$ulcer <- as.factor(Melanoma$ulcer)
          
          out <- print(dfSummary(Melanoma,graph.magnif = 0.8), style = 'grid', omit.headings = TRUE, method = 'render',bootstrap.css = FALSE) 
          
        }
      
      else
        
        if(input$examples == "iris"){
          
          iris$Species <- as.factor(iris$Species)
          
       out <- print(dfSummary(iris,graph.magnif = 0.8), style = 'grid', omit.headings = TRUE, method = 'render',bootstrap.css = FALSE) 
          
        }
      
      
      if(input$examples == "colonIDM"){
        
        
        colonIDM$sex <- as.factor(colonIDM$sex)
        
        colonIDM$differ <- as.factor(colonIDM$differ)
        
        colonIDM$extent <- as.factor(colonIDM$extent)
        
        colonIDM$surg <- as.factor(colonIDM$surg)
        
        colonIDM$node4 <- as.factor(colonIDM$node4)
        
        out <- print(dfSummary(colonIDM,graph.magnif = 0.8), style = 'grid', omit.headings = TRUE, method = 'render',bootstrap.css = FALSE) 
        
      }
        
        
      
      
              }else{
         
                req(v$data)
    
                out <- print(dfSummary(v$data,graph.magnif = 0.8), style = 'grid', omit.headings = TRUE, method = 'render',bootstrap.css = FALSE)
      
                }
    
    out
    
  })
  
  observeEvent(v$data, {
  
    updateSelectInput(session, "variable1", choices = names(v$data), selected = "") 
    
    updateSelectInput(session, "variable2", choices = names(v$data), selected = "")
    
    updateSelectInput(session, "variable3", choices = names(v$data) , selected = "")
    
    updateSelectInput(session, "variable4", choices = names(v$data), selected = "") 
    
    updateSelectInput(session, "variable5", choices = names(v$data), selected = "")
    
    updateSelectInput(session, "variable6", choices = names(v$data) , selected = "")
    
    updateCheckboxGroupInput(session,"cox_var", choices = colnames(v$data))

    updateCheckboxGroupInput(session,"distribution", choices = c("weibull","exponential","lognormal","loglogistic"))
    
    updateCheckboxGroupInput(session, "aft_variables", choices = colnames(v$data))
    
    updateCheckboxGroupInput(session,"rpart_var", choices = colnames(v$data))
    
    updateCheckboxGroupInput(session,"rpart_class_var", choices = colnames(v$data))
    
    updateSelectInput(session, "response", choices = names(v$data), selected = "")
    
    updateSelectInput(session, "compe_var1", choices = names(v$data), selected = "")
    
    updateSelectInput(session, "compe_var2", choices = names(v$data), selected = "")
   
    updateSelectInput(session, "compe_var3", choices = names(v$data), selected = "")
    
    updateCheckboxGroupInput(session,"compe_reg", choices = colnames(v$data))
    
    updateCheckboxGroupInput(session,"varforests", choices = colnames(v$data))
    
    updateSelectInput(session, "multi_var1", choices = names(v$data), selected = "") 
    
    updateSelectInput(session, "multi_var2", choices = names(v$data), selected = "")
    
    updateSelectInput(session, "multi_var3", choices = names(v$data) , selected = "")
    
    updateSelectInput(session, "multi_var4", choices = names(v$data), selected = "") 
    
    
       })
  

    model <- eventReactive(input$kaplan,{
    
    if(input$down_example == 'Example2'){
      
      if(input$examples == "GBSG"){
        
        fit <- survfit(Surv(unlist(gbsg[input$var1gbsg]), unlist(gbsg[input$var2gbsg]) )~1, data = gbsg)
        
      }else{
        
        if(input$examples == "IR_diabetes"){
          
          fit <- survfit(Surv(unlist(IR_diabetes[input$var1diabetes]), unlist(IR_diabetes[input$var2diabetes]), type = "interval2")~1,data = IR_diabetes)
          
        }
        
      }
      
    }else{
      
      if(input$censor == "right"){
        
        tempo <- unlist(v$data[input$variable1])
        
        estado <- unlist(v$data[input$variable2])
        
        fit <- survfit(Surv(tempo,estado)~1, data = v$data)
        
      }else{
        
        if(input$censor == "interval"){
          
          tempo1 <- unlist(v$data[input$variable4])
          
          tempo2 <- unlist(v$data[input$variable5])
          
          fit <- survfit(Surv(tempo1, tempo2, type = "interval2") ~ 1, data = 
                           v$data)
          
        }
        
      }
      
    }
    
    list(fit = fit)
    
  })
  
  
output$graf <- renderPlot({
  
 if(input$variable1 == "" & input$variable2 == "" & input$variable4 == "" & input$variable5 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var1diabetes == "" & input$var2diabetes == "") {
     
  print(NULL)
     
   }else{
     
   autoplot(model()$fit,surv.colour = "#2a283d") + 
     labs(x = "\n Survival Time", y = "Survival Probabilities \n")+
     theme(axis.text=element_text(size=14))+
     theme(axis.title.x  = element_text(size = 14))+
     theme(axis.title.y  = element_text(size = 14))+
      theme(
         panel.background = element_rect(fill = "#e5f9f4",
                                         colour = "lightblue",size = 0.5, linetype = "solid"),
         
         
         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                         colour = "white"), 
         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                         colour = "white")
         )
       
   }
              
    })
 

output$timesum <- renderPrint({
  
  if(input$variable1 == "" & input$variable2 == "" & input$variable4 == "" & input$variable5 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var1diabetes == "" & input$var2diabetes == ""){
    
    print(NULL)
    
  }else{
    
 summary(model()$fit, times = input$num)
    
  }
  
})
  
 output$printkm <- renderPrint({
   
   if(input$variable1 == "" & input$variable2 == "" & input$variable4 == "" & input$variable5 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var1diabetes == "" & input$var2diabetes == "") {
     
     print(NULL)
     
   }else{
   
    print(model()$fit,print.rmean = T)
   
   }
    
   })
 
 output$summary1 <- renderPrint({
   
   if(input$variable1 == "" & input$variable2 == "" & input$variable4 == "" & input$variable5 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var1diabetes == "" & input$var2diabetes == "") {
     
     print(NULL)
     
   }else{
   
   summary(model()$fit)

   }
   
    })
 
 
 output$suma <- renderPlot({
   
   if(input$variable1 == "" & input$variable2 == "" & input$variable4 == "" & input$variable5 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var1diabetes == "" & input$var2diabetes == "") {
     
     print(NULL)
     
   }else{
   
   autoplot(model()$fit,surv.colour = "#2a283d",data = v$data, fun="cumhaz")+ 
     labs(x = "\n Survival Time", y = "Cumulative Hazard \n")+
     theme(axis.text=element_text(size=14))+
     theme(axis.title.x  = element_text(size = 14))+
     theme(axis.title.y  = element_text(size = 14))+
       theme(
         panel.background = element_rect(fill = "#e5f9f4",
                                         colour = "lightblue",size = 0.5, linetype = "solid"),
         
         
         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                         colour = "white"), 
         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                         colour = "white")
       )
 
   }
   
   })
 
 
 output$down  <- downloadHandler(
   
   filename = function() {
     
     paste("survival",input$choose, sep=".") 
   
     },
   
   content = function(file){
    
       if(input$choose == "png")
     
           png(file)
     
     else
     
         pdf(file)
     
       print(autoplot(model()$fit,surv.colour = "#2a283d"))+ 
       labs(x = "\n Survival Time", y = "Survival Probabilities \n")+
       theme(axis.text=element_text(size=14))+
       theme(axis.title.x  = element_text(size = 14))+
       theme(axis.title.y  = element_text(size = 14))+
         theme(
           panel.background = element_rect(fill = "#e5f9f4",
                                           colour = "lightblue",size = 0.5, linetype = "solid"),
           
           
           panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                           colour = "white"), 
           panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                           colour = "white")
         )
     
     dev.off()
   
     }
   
)
 
 
 output$down1  <- downloadHandler(
   
   filename = function() {
     
     paste("cumulative",input$choose1, sep=".") 
   
     },
   
   content = function(file){
     
     if(input$choose1 == "png")
     
         png(file)
     
     else
       
       pdf(file)
     
     print(autoplot(model()$fit,surv.colour = "#2a283d",data = v$data, fun="cumhaz"))+ 
       labs(x = "\n Survival Time", y = "Cumulative Hazard \n")+
       theme(axis.text=element_text(size=14))+
       theme(axis.title.x  = element_text(size = 14))+
       theme(axis.title.y  = element_text(size = 14))+
       theme(
         panel.background = element_rect(fill = "#e5f9f4",
                                         colour = "lightblue",size = 0.5, linetype = "solid"),
         
         
         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                         colour = "white"), 
         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                         colour = "white")
       )
     
     dev.off()
   
     }
   
 )
 
 model1 <- eventReactive(input$compara,{
   
   if(input$down_example == 'Example2'){
       
       if(input$examples == "GBSG"){
         
         gbsg[gbsg$nodes > 13,'nodes'] <- 14  
         
         km1 <- survfit(Surv(unlist(gbsg[input$var1gbsg]), unlist(gbsg[input$var2gbsg]))~unlist(gbsg[input$var3gbsg]), data = gbsg)
         
         km1_dif <- survdiff(Surv(unlist(gbsg[input$var1gbsg]), unlist(gbsg[input$var2gbsg]))~unlist(gbsg[input$var3gbsg]), data = gbsg)
         
         km1_dif1 <- survdiff(Surv(unlist(gbsg[input$var1gbsg]), unlist(gbsg[input$var2gbsg]))~unlist(gbsg[input$var3gbsg]),rho = 1 ,data = gbsg)
         
       }else{
         
         if(input$examples == "IR_diabetes"){
           
           km1 <- survfit(Surv(unlist(IR_diabetes[input$var1diabetes]), unlist(IR_diabetes[input$var2diabetes]), type = "interval2")~unlist(IR_diabetes[input$var3diabetes]),data = IR_diabetes)
           
           km1_dif <- NULL
           
           km1_dif1 <- NULL
           
         }
         
       }
       
     }else{
       
       if(input$censor == "right"){
         
         tempo <- unlist(v$data[input$variable1])
         
         status <- unlist(v$data[input$variable2])
         
         km1 <- survfit(Surv(tempo,status)~unlist(v$data[input$variable3]), data = v$data)
         
         formula.surv<-as.formula(paste("Surv(tempo,status)~",
                                        paste(input$variable3, collapse = "+")))
         
         
        km1_dif <- survdiff(formula.surv, data=v$data)
        
        km1_dif1 <- survdiff(formula.surv, data=v$data, rho = 1)
         
       }else{
         
         if(input$censor == "interval"){
           
           tempo1 <- unlist(v$data[input$variable4])
           
           tempo2 <- unlist(v$data[input$variable5])
           
           estatuto <- unlist(v$data[input$variable6])
           
           km1 <- survfit(Surv(tempo1,tempo2,type = "interval2")~estatuto,data = v$data)
           
           km1_dif <- NULL
           
           km1_dif1 <- NULL
           
         }
         
       }
       
     }
   
   list(km1 = km1, km1_dif = km1_dif, km1_dif1 = km1_dif1)
   
   
 })
 
 
 output$curves <- renderPlot({
   
   if(input$variable1 == "" & input$variable2 == "" & input$variable3 == "" & input$variable4 == "" & input$variable5 == "" & input$variable6 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var3gbsg == "" & input$var1diabetes == "" & input$var2diabetes == "" & input$var3diabetes == "") {
     
     print(NULL)
     
   }else{
   
   autoplot(model1()$km1)+ 
     labs(x = "\n Survival Time", y = "Survival Probabilities \n")+
     theme(axis.text=element_text(size=14))+
     theme(axis.title.x  = element_text(size = 14))+
     theme(axis.title.y  = element_text(size = 14))+
     theme(legend.title = element_text(size = 14))+
     theme(legend.text = element_text(size = 14))+
       theme(
         panel.background = element_rect(fill = "#e5f9f4",
                                         colour = "lightblue",size = 0.5, linetype = "solid"),
         
         
         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                         colour = "white"), 
         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                         colour = "white")
       )
 
     }
    
 })
 
 
 output$summary2 <- renderPrint({
   
   if(input$variable1 == "" & input$variable2 == "" & input$variable3 == "" & input$variable4 == "" & input$variable5 == "" & input$variable6 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var3gbsg == "" & input$var1diabetes == "" & input$var2diabetes == "" & input$var3diabetes == "") {
     
     print(NULL)
     
   }else{
   
  summary(model1()$km1)
 
 }
   
   }) 
  
 
 output$print <- renderPrint({
   
   if(input$variable1 == "" & input$variable2 == "" & input$variable3 == "" & input$variable4 == "" & input$variable5 == "" & input$variable6 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var3gbsg == "" & input$var1diabetes == "" & input$var2diabetes == "" & input$var3diabetes == "") {
     
     print(NULL)
     
   }else{
   
   print(model1()$km1) 
   
   }
   
   })
 

 output$test <- renderPrint({
   
   if(input$variable1 == "" & input$variable2 == "" & input$variable3 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var3gbsg == "" & input$var1diabetes == "" & input$var2diabetes == "" & input$var3diabetes == "") {
     
     print(NULL)
     
   }else{
   
   
     model1()$km1_dif
     
   }
     
   })
  
 
 output$testgehan <- renderPrint({
   
   if(input$variable1 == "" & input$variable2 == "" & input$variable3 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var3gbsg == "" & input$var1diabetes == "" & input$var2diabetes == "" & input$var3diabetes == "") {
     
     print(NULL)
     
   }else{
   
   model1()$km1_dif1

}
     
 })
 

  output$down2  <- downloadHandler(
    
    filename = function() {
    
        paste("covariate",input$choose2, sep = ".") 
    
      },
    
    content = function(file){
      
      if(input$choose2 == "png")
      
            png(file)
      
      else
      
          pdf(file)
      
         print(autoplot(model1()$km1))+ 
        labs(x = "\n Survival Time", y = "Survival Probabilities \n")+
        theme(axis.text=element_text(size=14))+
        theme(axis.title.x  = element_text(size = 14))+
        theme(axis.title.y  = element_text(size = 14))+
        theme(legend.title = element_text(size = 14))+
        theme(legend.text = element_text(size = 14))+
           theme(
             panel.background = element_rect(fill = "#e5f9f4",
                                             colour = "lightblue",size = 0.5, linetype = "solid"),
             
             
             panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                             colour = "white"), 
             panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                             colour = "white")
           )
      
      dev.off()
    
      }
    
  )
  
  model_clus <- eventReactive(input$cluster_page,{
      withProgress(message = "Calculating the clusters", value = 10,{ 
          if(input$down_example == 'Example2'){
    
          if(input$examples == "GBSG"){
    
            gbsg[gbsg$nodes > 13,'nodes'] <- 14
    
            fit.gbcs <- survclustcurves(time = unlist(gbsg[input$var1gbsg]), status = unlist(gbsg[input$var2gbsg]),
                                      x = unlist(gbsg[input$var3gbsg]), nboot = 20, seed = 300716, algorithm = 'kmedians',
                                        cluster = TRUE)
    
          }}else{
    
            a <- unlist(v$data[input$variable1])
    
            b <- unlist(v$data[input$variable2])
    
            c <- unlist(v$data[input$variable3])
    
            fit.gbcs <- survclustcurves(time = a, status = b,
                                        x = c, nboot = 50, seed = 300716, algorithm = 'kmedians',
                                        cluster = TRUE)
    
          }
        })
       list(fit.gbcs = fit.gbcs)
    
    })
    
    
     output$cluster <- renderPrint({
    
          if(input$variable1 == "" & input$variable2 == "" & input$variable3 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var3gbsg == "") {
    
          print(NULL)
    
        }else{
    
      summary(model_clus()$fit.gbcs)
    
        }  
    
      })
    
    
       output$clus <- renderPlot({
    
        if(input$variable1 == "" & input$variable2 == "" & input$variable3 == "" & input$var1gbsg == "" & input$var2gbsg == "" & input$var3gbsg == "") {
    
          print(NULL)
    
        }else{
    
          autoplot(model_clus()$fit.gbcs,groups_by_color = TRUE)+
          labs(x = "\n Survival Time", y = "Survival Probabilities \n")+
          theme(axis.text=element_text(size=14))+
          theme(axis.title.x  = element_text(size = 14))+
          theme(axis.title.y  = element_text(size = 14))+
          theme(legend.title = element_text(size = 14))+
          theme(legend.text = element_text(size = 14))
    
        }
    
      })
    
    
      output$down_clust  <- downloadHandler(
    
        filename = function() {
    
          paste("clusters_curves",input$clust, sep = ".") 
    
          },
    
        content = function(file){
    
          if(input$clust == "png")
    
            png(file)
    
          else
    
            pdf(file)
    
          print(autoplot(model_clus()$fit.gbcs,groups_by_color = TRUE))+
            labs(x = "\n Survival Time", y = "Survival Probabilities \n")+
            theme(axis.text=element_text(size=14))+
            theme(axis.title.x  = element_text(size = 14))+
            theme(axis.title.y  = element_text(size = 14))+
            theme(legend.title = element_text(size = 14))+
            theme(legend.text = element_text(size = 14))
    
            dev.off()
    
        }
    
      )
  
  
  
  
  
    output$cox <- renderPrint({
    
    if(is.null(input$cox_var_gbsg) & is.null(input$cox_var_diabetes) & input$variable1 == "" & input$variable2 == "" & input$variable4 == "" & input$variable5 == "" & is.null(input$cox_var)) {
      
      print(NULL)
      
    }else{
    
    if(input$down_example == 'Example2'){
   
         if(input$examples == "GBSG"){
       
           gbsg$grade <- as.factor(gbsg$grade)
           
           gbsg$meno <- as.factor(gbsg$meno)
           
           gbsg$hormon <- as.factor(gbsg$hormon)
           
           formula.cox<-as.formula(paste("Surv(rfstime, status)~",
                                         paste(input$cox_var_gbsg ,collapse = "+")
           ))
           
             fit <- coxph(formula.cox, data = gbsg)
        
             print(fit)
  
             cat('','\n')
  
             cat('AIC value of the model:','\n')
  
             print(extractAIC(fit)[2])
             
             
      }else{
        
        if(input$examples == "IR_diabetes"){
        
            fit <- ic_sp(Surv(left, right, type = "interval2") ~ unlist(IR_diabetes[input$cox_var_diabetes]),data = IR_diabetes)
        
            print(fit)
            
            }
      }
      
      }else{
      
        if(input$censor == "right"){
      
      a <- unlist(v$data[input$variable1])
  
      b <- unlist(v$data[input$variable2])
      
      formula.cox<-as.formula(paste("Surv(a,b)~",
                                    paste(input$cox_var ,collapse = "+")
                                    ))
      
      fit <- coxph(formula.cox, data = v$data)
      
      print(fit)
  
      cat('','\n')
  
      cat('AIC value of the model:','\n')
  
      print(extractAIC(fit)[2])
      
      }else{
          
          if(input$censor == "interval"){
      
            tempo1 <- unlist(v$data[input$variable4])
    
            tempo2 <- unlist(v$data[input$variable5])
    
            estatuto <- unlist(v$data[input$cox_var])
            
            fit <- ic_sp(Surv(tempo1,tempo2, type = 'interval2') ~ estatuto,data = v$data)
          
            print(fit)
            
          }
        
          }
        
      }
    
    }
      
  })
  
  output$coxzph <- renderPlot({

    if(is.null(input$cox_var_gbsg) & input$variable1 == "" & input$variable2 == "" & is.null(input$cox_var)) {
      
      print(NULL)
      
    }else{
    
    if(input$down_example == 'Example2'){
      
      if(input$examples == "GBSG"){
      
        gbsg$grade <- as.factor(gbsg$grade)
      
        gbsg$meno <- as.factor(gbsg$meno)
        
        gbsg$hormon <- as.factor(gbsg$hormon)
        
        formula.cox<-as.formula(paste("Surv(rfstime, status)~",
                                      paste(input$cox_var_gbsg ,collapse = "+")))
        
        
        fit <- coxph(formula.cox, data = gbsg)
    
        ggcoxzph(cox.zph(fit))
      
        }
          
      }else{
    
        if(input$censor == "right"){
    
      a <- unlist(v$data[input$variable1])
      
      b <- unlist(v$data[input$variable2])
      
      formula.cox<-as.formula(paste("Surv(a,b)~",
                                    paste(input$cox_var, collapse = "+")))
      
      fit <- coxph(formula.cox, data = v$data)
      
      ggcoxzph(cox.zph(fit))
      
        }
        
        }
  
    }
      
    })
  
  
  output$aft_model <- renderPrint({
   
    if(is.null(input$aft_var_gbsg) & is.null(input$distribution_gbsg) & input$variable1 == "" & input$variable2 == "" & input$variable4 =="" & input$variable5 == "" & is.null(input$aft_variables) & is.null(input$distribution) & is.null(input$aft_var_diabetes) & is.null(input$distribution_diabetes)) {
      
      print(NULL)
      
    }else{
    
       if(input$down_example == 'Example2'){
      
      if(input$examples == "GBSG"){
        
        gbsg$grade <- as.factor(gbsg$grade)
        
        gbsg$meno <- as.factor(gbsg$meno)
        
        gbsg$hormon <- as.factor(gbsg$hormon)
        
        
        formula.aft <- as.formula(paste("Surv(rfstime,status)~",
                                        paste(input$aft_var_gbsg, collapse = "+")))
        
        fit.para <- survreg(formula.aft,data = gbsg,dist = input$distribution_gbsg)
        
        print(summary(fit.para))
        
        cat('','\n')
        
        cat('AIC value of the model:','\n')
        
        print(extractAIC(fit.para)[2])
       
    
      }else{
        
        if(input$examples == "IR_diabetes"){
          
          par_fit <- ic_par(cbind(left, right) ~ unlist(IR_diabetes[input$aft_var_diabetes]), data = IR_diabetes, dist = input$distribution_diabetes)
          
          summary(par_fit)
          
        }
    
          }
      
    }else{
    
      if(input$censor == "right"){
      
         time <- unlist(v$data[input$variable1])
      
         status <- unlist(v$data[input$variable2])
      
      if(is.null(input$aft_variables)){
        
        fit_0<- survreg(Surv(time, status) ~ 1, data=v$data, dist = input$distribution)
        
        print(summary(fit_0))
       
        cat('','\n')
       
        cat('AIC value of the model:','\n')
        
        print(extractAIC(fit_0)[2])
        
       }else{
        
        formula.aft_model<-as.formula(paste("Surv(time,status)~",
                                            paste(input$aft_variables, collapse = "+")))
        fit1 <- survreg(formula.aft_model, data = v$data, dist = input$distribution)
       
        print(summary(fit1))
        
        cat('','\n')
       
        cat('AIC value of the model:','\n')
        
        print(extractAIC(fit1)[2])
    
          }
    
      }else{
    
        if(input$censor == "interval"){
        
        tempo1 <- unlist(v$data[input$variable4])
    
        tempo2 <- unlist(v$data[input$variable5])
  
        estatuto <- unlist(v$data[input$aft_variables])
        
        par_fit <- ic_par(cbind(tempo1, tempo2) ~ estatuto, data = v$data, dist = input$distribution)
  
        summary(par_fit)
  
      }
      
      }
      
    }
    
    }
      
    })
  
 
model_rpart_reg <- eventReactive(input$page_rpart_reg,{
  
  if(input$down_example == 'Example2'){
    
    if(input$examples == "GBSG"){
      
      time <- unlist(gbsg[input$var1gbsg])
      
      status <- unlist(gbsg[input$var2gbsg])
      
      formula.rpart_gbsg<-as.formula(paste("Surv(time,status)~",
                                           paste(input$rpart_var_gbsg, collapse = "+")))
      
      set.seed(123)
      
      model <- rpart(formula.rpart_gbsg, data = gbsg)
      
      km <- survfit(Surv(time, status) ~ model$where, data=gbsg)
      
    }
    
    if(input$examples == "IR_diabetes"){
      
      print(NULL)
      
    }
    
  }else{
    
    if(input$censor == "right"){
      
      if(length(input$rpart_var) == 0){
        
        print(NULL)
        
      }else{
        
        time <- unlist(v$data[input$variable1])
        
        status <- unlist(v$data[input$variable2])
        
        formula.rpart_regre<-as.formula(paste("Surv(time,status)~",
                                              paste(input$rpart_var, collapse = "+")))
        
        set.seed(123)
        
        model <- rpart(formula.rpart_regre, data = v$data)
        
        km <- survfit(Surv(time, status) ~ model$where, data=v$data)
        
      }}else{
        
        if(input$censor == "interval"){
          
          print(NULL)
        }   
        
      }
    
  }
  
  list(model = model, km = km)
  
})  
   
  output$rpart_regression <- renderPlot({
    
    if(is.null(input$rpart_var_gbsg) & is.null(input$rpart_var) & input$variable1 == "" & input$variable2 == "" & input$var1gbsg =="" & input$var2gbsg == ""){
      
      print(NULL)
      
    }else{
    
  rpart.plot(model_rpart_reg()$model, type=5, extra=2, faclen=0, under=TRUE,
                   cex=1.1)
        
    }
    
      })

  
  output$rpart_regression_print <- renderPrint({
    
    if(is.null(input$rpart_var_gbsg) & is.null(input$rpart_var) & input$variable1 == "" & input$variable2 == "" & input$var1gbsg =="" & input$var2gbsg == ""){
      
      print(NULL)
      
    }else{
      
    print(model_rpart_reg()$km)

      }
    
})
  
  
  output$rpart_regression_summary <- renderPrint({
    
    if(is.null(input$rpart_var_gbsg) & is.null(input$rpart_var) & input$variable1 == "" & input$variable2 == "" & input$var1gbsg =="" & input$var2gbsg == ""){
      
      print(NULL)
      
    }else{
    
  print(summary(model_rpart_reg()$km))
          
    } 
      
    })
  
  
  model_rpart_class <- eventReactive(input$page_rpart_class,{
    
    if(input$down_example == 'Example2'){
      
      if(input$examples == 'iris'){
        
        set.seed(123)
        
       # response <- unlist(iris[input$iris_response])
   #  response <- unlist(iris[input$iris_response])
        training.samples <- iris$Species %>%
          createDataPartition(p = 0.8, list = FALSE)
        train.data <- iris[training.samples,]
        test.data <- iris[-training.samples,]

        formula.rpart_iris<-as.formula(paste("Species~",paste(input$rpart_class_iris, collapse = "+")))
        
        model <- train(formula.rpart_iris, data = train.data, method = "rpart",
                        trControl = trainControl("cv",number = 10),
                        tuneLength = 10)
        
        predicted.classes <- model %>% predict(test.data)
        acuracy <- mean(predicted.classes == test.data$Species)
   
      }
      
      
      
    }else{
      
          
          set.seed(123)
          
          response <- unlist(v$data[input$response])
          
          training.samples <- response %>%
            createDataPartition(p = 0.8, list = FALSE)
          train.data <- v$data[training.samples, ]
          test.data <- v$data[-training.samples, ]
          
          formula.rpart<-as.formula(paste("response~",paste(input$rpart_class_var, collapse = "+")))
          
          model <- train(formula.rpart, data = train.data, method = "rpart",na.action = na.pass,
                         trControl = trainControl("cv",number = 10),
                         tuneLength = 10)
          
          predicted.classes <- model %>% predict(test.data)
          acuracy <- mean(predicted.classes == test.data$response)
          
          
        }
    
    list(model = model,predicted.classes = predicted.classes, acuracy = acuracy)
    #, pred = pred)
    
  })
  
    
  output$rpart_classification <- renderPlot({  
  
 #   if(is.null(input$rpart_class_iris) & is.null(input$rpart_class_var) & input$iris_response == "" & input$response == ""){
      

    #      print(NULL)
      
  #  }else{
    
  rpart.plot(model_rpart_class()$model$finalModel, type=5,extra = 2,faclen=0, under=TRUE,cex=1.1)
   
           
  #  }
      
    })
  
  
  output$rpart_class_pred <- renderPrint({ 
    
    
    
        print(model_rpart_class()$acuracy)
          
      
      
    })

model_forest <- eventReactive(input$page_forest,{
  
  if(input$down_example == 'Example2'){
    
    if(input$examples == "GBSG"){
      
      a <- unlist(gbsg[input$var1gbsg])
      
      b <- unlist(gbsg[input$var2gbsg])
      
      ranger_formula <- as.formula(paste("Surv(a, b)~", paste(input$gbsgforests, collapse = "+")))
      
      set.seed(123)
      
      r_fit <- ranger(ranger_formula,
                      data = gbsg,
                      mtry = 2,
                      importance = "permutation",
                      splitrule = "extratrees",
                      verbose = TRUE)
      
     }
    
    if(input$examples == "IR_diabetes"){
      
      print(NULL)
      
    }}else{
      
      if(input$down_example == 'load_my_own2'){
        
        a <- unlist(v$data[input$variable1])
        
        b <- unlist(v$data[input$variable2])
        
        ranger_formula <- as.formula(paste("Surv(a, b)~", paste(input$varforests, collapse = "+")))
        
        set.seed(123)
        
        r_fit <- ranger(ranger_formula,
                        data = v$data,
                        mtry = 2,
                        importance = "permutation",
                        splitrule = "extratrees",
                        verbose = TRUE)
        
      }}
  
  death_times <- r_fit$unique.death.times 
  surv_prob <- data.frame(r_fit$survival)
  avg_prob <- sapply(surv_prob,mean)
  
  vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
  names(vi) <- "importance"
  
  list(r_fit = r_fit, death_times = death_times, surv_prob = surv_prob, avg_prob = avg_prob, vi = vi)
  
  })  
    
  
  output$forestplot <- renderPlot({
    
    if(is.null(input$gbsgforests) & is.null(input$varforests)){
      
      print(NULL)
      
    }else{
    
       plot(model_forest()$r_fit$unique.death.times,model_forest()$r_fit$survival[1,], 
             type = "l", 
             ylim = c(0,1),
             col = "red",
             xlab = "Time",
             ylab = "survival",
             cex.lab=1.5,
             cex.axis=1.5,
             cex.main=1.5,
             cex.sub=1.5)
        
        cols <- colors()
      
        if(input$down_example == 'Example2'){
          
          if(input$examples == "GBSG"){
        
          for (n in sample(c(2:dim(gbsg)[1]), 20)){
    
        lines(model_forest()$r_fit$unique.death.times, model_forest()$r_fit$survival[n,], type = "l", col = cols[n])
  
          }
            
          }
          
        }else{
          
          if(input$down_example == 'load_my_own2'){
          
            for (n in sample(c(2:dim(v$data)[1]), 20)){
              
              lines(model_forest()$r_fit$unique.death.times, model_forest()$r_fit$survival[n,], type = "l", col = cols[n])
              
            } 
            
             }
        
          }
        
        lines(model_forest()$death_times, model_forest()$avg_prob, lwd = 2)
        
        legend(500, 0.7,cex = 1.5 ,legend = c('Average = black'))
        
    }
    
  })

  
output$forestimp <- renderPrint({
    
    if(is.null(input$gbsgforests) & is.null(input$varforests)){
      
      print(NULL)
      
    }else{

    print(model_forest()$vi)
    
        }
    
  })
  
  
output$foresterror <- renderPrint({
  
  if(is.null(input$gbsgforests) & is.null(input$varforests)){
    
    print(NULL)
    
  }else{
  
  cat("Prediction Error = ", model_forest()$r_fit$prediction.error)
  
  }
  
})  
  

model_inci <- eventReactive(input$page_inci,{
  
  if(input$down_example == 'load_my_own2'){
    
    fit <- cuminc(ftime = unlist(v$data[input$compe_var1]), fstatus = unlist(v$data[input$compe_var2]), cencode = input$late)
    
  }else{
    
    if(input$down_example == 'Example2'){
      
      fit <- cuminc(ftime = unlist(Melanoma[input$melanoma_var1]), fstatus = unlist(Melanoma[input$melanoma_var2]), cencode = input$late)
      
    }
    
  }
  
  
  list(fit = fit)
  
})

  
 output$comu_inci <- renderPlot({
    
   if(input$compe_var1 == "" & input$compe_var2 == "" & input$melanoma_var1 == "" & input$melanoma_var2 == ""){
     
     print(NULL)
     
   }else{  
   
       ggcompetingrisks(model_inci()$fit)+
             labs(x = "\n Survival Time", y = "Survival Probabilities \n")+
             theme(axis.text=element_text(size=14))+
             theme(axis.title.x  = element_text(size = 14))+
             theme(axis.title.y  = element_text(size = 14))+
             theme(legend.title = element_text(size = 14))+
             theme(legend.text = element_text(size = 14))+
             geom_line(size = 1)
    
   }
           
      })
  
 
 output$print_inci <- renderPrint({
   
   if(input$compe_var1 == "" & input$compe_var2 == "" & input$melanoma_var1 == "" & input$melanoma_var2 == ""){
     
     print(NULL)
     
   }else{
     
     model_inci()$fit
     
   }
   
 })
  
 
 output$inci_down1 <- downloadHandler(
   
   filename = function(){
     paste("incidence", input$incitype1, sep = ".")
   },
   
   content = function(file){
     
     if(input$incitype1 == "png")
       
       png(file)
     
     else
       
       pdf(file)
     
     print(ggcompetingrisks(model_inci()$fit))+
       labs(x = "\n Survival Time", y = "Survival Probabilities \n")+
       theme(axis.text=element_text(size=14))+
       theme(axis.title.x  = element_text(size = 14))+
       theme(axis.title.y  = element_text(size = 14))+
       theme(legend.title = element_text(size = 14))+
       theme(legend.text = element_text(size = 14))+
       geom_line(size = 1)
     
     dev.off()
     
       }
   
 )
 
 
model_inci_group <- eventReactive(input$page_inci_group,{
  
  if(input$down_example == 'load_my_own2'){
    
    fit <- cuminc(ftime = unlist(v$data[input$compe_var1]), fstatus = unlist(v$data[input$compe_var2]), group = unlist(v$data[input$compe_var3]), cencode = input$late)
    
  }else{
    
    if(input$down_example == 'Example2'){
      
      fit <- cuminc(ftime = unlist(Melanoma[input$melanoma_var1]), fstatus = unlist(Melanoma[input$melanoma_var2]), group = unlist(Melanoma[input$melanoma_var3]), cencode = input$late)
      
    }
    
  }
  
  list(fit = fit)
  
}) 
 
output$comu_inci_group <- renderPlot({
  
  if(input$compe_var1 == "" & input$compe_var2 == "" & input$melanoma_var1 == "" & input$melanoma_var2 == "" & input$melanoma_var3 == ""){
    
    print(NULL)
    
  }else{
  
     ggcompetingrisks(model_inci_group()$fit)+
    labs(x = "\n Survival Time", y = "Survival Probabilities \n")+
    theme(axis.text=element_text(size=14))+
    theme(axis.title.x  = element_text(size = 14))+
    theme(axis.title.y  = element_text(size = 14))+
    theme(legend.title = element_text(size = 14))+
    theme(legend.text = element_text(size = 14))+
    geom_line(size = 1)
  
  }  
  
})  
  
  
output$inci_down2 <- downloadHandler(
  
  filename = function(){
    paste("incidence_gy_group", input$incitype2, sep = ".")
  },
  
  content = function(file){
    
    if(input$incitype2 == "png")
      
      png(file)
    
    else
      
      pdf(file)
    
    print(ggcompetingrisks(model_inci_group()$fit))+
      labs(x = "\n Survival Time", y = "Survival Probabilities \n")+
      theme(axis.text=element_text(size=14))+
      theme(axis.title.x  = element_text(size = 14))+
      theme(axis.title.y  = element_text(size = 14))+
      theme(legend.title = element_text(size = 14))+
      theme(legend.text = element_text(size = 14))+
      geom_line(size = 1)
    
     dev.off()
    
  }
  
)


output$print_inci_group <- renderPrint({
  
  if(input$compe_var1 == "" & input$compe_var2 == "" & input$compe_var3 == "" & input$melanoma_var1 == "" & input$melanoma_var2 == "" & input$melanoma_var3 == ""){
    
    print(NULL)
    
  }else{
  
    model_inci_group()$fit
  
  }
  
})


output$risk_reg <- renderPrint({
  
  if(input$down_example == 'load_my_own2'){
  
  if(input$regre == "sub_haz"){
  
    form <- as.formula(paste("~", paste(input$compe_reg, collapse = "+")))
    
  shr_fit <- 
    
    crr(
      
      ftime = unlist(v$data[input$compe_var1]),
      
      fstatus = unlist(v$data[input$compe_var2]),
      
      cov1 = model.matrix(form, data = v$data)[, -1],
     
      cencode = input$cenlevel,
      
      failcode = input$failcode
  
      )
  
  shr_fit
  
  }else{
    
    if(input$regre == "speci_haz"){
      
      a <- unlist(v$data[input$compe_var1])
      
      b <- unlist(v$data[input$compe_var2])
      
      formula<-as.formula(paste("Surv(a,b == input$text) ~ ",
                                    paste(input$compe_reg ,collapse = "+")
      ))
      
      chr_fit <- coxph(formula, data = v$data)
     
      chr_fit
      
    }
    
  }
    
  }else{
    
    if(input$down_example == 'Example2'){
      
      Melanoma$sex <- as.factor(Melanoma$sex)
      
      Melanoma$ulcer <- as.factor(Melanoma$ulcer)
      
      if(input$regre == "sub_haz"){
        
            form <- as.formula(paste("~", paste(input$compe_reg_melanoma, collapse = "+")))
            
             shr_fit <- 
    
                crr(
                
                ftime = unlist(Melanoma[input$melanoma_var1]),
                
                fstatus = unlist(Melanoma[input$melanoma_var2]),
                
                cov1 = model.matrix(form, data = Melanoma)[, -1],
                
                cencode = input$cenlevel,
             
                failcode =input$failcode)
            
            shr_fit
          
        }else{
          
        if(input$regre == "speci_haz"){
  
          a <- unlist(Melanoma[input$melanoma_var1])
          
          b <- unlist(Melanoma[input$melanoma_var2])
          
          formula<-as.formula(paste("Surv(a,b == input$text) ~ ",
                                    paste(input$compe_reg_melanoma ,collapse = "+")
          ))
          
          chr_fit <- coxph(formula, data = Melanoma)
          
          chr_fit
          
        }
              
            }
            
          }
       
       }  
  
  })


 
    
}

shinyApp(ui = ui, server = server)
