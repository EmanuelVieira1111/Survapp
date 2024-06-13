source("global.R")

ui_about_module <- function(id) {
  ns <- NS(id)
  
  tabPanel("About",
           #    wellPanel(
           #   splitLayout(
           #  flowLayout(
           #  verticalLayout(
           sidebarLayout(
             sidebarPanel(
               #        img(src = "ECUM.png"),
               video("stats.mp4", width = 400, height = NA),
               br(),  
               p("Survapp was developed within the scope of the Master's
          dissertation in Statistics for Data Science at the University of Minho,
                    Portugal by", a(href= "https://www.linkedin.com/in/emanuel-vieira-3b519a177/" ,"Emanuel Vieira Monteiro da Silva"),
                 "under the supervision of Professor" , a(href = "https://w3.math.uminho.pt/~lmachado/", "Luis Filipe Meira Machado"), "and Professor
Gustavo Soutinho.",style = "color:black"),
               
              # div(img(src = "pic1.jpg",height = 250, width = 200,alt = "Emanuel",align = "center"),style="text-align: center;"), 
               #     howler::howlerModuleUI(
               #        id = "sound2",
               #        files = list(
               #          "Author message" = "gravafinal.m4a"
               #        )
               #      ),      
               br(),
               #tags$video(src = "movie.mp4", width = "300px",height="100px", type = "video/mp4", control = "controls"),
               p("This is a shiny application developed using RStudio, a free software integrated development environment for R.",style = "color:black"),
               br(),
               #    img(src = "posit.jpg",height = 70, width = 150),
               br(),
               p("Shiny and Rstudio are a product of ", 
                 span("posit", style = "color:blue"), style = "color:black"),
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
                                         p("The Kaplan-Meier estimator (Kaplan and Meier, 1958), is the standard method
                                                             used to estimate the survival function from lifetime data in the presence of
                                                             right censoring. In medical research, it is often used to measure the fraction
                                                             of patients living for a certain amount of time after enrolment in study.
                                                             In other fields, this estimator can be used to measure the length of time people
                                                             remain unemployed after a job loss, the time-to-failure of machine parts, etc. ",style = "color:black" ),  
                                         p("Kaplan, E. L. and Meier, P. (1958). Nonparametric estimation from incomplete
                                                             observations. J. Amer. Statist. Assoc. 53 (282): 457-481.",style = "color:black" ),
                                         p("The first step to performing a survival analysis is to specify the variables of interest.
                                         This is done in the 'Survival Curves' menu, in which the user must always indicate:
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
               ))))
  
  
}
