source('global.R')
# source("Kaplan.R")

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
     data_to_export <- switch(
       input$examples,
       "GBSG" = gbsg,
"IR_diabetes" = IR_diabetes,
      "Melanoma" = Melanoma,
      "colonIDM" = colonIDM,
      "iris" = iris,
      v$data
    )
    write.csv(data_to_export, file, row.names = FALSE)
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
  updateSelectInput(session, "time1", choices = names(colonIDM), selected = "")
  updateSelectInput(session, "event1", choices = names(colonIDM), selected = "")
  updateSelectInput(session, "Stime", choices = names(colonIDM), selected = "")
  updateSelectInput(session, "event", choices = names(colonIDM), selected = "")
})
  

output$str_dados <- renderPrint({
  data_to_print <- NULL
  if (input$down_example == 'Example2') {
  if (input$examples == "GBSG") {
    data_to_print <- gbsg
  } else if (input$examples == "IR_diabetes") {
    data_to_print <- IR_diabetes
  } else if (input$examples == "Melanoma") {
    data_to_print <- Melanoma
  } else if (input$examples == "colonIDM") {
    data_to_print <- colonIDM
  }} else {
    data_to_print <- v$data
  }
  
  str(data_to_print)
})

output$colname_in <- renderUI({
  req(v$data)
  selectInput(inputId = "colname",
              label = "Choose column",
              choices = c("",colnames(v$data)),
              selected = "")})


observeEvent(input$change_class, {
  v$data <- eval(parse(text = paste0('v$data %>% mutate(',
                                     input$colname,
                                     ' = as.',
                                     input$class,
                                     '(',
                                     input$colname,
                                     '))')
  ))})


output$tabela <- DT::renderDataTable({
  data_to_display <- NULL
  if (input$down_example == 'Example2') {
  if (input$examples == "GBSG") {
    data_to_display <- gbsg
  } else if (input$examples == "IR_diabetes") {
    data_to_display <- IR_diabetes
  } else if (input$examples == "Melanoma") {
    data_to_display <- Melanoma
  } else if (input$examples == "colonIDM") {
    data_to_display <- colonIDM
  }} else {
    data_to_display <- v$data
  }
  
  datatable(data_to_display)
})


output$summaryTable <- renderUI({
  data_to_summarize <- NULL
  if (input$down_example == 'Example2') {
  if (input$examples == "GBSG") {
    data_to_summarize <- gbsg
  } else if (input$examples == "IR_diabetes") {
    data_to_summarize <- IR_diabetes
  } else if (input$examples == "Melanoma") {
    data_to_summarize <- Melanoma
  } else if (input$examples == "iris") {
    data_to_summarize <- iris
  } else if (input$examples == "colonIDM") {
    data_to_summarize <- colonIDM
  }} else {
    data_to_summarize <- v$data
  }
  
  req(data_to_summarize)
  out <- print(dfSummary(data_to_summarize, graph.magnif = 0.8),
               style = 'grid', omit.headings = TRUE, method = 'render', bootstrap.css = FALSE)
  out
})
  
  observeEvent(v$data, {
    colnames_data <- colnames(v$data)
    names_data <- names(v$data)
    updateSelectInput(session, "variable1", choices = names_data, selected = "") 
    updateSelectInput(session, "variable2", choices = names_data, selected = "")
    updateSelectInput(session, "variable3", choices = names_data, selected = "")
    updateSelectInput(session, "variable4", choices = names_data, selected = "") 
    updateSelectInput(session, "variable5", choices = names_data, selected = "")
    updateSelectInput(session, "variable6", choices = names_data, selected = "")
    updateCheckboxGroupInput(session, "cox_var", choices = colnames_data)
    updateCheckboxGroupInput(session, "distribution", choices = c("weibull", "exponential", "lognormal", "loglogistic"))
    updateCheckboxGroupInput(session, "aft_variables", choices = colnames_data)
    updateCheckboxGroupInput(session, "rpart_var", choices = colnames_data)
    updateCheckboxGroupInput(session, "rpart_class_var", choices = colnames_data)
    updateSelectInput(session, "response", choices = names_data, selected = "")
    updateSelectInput(session, "compe_var1", choices = names_data, selected = "")
    updateSelectInput(session, "compe_var2", choices = names_data, selected = "")
    updateSelectInput(session, "compe_var3", choices = names_data, selected = "")
    updateCheckboxGroupInput(session, "compe_reg", choices = colnames_data)
    updateCheckboxGroupInput(session, "varforests", choices = colnames_data)
    updateSelectInput(session, "multi_var1", choices = names_data, selected = "") 
    updateSelectInput(session, "multi_var2", choices = names_data, selected = "")
    updateSelectInput(session, "multi_var3", choices = names_data, selected = "")
    updateSelectInput(session, "multi_var4", choices = names_data, selected = "")
  })
  
  
#  callModule(kaplan_server,"Kaplan")
  
  model <- eventReactive(input$kaplan, {
    fit <- NULL
    if (input$down_example == 'Example2') {
      if (input$examples == "GBSG") {
        fit <- survfit(Surv(unlist(gbsg[input$var1gbsg]), unlist(gbsg[input$var2gbsg])) ~ 1, data = gbsg)
      } else if (input$examples == "IR_diabetes") {
        fit <- survfit(Surv(unlist(IR_diabetes[input$var1diabetes]), unlist(IR_diabetes[input$var2diabetes]), type = "interval2") ~ 1, data = IR_diabetes)
      }
    } else {
      if (input$censor == "right") {
        tempo <- unlist(v$data[input$variable1])
        estado <- unlist(v$data[input$variable2])
        fit <- survfit(Surv(tempo, estado) ~ 1, data = v$data)
      } else if (input$censor == "interval") {
        tempo1 <- unlist(v$data[input$variable4])
        tempo2 <- unlist(v$data[input$variable5])
        fit <- survfit(Surv(tempo1, tempo2, type = "interval2") ~ 1, data = v$data)
      }
    }

    list(fit = fit)
  })

  
  output$graf <- renderPlotly({
    input_vars <- c("variable1", "variable2", "variable4", "variable5", 
                    "var1gbsg", "var2gbsg", "var1diabetes", "var2diabetes")
    if(all(sapply(input_vars, function(var) input[[var]] == ""))) {
      return(NULL)
    }
    plot <- autoplot(model()$fit, surv.colour = "#2a283d") +
      labs(x = "\n Survival Time", y = "Survival Probabilities \n") +
      theme(axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            panel.background = element_rect(fill = "#e5f9f4",
                                            colour = "lightblue", size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))
    ggplotly(plot)
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
  
  
  output$suma <- renderPlotly({
    input_vars <- c("variable1", "variable2", "variable4", "variable5", 
                    "var1gbsg", "var2gbsg", "var1diabetes", "var2diabetes")
    if(all(sapply(input_vars, function(var) input[[var]] == ""))) {
      return(NULL)
    }
    plot <- autoplot(model()$fit, surv.colour = "#2a283d", data = v$data, fun = "cumhaz") +
      labs(x = "\n Survival Time", y = "Cumulative Hazard \n") +
      theme(axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            panel.background = element_rect(fill = "#e5f9f4",
                                            colour = "lightblue", size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))
    ggplotly(plot)
  })
  
  
  output$down <- downloadHandler(
    filename = function() {
      paste("survival", input$choose, sep = ".")
    },
    content = function(file) {
      plot <- autoplot(model()$fit, surv.colour = "#2a283d") +
        labs(x = "\n Survival Time", y = "Survival Probabilities \n") +
        theme(axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              panel.background = element_rect(fill = "#e5f9f4",
                                              colour = "lightblue", size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"),
              panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))
      if (input$choose == "png") {
        png(file)
      } else {
        pdf(file)
      }
      print(ggplotly(plot))
      dev.off()
    }
  )
  
  
  output$down1 <- downloadHandler(
    filename = function() {
      paste("cumulative", input$choose1, sep = ".")
    },
    content = function(file) {
      plot <- autoplot(model()$fit, surv.colour = "#2a283d", data = v$data, fun = "cumhaz") +
        labs(x = "\n Survival Time", y = "Cumulative Hazard \n") +
        theme(axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              panel.background = element_rect(fill = "#e5f9f4",
                                              colour = "lightblue", size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"),
              panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))
      if (input$choose1 == "png") {
        png(file)
      } else {
        pdf(file)
      }
      print(ggplotly(plot))
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
  
  
  
  output$curves <- renderPlotly({
    input_vars <- c("variable1", "variable2", "variable3", "variable4", "variable5", 
                    "variable6", "var1gbsg", "var2gbsg", "var3gbsg", "var1diabetes", 
                    "var2diabetes", "var3diabetes")
    if (all(sapply(input_vars, function(var) input[[var]] == ""))) {
      return(NULL)
    }
    plot <- autoplot(model1()$km1) +
      labs(x = "\n Survival Time", y = "Survival Probabilities \n") +
      theme(axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            panel.background = element_rect(fill = "#e5f9f4",
                                            colour = "lightblue", size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))
    
    ggplotly(plot)
  })
  
  
  output$summary2 <- renderPrint({
    input_vars <- c("variable1", "variable2", "variable3", "variable4", "variable5", 
                    "variable6", "var1gbsg", "var2gbsg", "var3gbsg", "var1diabetes", 
                    "var2diabetes", "var3diabetes")
    if (all(sapply(input_vars, function(var) input[[var]] == ""))) {
      return(NULL)
    }
    summary(model1()$km1)
  })
  
  
  output$print <- renderPrint({
    input_vars <- c("variable1", "variable2", "variable3", "variable4", "variable5", 
                    "variable6", "var1gbsg", "var2gbsg", "var3gbsg", "var1diabetes", 
                    "var2diabetes", "var3diabetes")
    if (all(sapply(input_vars, function(var) input[[var]] == ""))) {
      return(NULL)
    }
    print(model1()$km1)
  })
  
  output$test <- renderPrint({
    input_vars <- c("variable1", "variable2", "variable3", "var1gbsg", 
                    "var2gbsg", "var3gbsg", "var1diabetes", "var2diabetes", "var3diabetes")
    if (all(sapply(input_vars, function(var) input[[var]] == ""))) {
      return(NULL)
    }
    model1()$km1_dif
  })
  
  output$testgehan <- renderPrint({
    input_vars <- c("variable1", "variable2", "variable3", "var1gbsg", 
                    "var2gbsg", "var3gbsg", "var1diabetes", "var2diabetes", "var3diabetes")
    if (all(sapply(input_vars, function(var) input[[var]] == ""))) {
      return(NULL)
    }
    model1()$km1_dif1
  })
  
  output$down2 <- downloadHandler(
    filename = function() {
      paste("covariate", input$choose2, sep = ".")
    },
    content = function(file) {
      plot <- autoplot(model1()$km1) +
        labs(x = "\n Survival Time", y = "Survival Probabilities \n") +
        theme(axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 14),
              panel.background = element_rect(fill = "#e5f9f4",
                                              colour = "lightblue", size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"),
              panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))
      if (input$choose2 == "png") {
        png(file)
      } else {
        pdf(file)
      }
      print(ggplotly(plot))
      dev.off()
    }
  )
  
  model_clus <- eventReactive(input$cluster_page, {
    withProgress(message = "Calculating the clusters", value = 10, {
      if (input$down_example == 'Example2') {
        fit.gbcs <- NULL
        if (input$examples == "GBSG") {
          gbsg[gbsg$nodes > 13, 'nodes'] <- 14
          fit.gbcs <- survclustcurves(time = unlist(gbsg[input$var1gbsg]),
                                      status = unlist(gbsg[input$var2gbsg]),
                                      x = unlist(gbsg[input$var3gbsg]),
                                      nboot = 20, seed = 300716, algorithm = 'kmedians',
                                      cluster = TRUE)
        }
      } else {
        a <- unlist(v$data[input$variable1])
        b <- unlist(v$data[input$variable2])
        c <- unlist(v$data[input$variable3])
        fit.gbcs <- survclustcurves(time = a, status = b, x = c,
                                    nboot = 50, seed = 300716, algorithm = 'kmedians',
                                    cluster = TRUE)
      }
      list(fit.gbcs = fit.gbcs)
    })
  })
  
  
  output$cluster <- renderPrint({
    input_vars <- c("variable1", "variable2", "variable3", "var1gbsg", "var2gbsg", "var3gbsg")
    if (all(sapply(input_vars, function(var) input[[var]] == ""))) {
      return(NULL)
    }
    summary(model_clus()$fit.gbcs)
  })
  
  
  output$clus <- renderPlot({
    input_vars <- c("variable1", "variable2", "variable3", "var1gbsg", "var2gbsg", "var3gbsg")
    if (all(sapply(input_vars, function(var) input[[var]] == ""))) {
      return(NULL)
    }
    autoplot(model_clus()$fit.gbcs, groups_by_color = TRUE) +
      labs(x = "\n Survival Time", y = "Survival Probabilities \n") +
      theme(axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14))
  })
  
  
  output$down_clust <- downloadHandler(
    filename = function() {
      paste("clusters_curves", input$clust, sep = ".")
    },
    content = function(file) {
      plot <- autoplot(model_clus()$fit.gbcs, groups_by_color = TRUE) +
        labs(x = "\n Survival Time", y = "Survival Probabilities \n") +
        theme(axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 14))
      if (input$clust == "png") {
        png(file)
      } else {
        pdf(file)
      }
      print(plot)
      dev.off()
    }
  )
  
  model_cox <- eventReactive(input$cox_page, {
    fit <- NULL
    if (input$down_example == 'Example2') {
      if (input$examples == "GBSG") {
        formula.cox <- as.formula(paste("Surv(rfstime, status)~", paste(input$cox_var_gbsg, collapse = "+")))
        fit <- coxph(formula.cox, data = gbsg)
      } else if (input$examples == "IR_diabetes") {
        fit <- ic_sp(Surv(left, right, type = "interval2") ~ unlist(IR_diabetes[input$cox_var_diabetes]), data = IR_diabetes)
        print(fit)
      }
    } else {
      if (input$censor == "right") {
        a <- unlist(v$data[input$variable1])
        b <- unlist(v$data[input$variable2])
        formula.cox <- as.formula(paste("Surv(a, b)~", paste(input$cox_var, collapse = "+")))
        fit <- coxph(formula.cox, data = v$data)
        print(fit)
        cat('\n')
        cat('AIC value of the model:\n')
        print(extractAIC(fit)[2])
      } else if (input$censor == "interval") {
        tempo1 <- unlist(v$data[input$variable4])
        tempo2 <- unlist(v$data[input$variable5])
        estatuto <- unlist(v$data[input$cox_var])
        fit <- ic_sp(Surv(tempo1, tempo2, type = 'interval2') ~ estatuto, data = v$data)
        print(fit)
      }
    }
    list(fit = fit)
  })
  
  output$cox <- renderPrint({
    if(is.null(input$cox_var_gbsg) & is.null(input$cox_var_diabetes) & input$variable1 == "" & input$variable2 == "" & input$variable4 == "" & input$variable5 == "" & is.null(input$cox_var)) {
      print(NULL)
    }else{
      #      Variable <- names(unlist(model_cox()$fit$assign))
      #      Hazard <- round(exp(model_cox()$fit$coefficients),3)
      #      tabela <- data.frame(Variable,Hazard)
      #      kable(tabela, "html") %>% kable_styling(bootstrap_options = "striped", full_width = T)   
      #      as.data.frame(tabela)
      print(model_cox()$fit)
    }})
  
  output$aic_cox <- renderPrint({
    cat('AIC value of the model:','\n')
    print(extractAIC(model_cox()$fit)[2])
    cat('BIC value of the model:', '\n')
    print(BIC(model_cox()$fit))
  })  
  
  
  output$coxzph <- renderPlot({
    if(is.null(input$cox_var_gbsg) & input$variable1 == "" & input$variable2 == "" & is.null(input$cox_var)) {
      print(NULL)
    }else{
      ggcoxzph(cox.zph(model_cox()$fit))
    }})
  
  output$aft_model <- renderPrint({
    if (
      is.null(input$aft_var_gbsg) && is.null(input$distribution_gbsg) &&
      input$variable1 == "" && input$variable2 == "" && input$variable4 == "" && input$variable5 == "" &&
      is.null(input$aft_variables) && is.null(input$distribution) &&
      is.null(input$aft_var_diabetes) && is.null(input$distribution_diabetes)
    ) {
      print(NULL)
    } else {
      if (input$down_example == 'Example2') {
        if (input$examples == "GBSG") {
          formula.aft <- as.formula(paste("Surv(rfstime,status)~", paste(input$aft_var_gbsg, collapse = "+")))
          fit.para <- survreg(formula.aft, data = gbsg, dist = input$distribution_gbsg)
          print(summary(fit.para))
          cat('\n')
          cat('AIC value of the model:\n')
          print(extractAIC(fit.para)[2])
          cat('BIC value of the model:', '\n')
          print(BIC(fit.para))
        } else if (input$examples == "IR_diabetes") {
          par_fit <- ic_par(cbind(left, right) ~ unlist(IR_diabetes[input$aft_var_diabetes]), data = IR_diabetes, dist = input$distribution_diabetes)
          summary(par_fit)
        }
      } else {
        if (input$censor == "right") {
          time <- unlist(v$data[input$variable1])
          status <- unlist(v$data[input$variable2])
          if (is.null(input$aft_variables)) {
            fit_0 <- survreg(Surv(time, status) ~ 1, data = v$data, dist = input$distribution)
            print(summary(fit_0))
            cat('\n')
            cat('AIC value of the model:\n')
            print(extractAIC(fit_0)[2])
            cat('BIC value of the model:', '\n')
            print(BIC(fit_0))
          } else {
            formula.aft_model <- as.formula(paste("Surv(time,status)~", paste(input$aft_variables, collapse = "+")))
            fit1 <- survreg(formula.aft_model, data = v$data, dist = input$distribution)
            print(summary(fit1))
            cat('\n')
            cat('AIC value of the model:\n')
            print(extractAIC(fit1)[2])
            cat('BIC value of the model:', '\n')
            print(BIC(fit1))
          }
        } else if (input$censor == "interval") {
          tempo1 <- unlist(v$data[input$variable4])
          tempo2 <- unlist(v$data[input$variable5])
          estatuto <- unlist(v$data[input$aft_variables])
          par_fit <- ic_par(cbind(tempo1, tempo2) ~ estatuto, data = v$data, dist = input$distribution)
          summary(par_fit)
        }
      }
    }
  })
  
  
model_rpart_reg <- eventReactive(input$page_rpart_reg, {
    if (input$down_example == 'Example2') {
      if (input$examples == "GBSG") {
        time <- unlist(gbsg[input$var1gbsg])
        status <- unlist(gbsg[input$var2gbsg])
        formula.rpart_gbsg <- as.formula(paste("Surv(time,status)~", paste(input$rpart_var_gbsg, collapse = "+")))
        set.seed(123)
        model <- rpart(formula.rpart_gbsg, data = gbsg)
        km <- survfit(Surv(time, status) ~ model$where, data = gbsg)
      } else {
        if (input$examples == "IR_diabetes") {
          model <- NULL
          km <- NULL
        }
      }
    } else {
      if (input$censor == "right") {
        if (length(input$rpart_var) == 0) {
          model <- NULL
          km <- NULL
        } else {
          time <- unlist(v$data[input$variable1])
          status <- unlist(v$data[input$variable2])
          formula.rpart_regre <- as.formula(paste("Surv(time,status)~", paste(input$rpart_var, collapse = "+")))
          set.seed(123)
          model <- rpart(formula.rpart_regre, data = v$data)
          km <- survfit(Surv(time, status) ~ model$where, data = v$data)
        }
      } else {
        if (input$censor == "interval") {
          model <- NULL
          km <- NULL
        }
      }
    }
    list(model = model, km = km)
  })
  
output$rpart_regression <- renderPlot({
  if (is.null(input$rpart_var_gbsg) & is.null(input$rpart_var) & input$variable1 == "" & input$variable2 == "" & input$var1gbsg == "" & input$var2gbsg == "") {
    print(NULL)
  } else {
    rpart.plot(model_rpart_reg()$model, type = 5, extra = 2, faclen = 0, under = TRUE, cex = 1.1)
  }
})

output$rpart_regression_print <- renderPrint({
  if (is.null(input$rpart_var_gbsg) & is.null(input$rpart_var) & input$variable1 == "" & input$variable2 == "" & input$var1gbsg == "" & input$var2gbsg == "") {
    print(NULL)
  } else {
    print(model_rpart_reg()$km)
  }
})

output$rpart_regression_summary <- renderPrint({
  if (is.null(input$rpart_var_gbsg) & is.null(input$rpart_var) & input$variable1 == "" & input$variable2 == "" & input$var1gbsg == "" & input$var2gbsg == "") {
    print(NULL)
  } else {
    print(summary(model_rpart_reg()$km))
  }
})
  
  
  model_rpart_class <- eventReactive(input$page_rpart_class,{
    if(input$down_example == 'Example2'){
      if(input$examples == 'iris'){
         set.seed(123)
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
    })
  
  
  output$rpart_classification <- renderPlot({  
     rpart.plot(model_rpart_class()$model$finalModel, type=5,extra = 2,faclen=0, under=TRUE,cex=1.1)
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
            }}}
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
    if (input$compe_var1 == "" && input$compe_var2 == "" && input$melanoma_var1 == "" && input$melanoma_var2 == "") {
      return(NULL)
    } else {
      ggcompetingrisks(model_inci()$fit) +
        labs(x = "\n Survival Time", y = "Probability of occurrence \n") +
        theme(
          axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14)
        ) +
        geom_line(size = 1)
    }
  })
  
  
  output$print_inci <- renderPrint({
    if (input$compe_var1 == "" && input$compe_var2 == "" && input$melanoma_var1 == "" && input$melanoma_var2 == "") {
      print(NULL)
    } else {
      model_inci()$fit
    }
  })
  
  
  output$inci_down1 <- downloadHandler(
    filename = function() {
      paste("incidence", input$incitype1, sep = ".")
    },
    
    content = function(file) {
      if (input$incitype1 == "png") {
        png(file)
      } else {
        pdf(file)
      }
      
      plot <- ggcompetingrisks(model_inci()$fit) +
        labs(x = "\n Survival Time", y = "Survival Probabilities \n") +
        theme(axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        geom_line(size = 1)
      
      print(plot)
      dev.off()
    }
  )
  
  
  model_inci_group <- eventReactive(input$page_inci_group, {
    if (input$down_example == 'load_my_own2') {
      fit <- cuminc(
        ftime = unlist(v$data[input$compe_var1]),
        fstatus = unlist(v$data[input$compe_var2]),
        group = unlist(v$data[input$compe_var3]),
        cencode = input$late
      )
    } else if (input$down_example == 'Example2') {
      fit <- cuminc(
        ftime = unlist(Melanoma[input$melanoma_var1]),
        fstatus = unlist(Melanoma[input$melanoma_var2]),
        group = unlist(Melanoma[input$melanoma_var3]),
        cencode = input$late
      )
    } else {
      fit <- NULL
    }
    
    list(fit = fit)
  })
  
  output$comu_inci_group <- renderPlot({
    if (input$compe_var1 == "" && input$compe_var2 == "" && input$melanoma_var1 == "" && input$melanoma_var2 == "" && input$melanoma_var3 == "") {
      print(NULL)
    } else {
      ggcompetingrisks(model_inci_group()$fit) +
        labs(x = "\n Survival Time", y = "Probability of occurrence \n") +
        theme(axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        geom_line(size = 1)
    }
  }) 
  
  
  output$inci_down2 <- downloadHandler(
    filename = function() {
      paste("incidence_gy_group", input$incitype2, sep = ".")
    },
    
    content = function(file) {
      if (input$incitype2 == "png") {
        png(file)
      } else {
        pdf(file)
      }
      
      plot <- ggcompetingrisks(model_inci_group()$fit) +
        labs(x = "\n Survival Time", y = "Survival Probabilities \n") +
        theme(axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 14)) +
        geom_line(size = 1)
      
      print(plot)
      dev.off()
    }
  )
  
  
  output$print_inci_group <- renderPrint({
    if (input$compe_var1 == "" && input$compe_var2 == "" && input$compe_var3 == "" &&
        input$melanoma_var1 == "" && input$melanoma_var2 == "" && input$melanoma_var3 == "") {
      print(NULL)
    } else {
      model_inci_group()$fit
    }
  })
  
  
  output$risk_reg <- renderPrint({
    if (input$down_example == 'load_my_own2') {
      if (input$regre == "sub_haz") {
        form <- as.formula(paste("~", paste(input$compe_reg, collapse = "+")))
        shr_fit <- crr(
          ftime = unlist(v$data[input$compe_var1]),
          fstatus = unlist(v$data[input$compe_var2]),
          cov1 = model.matrix(form, data = v$data)[, -1],
          cencode = input$cenlevel,
          failcode = input$failcode
        )
        shr_fit
      } else if (input$regre == "speci_haz") {
        a <- unlist(v$data[input$compe_var1])
        b <- unlist(v$data[input$compe_var2])
        formula <- as.formula(paste("Surv(a, b == input$text) ~ ", paste(input$compe_reg, collapse = "+")))
        chr_fit <- coxph(formula, data = v$data)
        chr_fit
      }
    } else if (input$down_example == 'Example2') {
      Melanoma$sex <- as.factor(Melanoma$sex)
      Melanoma$ulcer <- as.factor(Melanoma$ulcer)
      if (input$regre == "sub_haz") {
        form <- as.formula(paste("~", paste(input$compe_reg_melanoma, collapse = "+")))
        shr_fit <- crr(
          ftime = unlist(Melanoma[input$melanoma_var1]),
          fstatus = unlist(Melanoma[input$melanoma_var2]),
          cov1 = model.matrix(form, data = Melanoma)[, -1],
          cencode = input$cenlevel,
          failcode = input$failcode
        )
        shr_fit
      } else if (input$regre == "speci_haz") {
        a <- unlist(Melanoma[input$melanoma_var1])
        b <- unlist(Melanoma[input$melanoma_var2])
        formula <- as.formula(paste("Surv(a, b == input$text) ~ ", paste(input$compe_reg_melanoma, collapse = "+")))
        chr_fit <- coxph(formula, data = Melanoma)
        chr_fit
      }
    }
  })
} 