suppressPackageStartupMessages(
  c(library(shiny),
    library(summarytools),
    library(DT),
    library(ggplot2),
    library(dplyr),
    library(broom),
    library(shinydashboard),
    library(leaflet),
    library(shinythemes),
    library(plotly),
    library(survival),
    library(survminer),
    library(vtable),
    library(smoothHR),
    library(grDevices),
    library(periscope),
    library(ggfortify),
 #   library(coxinterval),
    library(smoothHR),
    library(rpart),
    library(plotmo),
    library(rpart.plot),
    library(icenReg),
    library(cmprsk),
#    library(clustcurv),
    library(mvna),
    library(vctrs),
    library(mstate),
    library(tree),
    library(caret),
    library(htmltools),
    library(ranger),
    library(MASS),
    library(shinybusy),
    library(shinyjs),
    library(survidm),
    library(shinyWidgets),
    library(htmlwidgets),
    library(htmltools),
    #library(shinyauthr)
    library(ggthemes),
    library(knitr),
    library(kableExtra),
    library(magrittr),
    library(video),
    library(howler),
    library(purrr)
  )
)

data("IR_diabetes", package = "icenReg")



gbsg$grade <- as.factor(gbsg$grade)
gbsg$meno <- as.factor(gbsg$meno)
gbsg$hormon <- as.factor(gbsg$hormon)



IR_diabetes$gender <- as.factor(IR_diabetes$gender)

Melanoma$sex <- as.factor(Melanoma$sex)
Melanoma$ulcer <- as.factor(Melanoma$ulcer)

# colonIDM$sex <- as.factor(colonIDM$sex)
# colonIDM$differ <- as.factor(colonIDM$differ)
# colonIDM$extent <- as.factor(colonIDM$extent)
# colonIDM$surg <- as.factor(colonIDM$surg)
# colonIDM$node4 <- as.factor(colonIDM$node4)
