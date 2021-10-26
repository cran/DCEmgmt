## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE------------------------------------------------------------
load("survey.rda")
knitr::kable(cbind("B1_1" = data[1:10,1:1], "..." = c("...", "...","...","...","...","...","...","...","...", "..."), data[1:10,16:21])) 

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  install.packages("DCEmgmt") #Install DCEmgmt from the CRAN repository
#  library(DCEmgmt) #Load the DCEmgmt package
#  data(survey) #Load the original database (It will appear as "data" in the RStudio environment)

## ---- echo = FALSE------------------------------------------------------------
load("coded.rda")
knitr::kable(data.c[1:10,1:11], row.names = FALSE)

## ---- echo = FALSE------------------------------------------------------------
load("coded.rda")
knitr::kable(rbind(cbind(data.c[1:10, 1:3], "..." = c("...", "...", "...", "...", "...", "...", "...", "...", "...", "..."), data.c[1:10,10:23]),c("...","...","...","...","...","...","...","...","...","...","...","...","...","..."),cbind(data.c[1:10, 1:3], "..." = c("...", "...", "...", "...", "...", "...", "...", "...", "...", "..."), data.c[3820:3824,10:23])), row.names = FALSE)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  install.packages("DCEmgmt") #Install DCEmgmt from the CRAN repository
#  library(DCEmgmt) #Load the DCEmgmt package
#  data(survey) #Load the original database (It will appear as "data" in the RStudio environment)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  install.packages("DCEmgmt") #Install DCEmgmt from the CRAN repository
#  library(DCEmgmt) #Load the DCEmgmt package
#  data(design) #Load the design matrix

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  install.packages("DCEmgmt") #Install DCEmgmt from the CRAN repository
#  library(DCEmgmt) #Load the DCEmgmt package
#  data(survey) #Load the database
#  data(design) #Load the design matrix
#  data.c <- DCEmgmt::DCEmgmt(data = data, keepvar = c("sex", "height", "weight", "age", "educ"), create.id = TRUE, blocks = c("B1_", "B2_"), sets = 8, alts = 2, options = c("Prefiero el servicio A", "Prefiero el servicio B"), design = design)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  DCEestm(data.c = data.c, model = "all", params = c("pers2", "pers3", "pers4", "form2", "rut2", "prec2", "prec3", "prec4"), rand = c("pers2", "pers3", "pers4", "form2", "rut2", "prec2", "prec3", "prec4"))

