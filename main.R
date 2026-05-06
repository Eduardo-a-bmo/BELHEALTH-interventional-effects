#remove the library
rm(list=ls())

setwd("\\\\sciensano.be/fs/11403_LifeChron_Telehealth/EBMO/0_git/BELHEALTH")

##initialize libaries-----------

libs<-c("readxl","dplyr","data.table","ggplot2",
        "ggrepel","tidyverse","gee","lme4","ipw",
        "mediation","medflex","VGAM","finalfit",
        "mice","psych","haven","lavaan", "FactoMineR",
        "factoextra","xtable","table1","parallel","parabar","dplyr",
        "flextable","magrittr","table1","fastDummies",
        "officer","VIM")

#upload libraries needed 
for (pkg in libs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#Calculate the models
source("./scripts/interventional-effects-final.R")

#Descriptive
source("./scripts/descriptive.R")