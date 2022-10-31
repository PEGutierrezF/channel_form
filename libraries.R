



# ---------------------------------------------
# Libraries
# 20 Feb 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



libraries <- c("cowplot","cluster", "data.table","dplyr","factoextra", "ggplot2", 
               "ggpubr","gtable", "ggfortify","grid",
               'gridExtra',"labdsv","lemon","missForest","missMDA",'patchwork',
                "tidyr","vegan")

lapply(libraries, require, character.only = TRUE)


# Pairwise PERMANOVA
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis") 
library(pairwiseAdonis)


# Modify the y_scale by plot.  Individually 
devtools::install_github("teunbrand/ggh4x")
library(ggh4x)


library(missMDA) # Imputate
library(ggfortify) # autoplot()
library(cluster) #pam
library(factoextra) #get_pca_var()
library(data.table) # data.table()
library(labdsv) #loadings.pca(pca)

library(devtools)
install_github("vqv/ggbiplot") #ggbiplot
library(ggbiplot)
