



# ---------------------------------------------
# Libraries
# 20 Feb 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())

library(factoextra)
library(ggplot2)

libraries <- c("ggplot2", "ggpubr","dplyr", 'patchwork',
               'gridExtra',"cowplot","missForest","factoextra",
               "labdsv","data.table","ggfortify")
lapply(libraries, require, character.only = TRUE)


library(missMDA) # Imputate
library(ggfortify) # autoplot()
library(cluster) #pam
library(factoextra) #get_pca_var()
library(data.table) # data.table()
library(labdsv) #loadings.pca(pca)

library(devtools)
install_github("vqv/ggbiplot") #ggbiplot
library(ggbiplot)
