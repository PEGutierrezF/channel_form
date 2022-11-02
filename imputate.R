



# ---------------------------------------------
# Data imputation
# 02 Nov 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())

channel <- read.csv("data/PCA_channel_form.csv", header=TRUE)


channel_1 <- dplyr::select(channel, -Forma)
summary(channel_1)


# Imputate ----------------------------------------------------------------

df1 <- dplyr::select(channel_1, Elevacion, Ancho, Velocidad, Rocas, 
                     Canto, grava, arena, Limo)

df1_a <- missForest(df1, maxiter = 4, ntree = 100,
                    variablewise = TRUE, decreasing = FALSE, verbose = F, replace = TRUE,
                    classwt = NULL, cutoff = NULL, strata = NULL,
                    sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                    xtrue = NA, parallelize = "no")

df2_a <- dplyr::select(channel_1, Elevacion, NAtemp, NASatO2)


# New data frame ----------------------------------------------------------

new_channel <- do.call("merge", c(lapply(list(df1_a$ximp, df2_a), data.frame, 
                                         row.names=NULL), by = 0, all = TRUE, sort = FALSE))[-1]
#remove 1 elevation
new_channel_1 <- dplyr::select(new_channel, -Elevacion.y)


write.csv(new_channel_1, "data/physicochemistry.csv")

