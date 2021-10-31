



# ---------------------------------------------
# Channel form Percentage of Susbtrate
# 30 Oct 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

library(patchwork)
library(tidyr)
library(ggplot2)
library(dplyr)

data=read.csv("data/channelform.csv")
attach(data)
data
summary(data)

elevation_data <- data %>% filter(variable=='elevation')

E <- ggplot(elevation_data, aes(x=streams, y=value)) + 
  labs(title="Elevacion",x="Forma", y = "Elevacion")+
  geom_violin() +  
  geom_jitter(shape=16, position=position_jitter(0.2))+ 
  theme_classic()


wide_data <- data %>% filter(variable=='wide')
wide_data1 <- na.omit(wide_data)

W <- ggplot(wide_data1, aes(x=streams, y=value)) + 
  labs(title="Wide", x="Forma", y = "Channel width")+
  geom_violin() +  
  geom_jitter(shape=16, position=position_jitter(0.2))+ 
  theme_classic()


velocity_data <- data %>% filter(variable=='velocity')
velocity_data1 <- na.omit(velocity_data)

V <- ggplot(velocity_data1, aes(x=streams, y=value)) + 
  labs(title="velocity", x="Forma", y = "Velocity")+
  geom_violin() +  
  geom_jitter(shape=16, position=position_jitter(0.2))+ 
  theme_classic()



rocks_data <- data %>% filter(variable=='rocks')
rocks_data1 <- na.omit(rocks_data)

R <- ggplot(rocks_data1, aes(x=streams, y=value)) + 
  labs(title="Boulder", x="Forma", y = "Boulder")+
  geom_violin() +  
  geom_jitter(shape=16, position=position_jitter(0.2))+ 
  theme_classic()


canto_data <- data %>% filter(variable=='canto')
canto_data1 <- na.omit(canto_data)

C <- ggplot(canto_data1, aes(x=streams, y=value)) + 
  labs(title="Canto", x="Forma", y = "Canto")+
  geom_violin() +  
  geom_jitter(shape=16, position=position_jitter(0.2))+ 
  theme_classic()

grava_data <- data %>% filter(variable=='grava')
grava_data1 <- na.omit(grava_data)

G <- ggplot(grava_data1, aes(x=streams, y=value)) + 
  labs(title="Grava", x="Forma", y = "Grava")+
  geom_violin() +  
  geom_jitter(shape=16, position=position_jitter(0.2))+ 
  theme_classic()

sand_data <- data %>% filter(variable=='sand')
sand_data1 <- na.omit(sand_data)

S <- ggplot(sand_data1, aes(x=streams, y=value)) + 
  labs(title="Sand", x="Forma", y = "Sand")+
  geom_violin() +  
  geom_jitter(shape=16, position=position_jitter(0.2))+ 
  theme_classic()

silt_data <- data %>% filter(variable=='silt')
silt_data1 <- na.omit(silt_data)

Si <- ggplot(silt_data1, aes(x=streams, y=value)) + 
  labs(title="Silt", x="Forma", y = "Silt")+
  geom_violin() +  
  geom_jitter(shape=16, position=position_jitter(0.2))+ 
  theme_classic()

E+W+V+R+C+G+S+Si

R+C+G+S+Si



head(data)

b <- select(data, -streams)
summary(b)

c <- b%>%
pivot_wider(names_from = variable, values_from = value)

d <- select(c, -count)
summary(d)

res.comp <- imputePCA(d, ncp=2, scale = TRUE, method = c("Regularized","EM"), 
                      row.w = NULL, ind.sup=NULL,quanti.sup=NULL,quali.sup=NULL,
                      coeff.ridge = 1, threshold = 1e-06, seed = NULL, nb.init = 1,  
                      maxiter = 1000)


canal <- prcomp(res.comp$completeObs, center = TRUE, scale = TRUE)
summary(canal)

autoplot(canal,loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3,data=d)


res.var <- get_pca_var(canal)
res.var$contrib  


label=read.csv("label.csv")
head(label)

fviz_pca_var(canal,
             col.var = "contrib", # Color by contributions to the PC
             repel = TRUE)     # Avoid text overlapping

fviz_pca_biplot(canal, 
                col.ind = label$streams, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Chanel form")
