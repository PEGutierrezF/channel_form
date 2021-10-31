



# ---------------------------------------------
# Channel form Percentage of Susbtrate
# 30 Oct 2021
# Pablo E. Guti�rrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

library(patchwork)
library(tidyr)
library(ggplot2)
library(dplyr)

data=read.csv("data/channelform.csv")
attach(data)


# Elevation ---------------------------------------------------------------


elevation_data <- data %>% filter(variable=='elevation')

elevation_data$streams <- factor(elevation_data$streams,
              levels = c("V-shape", "Trapezoid","U-shape"),ordered = TRUE)


E <- ggplot(elevation_data, aes(x=streams, y=value, fill=streams)) + 
  labs(title="",x="", y = "Stream elevation (masl)")+
  
  scale_fill_manual(values=c("#7570b3", "#d95f02", "#1f78b4")) +
  geom_violin()+
  geom_jitter(width=0.1,alpha=0.5)+
  theme_classic()+
  theme(legend.position="none") +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y
  
   
E

# Stream wide -------------------------------------------------------------

wide_data <- data %>% filter(variable=='wide')
wide_data1 <- na.omit(wide_data)

wide_data1$streams <- factor(wide_data1$streams,
                    levels = c("V-shape", "Trapezoid","U-shape"),ordered = TRUE)

W <- ggplot(wide_data1, aes(x=streams, y=value,  fill=streams)) + 
  labs(title="", x="", y = "Channel width (m)")+

  scale_fill_manual(values=c("#7570b3", "#d95f02", "#1f78b4")) +
  geom_violin()+
  geom_jitter(width=0.1,alpha=0.5)+
  theme_classic()+
  theme(legend.position="none") +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

W


# Water velocity ----------------------------------------------------------

velocity_data <- data %>% filter(variable=='velocity')
velocity_data1 <- na.omit(velocity_data)

velocity_data1$streams <- factor(velocity_data1$streams,
                             levels = c("V-shape", "Trapezoid","U-shape"),ordered = TRUE)

V <- ggplot(velocity_data1, aes(x=streams, y=value,  fill=streams)) + 
  labs(title="", x="", y = "Velocity (m/s)")+
  
  scale_fill_manual(values=c("#7570b3", "#d95f02", "#1f78b4")) +
  geom_violin()+
  geom_jitter(width=0.1,alpha=0.5)+
  theme_classic()+
  theme(legend.position="none") +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

V


# Boulder % -----------------------------------------------------------------

boulder_data <- data %>% filter(variable=='Boulder')
boulder_data1 <- na.omit(boulder_data)

boulder_data1$streams <- factor(boulder_data1$streams,
                                 levels = c("V-shape", "Trapezoid","U-shape"),ordered = TRUE)

B <- ggplot(boulder_data1, aes(x=streams, y=value,  fill=streams)) + 
  labs(title="", x="", y = "Boulder")+
  
  scale_fill_manual(values=c("#7570b3", "#d95f02", "#1f78b4")) +
  geom_violin()+
  geom_jitter(width=0.1,alpha=0.5)+
  theme_classic()+
  theme(legend.position="none") +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

B


# Cobble % ----------------------------------------------------------------

cobble_data <- data %>% filter(variable=='Cobble')
cobble_data1 <- na.omit(cobble_data)

cobble_data1$streams <- factor(cobble_data1$streams,
                    levels = c("V-shape", "Trapezoid","U-shape"),ordered = TRUE)

C <- ggplot(cobble_data1, aes(x=streams, y=value,  fill=streams)) + 
  labs(title="", x="", y = "Cobble")+
  
  scale_fill_manual(values=c("#7570b3", "#d95f02", "#1f78b4")) +
  geom_violin()+
  geom_jitter(width=0.1,alpha=0.5)+
  theme_classic()+
  theme(legend.position="none") +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

C


# Gravel ------------------------------------------------------------------


gravel_data <- data %>% filter(variable=='Gravel')
gravel_data1 <- na.omit(gravel_data)

gravel_data1$streams <- factor(gravel_data1$streams,
                               levels = c("V-shape", "Trapezoid","U-shape"),ordered = TRUE)

G <- ggplot(gravel_data1, aes(x=streams, y=value,  fill=streams)) + 
  labs(title="", x="Channel form", y = "Gravel")+
  
  scale_fill_manual(values=c("#7570b3", "#d95f02", "#1f78b4")) +
  geom_violin()+
  geom_jitter(width=0.1,alpha=0.5) +
  theme_classic()+
  theme(legend.position="none") +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

G


# Sand --------------------------------------------------------------------


sand_data <- data %>% filter(variable=='sand')
sand_data1 <- na.omit(sand_data)

sand_data1$streams <- factor(sand_data1$streams,
                               levels = c("V-shape", "Trapezoid","U-shape"),ordered = TRUE)

S <- ggplot(sand_data1, aes(x=streams, y=value,  fill=streams)) + 
  labs(title="", x="Channel form", y = "Sand")+
  
  scale_fill_manual(values=c("#7570b3", "#d95f02", "#1f78b4")) +
  geom_violin()+
  geom_jitter(width=0.1,alpha=0.5)+
  theme_classic()+
  theme(legend.position="none") +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

S


# Silt --------------------------------------------------------------------

silt_data <- data %>% filter(variable=='silt')
silt_data1 <- na.omit(silt_data)

silt_data1$streams <- factor(silt_data1$streams,
                             levels = c("V-shape", "Trapezoid","U-shape"),ordered = TRUE)

Si <- ggplot(silt_data1, aes(x=streams, y=value,  fill=streams)) + 
  labs(title="", x="Channel form", y = "Silt")+
  
  scale_fill_manual(values=c("#7570b3", "#d95f02", "#1f78b4")) +
  geom_violin()+
  geom_jitter(width=0.1,alpha=0.5)+
  theme_classic()+
  theme(legend.position="none") +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

Si



Fig <- (E+W+V+B+C+G+S+Si) + plot_annotation(tag_levels = 'A')
Fig

Fig + ggsave("Figure 1.jpg",width = 8.5, height = 11, units = "in", dpi=300)




library(tidyverse)

# First, order variables
data$variable = factor(data$variable, levels=c('elevation','wide','velocity',
                        'Boulder', "Cobble","Gravel", "sand", "silt"))

data$streams <- factor(data$streams,
                levels = c("V-shape", "Trapezoid","U-shape"),ordered = TRUE)


p <- ggplot(data, aes(x = streams, y = value, fill = streams))+
  geom_violin() + 
  geom_jitter(width=0.1,alpha=0.5) +
  labs(title="", x="Channel form", y = "value") +
  
  stat_summary(fun = "mean", geom = "crossbar", width = 0.4, colour = "#91003f") +
  
  scale_fill_manual(values=c("#5ab4ac", "#fc8d59", "#4575b4")) +
  
  theme_bw() +
  theme(legend.position="none") +
  theme(strip.text.x = element_text(size=11, color="black", face="bold")) +

  theme(axis.title.x = element_text(size = 16, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 16, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  facet_wrap(.~variable, scales="free_y",  
  
    labeller = labeller(variable=c( #second, rename variables
  'elevation' = "Stream elevation (masl)",
  'wide' = "Channel wide (m)",
  'velocity' = "Water velocity (m/s)",
  'Boulder' = "Boulder (%)",
  'Cobble' = "Cobble (%)",
  'Gravel' = "Gravel (%)",
  'sand' = "Sand (%)",
  'silt' = "Silt (%)")), strip.position = "top") 
  
p1 <- p +   theme(strip.placement = 'outside') +
  theme(strip.switch.pad.grid = unit(8, "cm"))

p1

p + ggsave("Figure 2.jpg",width = 8.5, height = 11, units = "in", dpi=300)


