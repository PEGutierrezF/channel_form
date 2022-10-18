



# ---------------------------------------------
# Principal component analysis
# 20 Feb 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



channel.form =read.csv("data/physicochemistry.csv", header=T, row.names=1)


channel.pca <- prcomp(channel.form, center = TRUE, scale =TRUE)
summary(channel.pca)

# Variables
res.var <- get_pca_var(channel.pca)
res.var$coord          # Coordinates

fviz_pca_biplot(channel.pca,
                repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# ggplot  -----------------------------------------------------------------

# New names physicochemical var.
physico.names <- c("Elevacion.x" = "Elevation",
                   "Ancho"="Width",
                   "Velocidad"= "Velocity", 
                   "Rocas" = "Boulders",
                   "Canto"="Cobbles",
                   "grava"="Gravel",
                   "arena" = "Sand",
                   "Limo" = "Silt",
                   "NAtemp" = "Temperature",
                   "NaSatO2"="Diss. Oxygen") 
 
new.groups =read.csv("data/PCA_groups.csv")

PCA.biplot <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x)
  # Site names
  plot <- ggplot(data, aes_string(x=x, y=y)) + 
    geom_point(aes(colour = new.groups$group), size=5) +
    scale_color_manual(values=c("#67a9cf", "#91cf60", "#d73027"),
                       breaks=c("V-Shape", "Trapezoid", "U-Shape"),) +
    labs(x= "PC1 (32.4%)", y = "PC2 (16.5%)", colour ="Channel form") # Modifica con tus datos
  # Intercepts  
  plot <- plot + geom_hline(yintercept=0, size=.2,linetype="dashed") + 
    geom_vline(xintercept=0, size=.2,linetype="dashed") 
  
  # Loading table  
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min((max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
              (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x]))))
  datapc <- transform(datapc,v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y)))
  # Coordinates & loading names 
  plot <- plot + coord_equal() + ylim(-4,4) + xlim(-5,5) +
    geom_text(data=datapc, aes(x=v1, y=v2, label=physico.names), #varnames
              size = 4, vjust=0.5,hjust =0.5, color="black")
  # Arrows  
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                              arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="black")
  plot <- plot + theme_bw() +
   # theme(plot.margin = unit(c(1.2,1.2,1.2,1.2), "cm")) +
    theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
    theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y
    theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
    theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black"))  #subaxis y
  
  plot
}

Fig <- PCA.biplot(channel.pca)
Fig

ggsave("Figure 1.pdf", Fig, width = 200, height = 150, units = "mm")


stat_chull(aes(colour = new.groups$group), 
           alpha = 0.1, 
           geom = "polygon")

# https://stackoverflow.com/questions/18163153/convex-hulls-with-ggbiplot
