## This work is in conjunction with Nedret Billor and J.J. Lelis
## This is the analysis used for the paper on spatial analysis of the Minas Gerais region

# Attach packages - Data Cleaning
library(readxl)                   # Loading excel dataset
library(tidyverse)                # Data manipulation
library(dplyr)                    # Pipe operator
library(janitor)                  # Clean variable names
library(ggplot2)                  # Data visualization

# Attach packages - Moran Eigenvector Maps
library(ade4)
library(adespatial)
library(adegraphics)
library(spdep)
library(sp)

## This work is in conjunction with Nedret Billor and J.J. Lelis
## This is the analysis used for the paper on spatial analysis of the Minas Gerais region

datum <- read.csv("~/DataspellProjects/Minas_Gerais/Database/datum.csv")
shape <- st_read("~/DataspellProjects/Minas_Gerais/Database/Lithology/MinasGerais_lito.shp")

colnames(datum)[colnames(datum) == "V...."] <- "V_Percent"
colnames(datum)[colnames(datum) == "m...."] <- "m_Percent"

datum$Zones <- as.factor(datum$Zones)
datum$Lab <- as.factor(datum$Lab)

str(datum)

#### Moran's Maps ####

mxy <- as.matrix(datum[,c(2,1)])
rownames(mxy) <- NULL
s.label(mxy, ppoint.pch = 15, ppoint.col = "darkgreen")

# Spatial Point Data Frame
sp.data <- SpatialPointsDataFrame(coords = mxy, data = datum[,-c(1:2, 4)])
s.Spatial(sp.data, nclass = 4)

Arsenic <- as.data.frame(datum$As)
colnames(Arsenic) <- "Arsenic"

sp.Ar <- SpatialPointsDataFrame(coords = mxy, data = Arsenic)
s.Spatial(sp.Ar, ppoint.pch = 15, ppoint.col = "darkgreen")

# Building Spatial Neighborhood on Gabriel Graph
nbgab <- graph2nb(gabrielneigh(mxy), sym = TRUE)
s.label(mxy, nb = nbgab, ppoint.pch = 16, 
        ppoint.col = "black", pnb.edge.col = "red")

## Using Gabriel graph for its low percentage of nonzero weights b/c
## more local relationships expected than global

# Defining Spatial Weighting Matrix
nb2listw(nbgab)   

distgab <- nbdists(nbgab, mxy)
fdist <- lapply(distgab, function(x) 1 - x/max(dist(mxy)))

listwgab <- nb2listw(nbgab, glist = fdist)
listwgab

print(listw2mat(listwgab)[1:10, 1:10], digits = 3) # Checking matrix

# Moran Eigenvector Maps
mems <- mem(listwgab)
mems

# orthobasis <- as.data.frame(mems)
# write.csv(orthobasis, file = "~/DataspellProjects/Minas_Gerais/Results for Paper/orthobasis.csv")

barplot(attr(mems, "values"), 
        main = "Eigenvalues of the spatial weighting matrix", cex.main = 0.7)

plot(mems[,c(1, 2, 5, 10, 25, 50)], SpORcoords = mxy)

maps.ar <- s.Spatial(sp.Ar)

# Testing Moran's I
moranI <- moran.randtest(mems, listwgab, 99)
moranI

# Moran Coefficient of Spatial Autocorrelation
MC.lith <- moran.randtest(datum[,-c(1:4)], listwgab, 99)
MC.lith

mc.bounds <- moran.bounds(listwgab)
mc.bounds

lith.maps <- s1d.barchart(MC.lith$obs, labels = MC.lith$names, 
                          plot = FALSE, xlim = 1.1 * mc.bounds, 
                          paxes.draw = TRUE, pgrid.draw = FALSE)
addline(lith.maps, v = mc.bounds, 
        plot = TRUE, pline.col = 'red', pline.lty = 3)

# Decomposing Moran's I
NP.As <- moranNP.randtest(datum$As, listwgab, nrepet = 999, alter = "two-sided") 
NP.As

plot(NP.As)

# MULTISPATI
pca.datum <- dudi.pca(datum[,c(19, 25:43)], scale = TRUE, scannf = FALSE)
adegraphics::screeplot(pca.datum, scannf = FALSE, main = "Scree Plot")

moran.randtest(pca.datum$li, listw = listwgab)
ms.datum <- adespatial::multispati(pca.datum, listw = listwgab, scannf = FALSE)
s.value(mxy, pca.datum$li)

ms.datum <- multispati(pca.datum, listw = listwgab, scannf = F)
summary(ms.datum)

g.ms.spe <- s.arrow(ms.datum$c1, plot = TRUE)
g.abund <- s.value(mxy, datum[,c(19, 25, 27, 42)], symbol = "circle", col = c("black", "palegreen4"), 
                   plegend.drawKey = FALSE, ppoint.cex = 0.4, plot = TRUE)
p1 <- list(c(0.05, 0.65), c(0.01, 0.25), c(0.74, 0.58), c(0.55, 0.05))
for (i in 1:4){
  g.ms.spe <- insert(g.abund[[i]], g.ms.spe, posi = p1[[i]], ratio = 0.25, plot = FALSE)
}

g.ms.spe
