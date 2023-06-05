# The purpose of this script is to perform exploratory data analysis on the Minas Gerais database
## This work is in conjunction with Nedret Billor and J.J. Lelis

# Attach packages - Data Cleaning
library(readxl)       # Loading excel dataset
library(tidyverse)    # Data manipulation
library(magrittr)     # Pipe operator

# Attach packages
library(GWmodel)      # Geographically weighted models
library(sp)           # Data management
library(spdep)        # Spatial autocorrelation
library(gstat)        # Geostatistics
library(RColorBrewer) # Visualization
library(classInt)     # Class intervals
library(gridExtra)    # Multiple plot
library(ggplot2)      # Multiple plo
library(leaflet)      # Creating geographic maps
library(corrplot)     # Correlation plots
library(Hmisc)        # Correlation analysis
library(ade4)         # Spatial analysis suite
library(adespatial)   # Spatial analysis suite
library(adegraphics)  # Spatial plotting suite
library(sf)           # Spatial analysis suite
library(umap)         # UMAP algorithm

#### Data Cleaning ####
# Loading main database
datum <- read_excel("/Users/basecamp/DataspellProjects/Minas_Gerais/Database/Database.xlsx", skip = 1)

# PQL limits file
PQL <- read_excel("/Users/basecamp/DataspellProjects/Minas_Gerais/Database/PQL.xlsx", skip = 1)
PQL <- PQL[-c(26:32),]  # Removing extra information not needed in R

# Checking the structure of the databases
str(datum)
str(PQL)

# Changing descriptive variables

## AFTER ZONATION, ADD COORDINATES FOR UFOP LAB BASED ON ZONE MIDDLE

datum$ID <- NULL
datum$Lab <- as.factor(datum$Lab)
datum$Zones <- as.factor(datum$Zones)
datum$Longitude <- as.numeric(datum$Longitude)
datum$Latitude <- as.numeric(datum$Latitude)

## NAs that were introduced where "-" values was...

## Known that a lot of the data will have <PQL values when the heavy metal is below limit ...
## This value is causing a lot of numeric variables to register as characters ...
## The goal then is to replace the <PQL values first ....
## Therefore, we need to do some slight cleaning with the PQL limits to make this easier...
## NOTE: `UFV_1' was changed to `UFV1' in the Excel document to match easier here...

# Changing character to numerics in PQL
PQL <- PQL %>% mutate_at(c("UFV1", "UFOP", "CETEC", "UFV_2"), as.numeric)

# Change any introduced PQL NA to 0
PQL[is.na(PQL)] <- 0

## In literature, commonly use half of the PQL in the samples which registered heavy metal content below the PQL...

# Changing given PQL values to be the half values we will use
Elemento <- as.factor(PQL$Elemento)
PQL <- cbind(Elemento, PQL[,2:ncol(PQL)]/2)

## Now we are ready to replace <PQL values....

# Loop through each relevant column of data frame
for (col in names(datum[,5:25])) {
  # Check if the column contains a "<PQL" value
  if ("<PQL" %in% datum[[col]]) {
    # Identify where "<PQL" values are
    index <- which(datum[[col]] == "<PQL", arr.ind = T)
    for(i in index){
      # Find the PQL value needed
      pql_val <- PQL[PQL$Elemento == col, as.character(datum$Lab[i])]
      # Replace the "<PQL" value with the corresponding PQL value
      datum[i, col] <- as.character(pql_val)
    }
  }
}

View(datum)

# Coerce all newly changed columns to be numerics
datum[,5:25] <- lapply(datum[,5:25], as.numeric)

## Now that the heavy metal contents part of the database is cleaned ...
## I can focus on cleaning the soil properties part ...

# Remove column separating heavy metal and soil properties (column 25)
datum <- datum[,-25]

## ph KCl is a character variable, because of "-" token....

# Coerce ph KCL to be numeric
datum$`pH KCl` <- as.numeric(datum$`pH KCl`)

# NA Values to 0 for datum
datum[,5:43][is.na(datum[,5:43])] <- 0

# Removing values that did not have a latitude, longitude coordinate 
datum <- na.omit(datum)

# Final checks for both datasets
View(datum)
View(PQL)

str(datum)

## Just in case it's needed for future reference we have:
## Heavy metal contents are in columns 5:24 ...
## Soil Properties are in columns 25:43 ....

#### Exploratory Data Analysis - Graphics ####

# Leaflet map of each (complete) sampled point, hovering over gives the Zone
map <- leaflet(datum) %>%
  addTiles() %>%
  addMarkers(lng = datum$Longitude, lat = datum$Latitude, 
             label = datum$Zones)
map

# Boxplots of Heavy Metals by Zones
for(cols in colnames(datum[5:24])){
  boxplot(datum[[cols]] ~ datum$Zones, xlab = "Labs", ylab = paste(cols), main = paste("Boxplot of", cols))
}

# Boxplots of Soil Properties by Zones
for(cols in colnames(datum[25:43])){
  boxplot(datum[[cols]] ~ datum$Zones, xlab = "Labs", ylab = paste(cols), main = paste("Boxplot of", cols))
}

# Pairwise scatter plots for each metal and the soil properties
for(cols1 in colnames(datum[5:24])){
  for(cols2 in colnames(datum[25:43])){
    plot(x = datum[[cols2]], y = datum[[cols1]], 
         xlab = paste(cols2), ylab = paste(cols1), 
         main = paste("Scatterplot of", cols1, "versus", cols2))
  }
}

#### Exploratory Data Analysis - Correlation ####

# Flatten correlation function 
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row <- rownames(cormat)[row(cormat)[ut]],
    column <- rownames(cormat)[col(cormat)[ut]],
    cor  <-(cormat)[ut],
    p <- pmat[ut]
  )
}

# Correlation plots - Correlation of heavy minerals only
corr <- cor(datum[,5:24])
cor_plot <- corrplot(corr, type = "upper", method = "shade")
cor_plot

## From the correlation plot there seems to be some variables that might be more correlated ...
## Going to look at numerical summary to see which variables are most correlated ...

# Flattened Correlation Matrix - heavy minerals only
corr2 <- rcorr(as.matrix(datum[5:24]))
corr_flat <- flattenCorrMatrix(round(corr2$r,2), round(corr2$P,2))

# Which variables have moderate - strong correlation?
for (i in which(corr_flat$cor > .50, arr.ind = T)){
  print(corr_flat[i,-4])
}

## Next I want to see if there are correlations in just the soil properties ...

# Correlation Plots - Correlation of soil properties only
corr3 <- cor(datum[,25:43])
cor_plot2 <- corrplot(corr3, type = "upper", method = "shade")
cor_plot2

## Same as before, there seems to be some highly correlated soil properties ....
## Let's use same numerical summaries as before ...

# Flattened Correlation Matrix 
corr4 <- rcorr(as.matrix(datum[24:43]))
corr_flat2 <- flattenCorrMatrix(round(corr4$r,2), round(corr4$P,2))

# Which variables have moderate - strong correlation?
for (i in which(corr_flat2$cor > .50, arr.ind = T)){
  print(corr_flat2[i,-4])
}

## Finally, let's look at how the two correlate together ....

# Correlation Plots - Heavy Metals and Soil Properties 
corr5 <- cor(datum[,5:43])
corr5

cor_plot3 <- corrplot(corr5, type = "upper", method = "shade")
cor_plot3

corr6 <- rcorr(as.matrix(datum[5:43]))
corr_flat3 <- flattenCorrMatrix(round(corr6$r,2), round(corr6$P,2))
corr_flat3

# Which variables have strong correlation?
for (i in which(corr_flat3$cor > .50, arr.ind = T)){
  print(corr_flat3[i,-4])
}

## Highest correlation between a metal and soil property is:
## Al     Clay 0.69

## Most of the high correlations seems to come from within groups ....
## Metals with metals, soil properties with soil properties ...
##
## However, all correlation analysis has not accounted for spatial autocorrelations ...
## Therefore, we need to perform new analysis accounting for spatial interactions ....
## This will be done in the minas_gerais_ESDA file since it requires changing data frame into SpatialPointDataFrame object ....

#### Exploratory Spatial Analysis - UMAP ####
plot.umap <- function(x, labels,
                      main="A UMAP visualization of the Iris dataset",
                      colors=c("#ff7f00", "#e377c2", "#17becf", "#336633", "#0000FF"),
                      pad=0.1, cex=0.6, pch=19, add=FALSE, legend.suffix="",
                      cex.main=1, cex.legend=0.85) {
  layout <- x
  if (is(x, "umap")) {
    layout <- x$layout
  } 

  xylim <- range(layout)
  xylim <- xylim + ((xylim[2]-xylim[1])*pad)*c(-0.5, 0.5)
  if (!add) {
    par(mar=c(0.2,0.7,1.2,0.7), ps=10)
    plot(xylim, xylim, type="n", axes=F, frame=F)
    rect(xylim[1], xylim[1], xylim[2], xylim[2], border="#aaaaaa", lwd=0.25)  
  }
  points(layout[,1], layout[,2], col=colors[as.integer(labels)],
         cex=cex, pch=pch)
         mtext(side=3, main, cex=cex.main)
        
  labels.u <- unique(labels)
  legend.pos <- "topleft"
  legend.text <- as.character(labels.u)
  if (add) {
    legend.pos <- "bottomleft"
    legend.text <- paste(as.character(labels.u), legend.suffix)
  }

  legend(legend.pos, legend=legend.text, inset=0.03,
         col=colors[as.integer(labels.u)],
         bty="n", pch=pch, cex=cex.legend)
}

# Tuning parameters
custom.config <- umap.defaults
custom.config$n_neighbors <- 500
custom.config$min_dist <- 1
custom.config$spread <- 1.5
custom.config$alpha <- .5
custom.config$negative_sample_rate <- 100

datum.umap <- umap(datum[,5:43], custom.config)
plot.umap(datum.umap, datum$Zones)

#### Exploratory Spatial Analysis - Variograms ####

for(i in 5:43){
  vario <- variogram(datum[,i], datum[,1:2])
  plot(vario$dist, vario$gamma,
       xlab = "Distance", ylab = "C(distance)", main = paste("Variogram for", colnames(datum[,i])))
}

# Multivariate Variogram for Metals- strictly equivalent to summing individual ones
multi_vario_metals = variogmultiv(datum[,5:24], datum[,1:2])
plot(multi_vario_metals$d, multi_vario_metals$var,type = 'b', pch = 20, xlab = "Distance", ylab = "C(distance)")

# Multivariate Variogram for Soil Property - strictly equivalent to summing individual ones
multi_vario_soil = variogmultiv(datum[,25:43], datum[,1:2])
plot(multi_vario_soil$d, multi_vario_soil$var,type = 'b', pch = 20, xlab = "Distance", ylab = "C(distance)")

# Multivariate Variogram for Total - strictly equivalent to summing individual ones
multi_vario = variogmultiv(datum[,5:43], datum[,1:2])
plot(multi_vario$d, multi_vario$var,type = 'b', pch = 20, xlab = "Distance", ylab = "C(distance)")

#### Spatial Data Analysis - Kriging ####

#### Spatial Data Analysis - Spatial Weighting Matrix ####

## First I need to turn my dataset into a spatial object for use in adepsatial and sf
## The plan is to go from a points sampling design to a get a rough polygon based off zones
## Then, we create a spatial neighborhood and the spatial weighting matrix to give the orthogonal spatial vectors
## Finally, we can perform spatial multivariate analysis using Moran's Eigenvalue Maps

# Define projection string and CRS
prj4string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
my_projection <- st_crs(prj4string)

# Convert datum into sf object
spatial_datum <- st_as_sf(datum, coords = c("Longitude", "Latitude"), crs = my_projection)

## Checking sf object
# str(spatial_datum)
# class(spatial_datum)
# st_is_valid(spatial_datum)

# Plot of zonations
plot(spatial_datum[,1])

# Plot of labs
plot(spatial_datum[,2])

# Create polygon area around each zone
buf <- st_buffer(spatial_datum, dist = 40000)
plot(buf[,1], pal = c("#ff7f00", "#e377c2", "#17becf", "#336633", "#0000FF"))
# plot.umap(datum.umap, datum$Zones)


# Creating spatial neighborhood 
nb.datum <- poly2nb(buf$geometry) # From polygons calculated above

# Simple row standardization for spatial weight matrix (SWM)
mxy <- as.matrix(datum[,c(1,2)]) # Matrix of coordinates
listwdatum <- nb2listw(nb.datum)

#### Spatial Data Analysis - Univariate Spatial Predictors ####

## Spatial predictors are orthogonal vectors stored in an object of class orthobasisSp
## Allows for Moran's Eigenvalue Maps (MEM) based on the diagonalization of a doubly-centered SWM
## MEMs maximize the Moran's coefficient of spatial autocorrelation

# Calculate MEM
mem.datum <- mem(listwdatum)
mem.datum

# Plot of first few relevant MEM
plot(mem.datum[,c(1, 2, 3, 4, 5, 10, 25, 50, 70)], SpORcoords = mxy)

# Moran's Coefficient
MC.datum <- moran.randtest(datum[5:43], listwdatum, alter = "two-sided", nrepet = 999)
MC.datum
  
## All but Se & Hg can reject the null hypothesis that the data is from a random point process. There is 
## spatial autocorrelation that leads to natural clustering for the elements and soil properties

# Moran's Coefficient Bounds
mc.bounds <- moran.bounds(listwdatum)
mc.bounds

# Graph of each individual variable
env.maps <- s1d.barchart(MC.datum$obs, labels = MC.datum$names, plot = TRUE, xlim = 1.1 * mc.bounds, paxes.draw = TRUE, pgrid.draw = FALSE)
addline(env.maps, v = mc.bounds, plot = TRUE, pline.col = 'red', pline.lty = 3)
  
# Decomposing Moran's Coefficient
NP.plot <- NULL
for(i in 5:43){
  NP.plot <- moranNP.randtest(datum[,i], listwdatum, nrepet = 999, alter = "two-sided")
  plot(NP.plot, main = paste("Moran Coefficient Decomposition for", colnames(datum[,i])))
}

## Positive spatial autocorrelation is when similar values cluster together on the map
## Negative spatial autocorrelation is when dissimilar values cluster together

## Previously looked at individual spatial structures on each variable separately separately.
## Now summarizing all the data by multivariate methods and then looking at final analysis 

#### Spatial Data Analysis - Principal Component Analysis ####

# Scaling data
data.scaled <- scale(datum[,5:43])

# Principal Component Analysis on Scaled Data
pca <- princomp(data.scaled, cor = FALSE)
(pca$sdev^2 / sum(pca$sdev^2)) * 100

# Loadings
pca$loadings

#### Spatial Data Analysis - Geographically Weighted PCA ####

# Need a Spatial Points DF
coords <- datum[,1:2]
scaled.spdf <- SpatialPointsDataFrame(coords, as.data.frame(data.scaled))

# Optimal bandwith 
bw.gw.pca <- bw.gwpca(scaled.spdf, 
                   vars = colnames(scaled.spdf@data),
                   k = 5,
                   robust = FALSE, 
                   adaptive = TRUE,
                   longlat = TRUE,
                   kernel = "bisquare")

# GW PCA
gw.pca<- gwpca(scaled.spdf, 
               vars = colnames(scaled.spdf@data), 
               bw = bw.gw.pca,
               k = 5, 
               robust = FALSE, 
               adaptive = TRUE)

# Loadings for PC1
gw.pca$loadings[,,1]

# Function to calculate proportion of variance for GWPCA
prop.var <- function(gwpca.obj, n.components) {
  return((rowSums(gwpca.obj$var[, 1:n.components]) /rowSums(gwpca.obj$var)) * 100)
}

var.gwpca <- prop.var(gw.pca, 3)
scaled.spdf$var.gwpca <- var.gwpca

# Visualization of Total Percent of Variances
state <- shapefile('~/DataspellProjects/Minas_Gerais/References/31MUE250GC_SIR.shp')
polys<- list("sp.lines", as(state, "SpatialLines"), col="grey", lwd=.8,lty=1)
col.palette<-colorRampPalette(c("blue",  "sky blue", "green","yellow", "red"),space="rgb",interpolate = "linear")

mypalette.4 <- brewer.pal(8, "Accent")

spplot(scaled.spdf, "var.gwpca", key.space = "right",
       col.regions = mypalette.4, cuts = 7, 
       sp.layout =list(polys),
       col="transparent",
       main = "Percent Total Variation for Local components 1 to 3")

# Loading visualization to see how each variable influences component
loadings.pc1 <- gw.pca$loadings[, , 1]
win.item = max.col(abs(loadings.pc1))
scaled.spdf$win.item <- win.item

mypalette <- c("lightpink", "blue", "grey", "purple",  "green")
spplot(scaled.spdf, "win.item", key.space = "right",
       col.regions = mypalette,
       main = "Winning variable: highest \n abs. loading on local Comp.1",
       sp.layout = list(polys))

# MC for PCA Scores
moran.randtest(pca$scores, listw = listwdatum, alter = "two-sided")

#### Spatial Data Analysis - MULTISPATI ####

## This is an alternate way of looking at spatial PCA. 
## Instead of geographical weighting, compute MC on the PCA scores
## and then searching directly for multivariate structure for axes that 
## maximize the product of variances by MC
## (Dray, Saïd, and Débias 2008)

# Creating dudi pca object
pca.dudi <- dudi.pca(data.scaled, scale = F, scannf = F, nf = 10)

# MULTISPATI
ms.datum <- multispati(pca.dudi, listw = listwdatum, scannf = F)
summary(ms.datum)

# Visualizations on first two PC
g.ms.spe <- s.arrow(ms.datum$c1, plot = FALSE)
g.ms.spe

#### Spatial Data Analysis - Multiscale Analysis with MEM ####
