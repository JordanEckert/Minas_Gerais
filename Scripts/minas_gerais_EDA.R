# The purpose of this script is to perform exploratory data analysis on the Minas Gerais database
## This work is in conjunction with Nedret Billor and J.J. Lelis

library(mise)
mise()

## Common methodologies used ##
# PCA
# k-means clustering
# correlation matrix
# Variogram

## Global Functions

# ++++++++++++++++++++++++++++
# plotUMAP
# ++++++++++++++++++++++++++++
### Creates a plot for the UMAP projections ###

 plotUMAP = function(x, labels,
          main="UMAP Projections",
          colors=c("#ff7f00", "#e377c2", "#17becf", "purple", "black", "green"),
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
 
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
### Creates a flattened version of the correlation matrix since the matrix would be large ###
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row <- rownames(cormat)[row(cormat)[ut]],
    column <- rownames(cormat)[col(cormat)[ut]],
    cor  <-(cormat)[ut],
    p <- pmat[ut]
  )
}

# Attach packages - Data Cleaning
library(readxl)       # Loading excel dataset
library(tidyverse)    # Data manipulation
library(magrittr)     # Pipe operator

# Attach packages - Exploratory Spatial Data Analysis
library(leaflet)      # Creating geographic maps
library(corrplot)     # Correlation plots
library(Hmisc)        # Correlation analysis
library(adespatial)   # Multivariate variogram
library(geoR)         # Distance matrix

# Attach packages - Topological Data Analysis
library(umap)         # Uniform manifold approximations and projections for exploring clustering patterns


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
for (col in names(datum[,5:24])) {
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

# Coerce all newly changed columns to be numerics
datum[,4:24] <- lapply(datum[,4:24], as.numeric)

## Now that the heavy metal contents part of the database is cleaned ...
## I can focus on cleaning the soil properties part ...

# Remove column separating heavy metal and soil properties
datum$...25 <- NULL


## ph KCl is a character variable, because of "-" token....

# Coerce ph KCL to be numeric
datum$`pH KCl` <- as.numeric(datum$`pH KCl`)

### THERE NEEDS TO BE A DISCUSSION ON HOW TO HANDLE NA DATA ...

## Look at the column, if its 80% missing, then remove column

## Imputation the assumption is data is missing not at random ....
## Using correlation to help predict content from absent samples

# NA Values to 0 for datum
datum[,4:42][is.na(datum[,4:42])] <- 0

# Removing values that did not have a latitude, longitude coordinate
datum <- na.omit(datum)



# Final checks for both datasets
View(datum)
View(PQL)

str(datum)

## Just in case it's needed for future reference we have:
## Heavy metal contents are in columns 4:23 ...
## Soil Properties are in columns 24:42 ....

#### Exploratory Data Analysis - Graphics ####

# Map of each (complete) sampled point, hovering over gives the lab
map <- leaflet(datum) %>%
  addTiles() %>%
  addMarkers(lng = datum$Longitude, lat = datum$Latitude, label = datum$Lab)

map

# Boxplots of Heavy Metals by Lab
for(cols in colnames(datum[4:23])){
  boxplot(datum[[cols]] ~ datum$Lab, xlab = "Labs", ylab = paste(cols), main = paste("Boxplot of", cols))
}

# Boxplots of Soil Properties by Lab
for(cols in colnames(datum[24:42])){
  boxplot(datum[[cols]] ~ datum$Lab, xlab = "Labs", ylab = paste(cols), main = paste("Boxplot of", cols))
}

# Pairwise scatterplot for each metal and the soil properties
## NEEDS TO BE DONE STILL!

#### Exploratory Data Analysis - Correlation ####
# Correlation plots - Correlation of heavy minerals only
corr <- cor(datum[,4:23])
cor_plot <- corrplot(corr, type = "upper", method = "shade")
cor_plot

## From the correlation plot there seems to be some variables that might be more correlated ...
## Going to look at numerical summary to see which variables are most correlated ...

# Flattened Correlation Matrix - heavy minerals only
corr2 <- rcorr(as.matrix(datum[4:23]))
corr_flat <- flattenCorrMatrix(round(corr2$r,2), round(corr2$P,2))

# Which variables have moderate - strong correlation?
for (i in which(corr_flat$cor > .50, arr.ind = T)){
  print(corr_flat[i,-4])
}

## Next I want to see if there are correlations in just the soil properties ...

# Correlation Plots - Correlation of soil properties only
corr3 <- cor(datum[,24:42])
cor_plot2 <- corrplot(corr3, type = "upper", method = "shade")
cor_plot2

## Same as before, there seems to be some highly correlated soil properties ....
## Let's use same numerical summaries as before ...

# Flattened Correlation Matrix 
corr4 <- rcorr(as.matrix(datum[24:42]))
corr_flat2 <- flattenCorrMatrix(round(corr4$r,2), round(corr4$P,2))

# Which variables have moderate - strong correlation?
for (i in which(corr_flat2$cor > .50, arr.ind = T)){
  print(corr_flat2[i,-4])
}

## Finally, let's look at how the two correlate together ....

# Correlation Plots - Heavy Metals and Soil Properties 
corr5 <- cor(datum[,4:42])
corr5

cor_plot3 <- corrplot(corr5, type = "upper", method = "shade")
cor_plot3

corr6 <- rcorr(as.matrix(datum[4:42]))
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

#### Exploratory Data Analysis - Principal Components ####


#### Exploratory Data Analysis - Topological Data Analysis ####

## NOTE: May require tuning for number of neighbors, metric (maybe pearson instead of euclidean?),
## or just general tuning...
help(umap.defaults)     # For more information about default settings ....

## First, let's try to see if there are some natural structures in the data by UMAP ....

# Creating UMAP of heavy metals by Lab
datum.umap <- umap(datum[,4:23], labels = datum$Lab)

# Plot of heavy metals UMAP
plotUMAP(datum.umap, labels = datum$Lab)

# Individual UMAPs for each heavy metal by Lab
for(col in colnames(datum[,4:23])){
  datum.umap <- umap(as.matrix(datum[[col]]), labels = datum$Lab)
  plotUMAP(datum.umap, labels = datum$Lab, main = paste("UMAP Projection of", col))
}

# Creating UMAP of soil properties by Lab
datum.umap <- umap(datum[,4:23], labels = datum$Lab)

# Plot of soil properties UMAP
plotUMAP(datum.umap, labels = datum$Lab)

# Individual UMAPs for each soil property by Lab
for(col in colnames(datum[,24:42])){
  datum.umap <- umap(as.matrix(datum[[col]]), labels = datum$Lab)
  plotUMAP(datum.umap, labels = datum$Lab, main = paste("UMAP Projection of", col))
}

# Creating UMAP of heavy metals and soil properties by Lab
datum.umap <- umap(datum[,4:42], labels = datum$Lab)

# Plot of previous UMAP
plotUMAP(datum.umap, labels = datum$Lab)

# Distance matrix
dists <- as.matrix(dist(datum[,1:2]))
dists

# UMAP of Distances between observations
datum.umap <- umap(dists, input = "dist")
plotUMAP(datum.umap, labels = datum$Lab)

# Creating Zones based on latitude / longitutde
# UMAP based on Zoning
# Plot of zoned UMAP




