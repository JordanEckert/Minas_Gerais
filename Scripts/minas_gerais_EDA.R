# The purpose of this script is to perform exploratory spatial data analysis (ESDA) on the Minas Gerais database
## This work is in conjunction with Nedret Billor and J.J. Lelis

library(mise)
mise()

## Global Functions
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
### Creates a flattened version of the correlation matrix since the matrix would be large ###
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
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

# Attach packages - Topological Data Analysis
library(umap)         # Uniform manifold approximations and projections for exploring clustering patterns
library(pcds)         # Proximity catch digraphs for spatial classification

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
datum$ID = NULL
datum$Lab = as.factor(datum$Lab)
datum$Longitude = as.numeric(datum$Longitude)
datum$Latitude = as.numeric(datum$Latitude)

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
Elemento = as.factor(PQL$Elemento)
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
### DOES THE DATA NEED TO BE IMPUTED? IS NA ESSENTIALLY 0?
### RIGHT NOW NA ARE TREATED AS 0, IF THIS IS NOT RIGHT NEED TO CHANGE ANALYSIS ...

# NA Values to 0 for datum
datum[,4:42][is.na(datum[,4:42])] <- 0

# Removing values that did not have a latitude, longitude coordinate
datum = na.omit(datum)

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
corr = cor(datum[,4:23])
cor_plot = corrplot(corr, type = "upper", method = "shade")
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
corr3 = cor(datum[,24:42])
cor_plot2 = corrplot(corr3, type = "upper", method = "shade")
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
corr5 = cor(datum[,4:42])
corr5

cor_plot3 = corrplot(corr5, type = "upper", method = "shade")
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

multi_vario = variogmultiv(datum[,4:23], datum[,1:3])
plot(multi_vario$d, multi_vario$var,type = 'b', pch = 20, xlab = "Distance", ylab = "C(distance)")

#### CODE FOR SINGLE VARIABLE VARIOGRAM
# # Transform data frame into a SpatialPointDataFrame
# coordinates(datum)=~ Longitude + Latitude
# class(datum)
#
# # Variogram
# vario_datum = variogram(B ~ 1, data = datum)
# plot(vario_datum)

