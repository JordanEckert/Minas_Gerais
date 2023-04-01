# The purpose of this script is to perform exploratory spatial data analysis (ESDA) on the Minas Gerais database
## This work is in conjunction with Nedret Billor and J.J. Lelis

# Attach packages - Data Cleaning
library(readxl)       # Loading excel dataset
library(tidyverse)    # Data manipulation
library(dplyr)        # Data manipulation
library(magrittr)     # Pipe operator

# Attach packages - Exploratory Data Analysis
library(umap)         # Uniform manifold approximations and projections for exploring clustering patterns
library(adegraphics)  # Graphical representation
library(spdep)        # Spatial dependency

# Attach packages - Spatial Analysis
library(pcds)         # Proximity catch digraphs for spatial classification
library(sp)           # Management of spatial data
library(terra)        # Spatial data analysis
library(ade4)         # Multivariate analysis
library(adespatial)   # Multivariate spatial analysis

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
datum$ID = as.numeric(datum$ID)
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
datum[,5:24] <- lapply(datum[,5:24], as.numeric)

## Now that the heavy metal contents part of the database is cleaned ...
## I can focus on cleaning the soil properties part ...

# Remove column separating heavy metal and soil properties
datum$...25 <- NULL

## ph KCl is a character variable, because of "-" token....

# Coerce ph KCL to be numeric
datum$`pH KCl` <- as.numeric(datum$`pH KCl`)

# Final checks for both datasets
View(datum)
View(PQL)

str(datum)

## Just in case it's needed for future reference we have:
## Heavy metal contents are in columns 5:24 ...
## Soil Properties are in columns 25:29 ....

#### Exploratory Spatial Data Analysis ####


