# The purpose of this script is to perform spatial multivariate, multiscale data analysis  on the Minas Gerais database
## This work is in conjunction with Nedret Billor and J.J. Lelis
### More details on data cleaning process / ESDA in minas_gerais_exploratory.R

library(mise)
mise()

# Attach packages - Data Cleaning
library(readxl)       # Loading excel dataset
library(tidyverse)    # Data manipulation
library(magrittr)     # Pipe operator

# Attach packages - Spatial Analysis
library(adespatial)   # Multiscale analysis of spatial multivariate data

# Attach packages - Topological Data Analysis
library(umap)         # Uniform manifold approximations and projections for exploring clustering patterns
library(pcds)         # Proximity catch digraphs for spatial classification
library(adespatial)   # Multivariate variogram


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

# Changing character to numerics in PQL
PQL <- PQL %>% mutate_at(c("UFV1", "UFOP", "CETEC", "UFV_2"), as.numeric)

# Change any introduced PQL NA to 0
PQL[is.na(PQL)] <- 0

# Changing given PQL values to be the half values we will use
Elemento = as.factor(PQL$Elemento)
PQL <- cbind(Elemento, PQL[,2:ncol(PQL)]/2)

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

# Remove column separating heavy metal and soil properties
datum$...25 <- NULL

# Coerce ph KCL to be numeric
datum$`pH KCl` <- as.numeric(datum$`pH KCl`)

# NA Values to 0 for datum
datum[,4:42][is.na(datum[,4:42])] <- 0

# Removing values that did not have a latitude, longitude coordinate
datum = na.omit(datum)


## Just in case it's needed for future reference we have:
## Heavy metal contents are in columns 4:23 ...
## Soil Properties are in columns 24:42 ....

