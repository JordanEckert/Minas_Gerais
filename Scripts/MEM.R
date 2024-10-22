## This work is in conjunction with Nedret Billor and J.J. Lelis
## This is the analysis used for the paper on spatial analysis of the Minas Gerais region

# Attach packages - Data Cleaning
library(readxl)                   # Loading excel dataset
library(tidyverse)                # Data manipulation
library(dplyr)                    # Pipe operator
library(janitor)                  # Clean variable names

# Attach packages - Moran Eigenvector Maps
library(ade4)
library(adespatial)
library(adegraphics)
library(spdep)

## This work is in conjunction with Nedret Billor and J.J. Lelis
## This is the analysis used for the paper on spatial analysis of the Minas Gerais region

# Attach packages - Data Cleaning
library(readxl)                   # Loading excel dataset
library(tidyverse)                # Data manipulation
library(dplyr)                    # Pipe operator
library(janitor)                  # Clean variable names

# Attach packages - Exploratory Analysis
library(leaflet)                  # Creating geographic maps
library(corrplot)                 # Correlation plots
library(gstat)                    # Variogram
library(adespatial)               # Multivariate variogram
library(ggplot2)                  # Graphical interface suite

# Attach packages - Spatial Random Forest
library(spatialRF)                # Spatial Random Forest
library(kableExtra)               # HTML Tables Exported 
library(randomForestExplainer)    # Variable Importance

# Attach packages - Moran Eigenvector Maps
library(spdep)                    # Nearest neighbor calculations
library(ade4)                     # Multispati 

#### Data Cleaning ####
# Loading main database
datum <- read_excel("~/DataspellProjects/Minas_Gerais/Database/Database.xlsx", skip = 1)

# PQL limits file
PQL <- read_excel("~/DataspellProjects/Minas_Gerais/Database/PQL.xlsx", skip = 1)
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
