# The purpose of this script is to perform exploratory spatial data analysis (ESDA) on the Minas Gerais database
## This work is in conjunction with Nedret Billor and J.J. Lelis

# Attach packages
library(umap)         # Uniform manifold approximations and projections for exploring clustering patterns
library(sp)           # Management of spatial data
library(ade4)         # Multivariate analysis
library(adegraphics)  # Graphical representation
library(spdep)        # Spatial dependency
library(adespatial)   # Multivariate spatial analysis
library(readxl)       # Loading excel dataset

# Loading database
database <- read_excel("/Users/basecamp/DataspellProjects/Minas_Gerais/Database/Database.xlsx", skip = 1)

# Structure of database
str(database)

## From the structure, we see that the heavy metals are classified as characters (because of <PCL values)
##


