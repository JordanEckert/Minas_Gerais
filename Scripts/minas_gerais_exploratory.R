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
library(tidyverse)    # Data manipulation
library(magrittr)     # Pipe operator

# Loading database
datum <- read_excel("/Users/basecamp/DataspellProjects/Minas_Gerais/Database/Database.xlsx", skip = 1)

# Checking the structure of the database
str(datum)

## Known that a lot of the data will have <PQL values when the heavy metal is below limit ...
## This value is causing a lot of numeric variables to register as characters ...
## In literature, commonly use half of the PQL in the samples which registered heavy metal content below the PQL...

# Replacing '<PQL' values with half of the PQL for the heavy metal


# Coercing structures to be numeric where needed
## BELOW IS EXAMPLE CODE:
# data <- data.frame(matrix(sample(1:40), 4, 10, dimnames = list(1:4, LETTERS[1:10])))
# cols <- c("A", "C", "D", "H")
#
# data %<>% mutate_at(cols, factor)
