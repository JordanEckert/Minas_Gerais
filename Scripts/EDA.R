## This work is in conjunction with Nedret Billor and J.J. Lelis
## This is the analysis used for the paper on spatial analysis of the Minas Gerais region

# Attach packages - Data Cleaning
library(readxl)                   # Loading excel dataset
library(tidyverse)                # Data manipulation
library(dplyr)                    # Pipe operator
library(janitor)                  # Clean variable names
library(sp)                       # Spatial
library(sf)                       # Shape files


# Attach packages - Exploratory Analysis
library(leaflet)                  # Creating geographic maps
library(corrplot)                 # Correlation plots
library(gstat)                    # Variogram
library(adespatial)               # Multivariate variogram
library(ggplot2)                  # Graphical interface suite

#### Data Cleaning ####
# Loading main database
datum <- read_excel("./Database/Database.xlsx", skip = 1)

# PQL limits file
PQL <- read_excel("./Database/PQL.xlsx", skip = 1)
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

write_csv(datum, "./Database/datum.csv")

# Shape file
shape <- st_read("./Database/Lithology/MinasGerais_lito.shp")
write_csv(shape, "./Database/shape.csv")

#### Exploratory Data Analysis ####

# Leaflet map of each (complete) sampled point, hovering over gives the Zone
map <- leaflet(datum) %>%
  addTiles() %>%
  addMarkers(lng = datum$Longitude, lat = datum$Latitude, 
             label = datum$Zones)
map

# Nonspatial correlation plot - Heavy Metals and Soil Properties 
corr5 <- cor(datum[,5:43])
corr5

cor_plot3 <- corrplot(corr5, type = "upper", method = "shade")
cor_plot3
title("Correlation Plot of Heavy Metals and Soil Properties", adj = 0, line = -25)

# Nonspatial correlation plot - Arsenic and Soil Properties
corr6 <- cor(datum[,c(19, 25:43)])
corr6

cor_plot4 <- corrplot(corr6, type = "upper", method = "shade")
cor_plot4
title("Correlation Plot of Arsenic and Soil Properties", adj = 0, line = -25)


## Highest correlation between a metal and soil property is:
## Al     Clay 0.69

## Most of the high correlations seems to come from within groups ....
## Metals with metals, soil properties with soil properties ...
##
## However, all correlation analysis has not accounted for spatial autocorrelations ...

# Variogram for Arsenic
vario <- variogram(datum[,19], datum[,1:2])
# Save current par settings to restore later if needed
op <- par(no.readonly = TRUE)

# Set margins and mgp (margin line for labels)
par(mar = c(5, 5, 4, 2), mgp = c(2.5, 0.7, 0)) 
# mgp[2] controls distance of axis title from axis (default is 1)

# Now plot
plot(
  vario$dist, vario$gamma,
  type = "b",
  pch = 19,
  cex = 1.4,
  lwd = 2,
  xlab = "Distance",
  ylab = expression(γ(h)),
  main = "Empirical Variogram for Arsenic",
  cex.axis = 1.2,
  cex.lab = 1.4,
  cex.main = 1.6
)

# Optional: restore original settings after
par(op)
# Variogram for soil properties
for(i in 25:43){
  vario <- variogram(datum[,i], datum[,1:2])
  plot(vario$dist, vario$gamma, type = "b", pch = 20,
       xlab = "Distance", ylab = "C(distance)", main = paste("Variogram for", colnames(datum[,i])))
}

# Multivariate Variogram for Soil Property - strictly equivalent to summing individual ones
# Save original parameters
op <- par(no.readonly = TRUE)

# Set margins and adjust mgp to move axis titles inward
par(mar = c(5, 5, 4, 2), mgp = c(2.5, 0.7, 0))

# Improved plot
plot(
  multi_vario_soil$d, multi_vario_soil$var,
  type = "b",
  pch = 19,             # Solid points
  cex = 1.5,            # Bigger points
  lwd = 2,              # Thicker line
  xlab = "Distance",
  ylab = expression(γ(h)),  # Conventional variogram notation
  main = "Multivariate Variogram for Arsenic and Soil Properties",
  cex.axis = 1.3,
  cex.lab = 1.5,
  cex.main = 1.7
)

# Restore parameters (optional)
par(op)
# Creating arsenic levels variable
datum$AsLevel <- as.factor(ifelse(datum$As >= 8.0, "High", "Low"))

# Count table for imbalance checks
table(datum$AsLevel)

## 202 out of the 696 are high levels of Arsenic (~30%)
## Is there a local imbalance of high levels?

# Create a scatter plot of As values across region
as_plot <- ggplot(datum, aes(x = Longitude, y = Latitude)) + 
  geom_point(aes(size = As, color = AsLevel), alpha = 0.7) + 
  scale_size_continuous(range = c(2, 10), name = "Arsenic Value") + 
  labs(
    title = "Distribution of Arsenic Values Across Minas Gerais Region",
    x = "Longitude", y = "Latitude", color = "Arsenic Level"
  ) + 
  theme_minimal(base_size = 14) +  # Set overall base font size
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),   # Center and bold title
    axis.title = element_text(size = 16),     # Axis titles
    axis.text = element_text(size = 14),      # Tick labels
    legend.title = element_text(size = 15),   # Legend title
    legend.text = element_text(size = 13)     # Legend labels
  )

# Print the scatter plot
print(as_plot)
