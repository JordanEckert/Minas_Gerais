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

shapefile <- st_read("~/DataspellProjects/Minas_Gerais/Database/Lithology/MinasGerais_lito.shp")
plot(st_geometry(shapefile))

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
plot(vario$dist, vario$gamma, type = "b", pch = 20,
     xlab = "Distance", ylab = "C(distance)", main = paste("Variogram for Arsenic"))

# Variogram for soil properties
for(i in 25:43){
  vario <- variogram(datum[,i], datum[,1:2])
  plot(vario$dist, vario$gamma, type = "b", pch = 20,
       xlab = "Distance", ylab = "C(distance)", main = paste("Variogram for", colnames(datum[,i])))
}

# Multivariate Variogram for Soil Property - strictly equivalent to summing individual ones
multi_vario_soil = variogmultiv(datum[,25:43], datum[,1:2])
plot(multi_vario_soil$d, multi_vario_soil$var,type = 'b', pch = 20, xlab = "Distance", ylab = "C(distance)", main = "Multivariate Variogram for Soil Properties")

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
  labs(title = "Distribution of Arsenic Values Across Minas Gerais Region",
       x = "Longitude", y = "Latitude", color = "Arsenic Level") + 
  theme_minimal()

# Print the scatter plot
print(as_plot)

#### Spatial Random Forest ####

# It's picky about the column names, so I rename variables in columns 25 to 43 by removing spaces.
col_indices <- 25:43
new_col_names <- gsub(" ", "_", colnames(datum)[col_indices])  # Replace spaces with underscores

## Naming will be changed back when doing further analysis sections for simplicity
## Allows code to be run in sections in case specific analysis is targeted

# Rename the columns
colnames(datum)[col_indices] <- new_col_names

# Distance Matrices with and without latitude (without latitude and longitude only used for graphics)
dist_mat <- as.matrix(dist(datum[,c(1,2,19,25:43)]))
dist_mat2 <- as.matrix(dist(datum[,c(19,25:43)]))

# Extract relevant columns for model training
predictor_columns <- c("Latitude", "Longitude", colnames(datum)[25:43])
predictor_columns2 <- c(colnames(datum)[25:43])

# Plot of Moran Maps across thresholds
spatialRF::plot_training_df_moran(
  data = datum[,c(19, 25:43)],
  dependent.variable.name = "As",
  predictor.variable.names = predictor_columns2,
  distance.matrix = dist_mat,
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1), 
  point.color = "black")


## NOTE: Lower p-values and Moran's I values indicate there is no spatial 
## autocorrelation for given variable and distance threshold

# Rename Latitude to x and Longitude to y required in package
colnames(datum)[colnames(datum) == "Latitude"] <- "x"
colnames(datum)[colnames(datum) == "Longitude"] <- "y"

# Coordinates of the cases
xy <- datum[, c("x", "y")]

# Train the non-spatial random forest model
model.non.spatial <- spatialRF::rf(
  data = datum[,c(1,2,19, 25:43)],
  dependent.variable.name = "As",
  predictor.variable.names = colnames(datum[,c(1,2,25:43)]),
  distance.matrix = as.matrix(dist(datum[,c(1,2,19,25:43)])),
  xy = xy,
  scaled.importance = TRUE,
  seed = 2023,
  verbose = TRUE
)

## Variable importance represents the increase in mean error 
## (computed on the out-of-bag data) across trees when a predictor is permuted.

## Values lower than zero would indicate that the variable performs worse than 
## a random one.

# Variable Importance of nonspatial random forest through randomForestExplainer
importance.df <- randomForestExplainer::measure_importance(
  model.non.spatial,
  measures = c("mean_min_depth", "no_of_nodes", "times_a_root", "p_value")
)

kableExtra::kbl(
  importance.df %>% 
    dplyr::arrange(mean_min_depth) %>% 
    dplyr::mutate(p_value = round(p_value, 4)),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

# Contribution of predictors to model transferability - spatial folds measured with rf_evaluate()
model.non.spatial <- spatialRF::rf_importance(
  model = model.non.spatial
)

model.non.spatial$importance$per.variable %>% 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = importance.oob,
    y = importance.cv
  ) + 
  ggplot2::geom_point(size = 3) + 
  ggplot2::theme_bw() +
  ggplot2::xlab("Importance (out-of-bag)") + 
  ggplot2::ylab("Contribution to transferability") + 
  ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "red4")

## Importance and contribution show very little correlation with each other, 
## this indicates that the importance measures seem to capture the same aspects
## of the effects of the variables on the model results

# Model performance
spatialRF::print_performance(model.non.spatial)

# Spatial cross-validation
model.non.spatial <- spatialRF::rf_evaluate(
  model = model.non.spatial,
  xy = xy,                  #data coordinates
  repetitions = 30,         #number of spatial folds
  training.fraction = 0.75, #training data fraction on each fold
  metrics = "r.squared",
  verbose = TRUE
)

# Plot of spatial cross-validation
spatialRF::plot_evaluation(model.non.spatial)

# Comparison of different spatial folds
pr <- datum[, c("x", "y")]
pr$group.2 <- pr$group.1 <- "Training"
pr[model.non.spatial$evaluation$spatial.folds[[1]]$testing, "group.1"] <- "Testing"
pr[model.non.spatial$evaluation$spatial.folds[[25]]$testing, "group.2"] <- "Testing"

p1 <- ggplot2::ggplot() +
  ggplot2::geom_point(data = pr,
                      ggplot2::aes(
                        x = x,
                        y = y,
                        color = group.1
                      ),
                      size = 2
  ) +
  ggplot2::scale_color_viridis_d(
    direction = -1, 
    end = 0.5, 
    alpha = 0.8, 
    option = "F"
  ) +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Group") +
  ggplot2::scale_x_continuous() +
  ggplot2::scale_y_continuous()  +
  ggplot2::ggtitle("Spatial Fold 1") + 
  ggplot2::theme(
    legend.position = "none", 
    plot.title = ggplot2::element_text(hjust = 0.5)
  ) + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

p2 <- ggplot2::ggplot() +
  ggplot2::geom_point(data = pr,
                      ggplot2::aes(
                        x = x,
                        y = y,
                        color = group.2
                      ),
                      size = 2
  ) +
  ggplot2::scale_color_viridis_d(
    direction = -1, 
    end = 0.5, 
    alpha = 0.8, 
    option = "F"
  ) +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Group") +
  ggplot2::scale_x_continuous() +
  ggplot2::scale_y_continuous() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5)
  ) + 
  ggplot2::ggtitle("Spatial Fold 25") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("")

p1 | p2

# Plot of Multiscale Moran's I
spatialRF::plot_moran(
  model.non.spatial, 
  verbose = FALSE
)

# Need to clean the dataset names and rerun everything first
datum <- datum %>% clean_names()

# Train the non-spatial random forest model under new names
model.non.spatial <- spatialRF::rf(
  data = datum[,c(1,2,19, 25:43)],
  dependent.variable.name = "as",
  predictor.variable.names = colnames(datum[,c(1,2,25:43)]),
  distance.matrix = as.matrix(dist(datum[,c(1,2,19,25:43)])),
  xy = xy,
  seed = 2023,
  verbose = TRUE
)

model.non.spatial <- spatialRF::rf_importance(
  model = model.non.spatial
)

# Spatial Model
model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  method = "mem.moran.sequential", #default method
  verbose = TRUE,
  scaled.importance = TRUE
)

# Hyperparameter Tuning - Results indicate that all possible results increase spatial autocorrelation 
# model.spatial <- rf_tuning(
#  model = model.spatial,
#  xy = xy,
#  repetitions = 30,
#  num.trees = c(500, 1000),
#  mtry = seq(
#    2,
#    length(model.spatial$ranger.arguments$predictor.variable.names), #number of predictors
#    by = 5),
#  min.node.size = c(5, 10),
#  verbose = TRUE
# )

# Spatial Residual Diagnostics
spatialRF::plot_residuals_diagnostics(
  model.spatial,
  verbose = FALSE
)

# Variable Importance of spatial random forest through randomForestExplainer
importance.df <- randomForestExplainer::measure_importance(
  model.spatial,
  measures = c("mean_min_depth", "no_of_nodes", "times_a_root", "p_value")
)

kableExtra::kbl(
  importance.df %>% 
    dplyr::arrange(mean_min_depth) %>% 
    dplyr::mutate(p_value = round(p_value, 4)),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

# Contribution of predictors to model transferability - spatial folds measured with rf_evaluate()
model.spatial <- spatialRF::rf_importance(
  model = model.spatial
)

model.spatial$importance$per.variable %>% 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = importance.oob,
    y = importance.cv
  ) + 
  ggplot2::geom_point(size = 3) + 
  ggplot2::theme_bw() +
  ggplot2::xlab("Importance (out-of-bag)") + 
  ggplot2::ylab("Contribution to transferability") + 
  ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "red4")

# Local variable importance
local.importance <- spatialRF::get_importance_local(model.spatial)

# Response Curves - Not Graphically interesting results with data shown
spatialRF::plot_response_curves(
  model.spatial,
  quantiles = c(0.1, 0.5, 0.9),
  line.color = viridis::viridis(
    3, #same number of colors as quantiles
    option = "F", 
    end = 0.9
  ),
  ncol = 4,
  show.data = FALSE
)

# Individual Response curves at three different quantiles
spatialRF::plot_response_curves(
  model.spatial,
  quantiles = 0.1,
  ncol = 3
)

spatialRF::plot_response_curves(
  model.spatial,
  quantiles = 0.5,
  ncol = 3
)

spatialRF::plot_response_curves(
  model.spatial,
  quantiles = 0.9,
  ncol = 3
)

# Response surfaces
spatialRF::plot_response_surface(
  model.spatial,
  a = "clay",
  b = "cec"
)

# Model Performance
spatialRF::print_performance(model.spatial)

# Spatial Cross Validation
model.spatial <- spatialRF::rf_evaluate(
  model = model.spatial,
  xy = xy,                  #data coordinates
  repetitions = 30,         #number of spatial folds
  training.fraction = 0.75, #training data fraction on each fold
  metrics = "r.squared",
  verbose = FALSE
)

spatialRF::plot_evaluation(model.spatial)

spatialRF::print_evaluation(model.spatial)

# Moran's I
spatialRF::plot_moran(
  model.spatial, 
  verbose = FALSE
)



# Comparison of importance
p1 <- spatialRF::plot_importance(
  model.non.spatial, 
  verbose = FALSE) + 
  ggplot2::ggtitle("Non-spatial model") 

p2 <- spatialRF::plot_importance(
  model.spatial,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model")

p1 | p2 

# Model Transferability
model.spatial <- spatialRF::rf_importance(
  model = model.spatial
)

# 10 most important variables in spatial model
kableExtra::kbl(
  head(model.spatial$importance$per.variable, n = 10),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

# Map of Spatial Predictors
spatial.predictors <- spatialRF::get_spatial_predictors(model.spatial)
pr <- data.frame(spatial.predictors, datum[, c("x", "y")])

p1 <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = pr,
    ggplot2::aes(
      x = x,
      y = y,
      color = spatial_predictor_175_1
    ),
    size = 2.5
  ) +
  ggplot2::scale_color_viridis_c(option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Eigenvalue") +
  ggplot2::scale_x_continuous() +
  ggplot2::scale_y_continuous()  +
  ggplot2::ggtitle("Most Important Spatial Predictor from Neighborhood Distance 175 (1st Overall)") + 
  ggplot2::theme(legend.position = "bottom")+ 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

p2 <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = pr,
    ggplot2::aes(
      x = x,
      y = y,
      color = spatial_predictor_0_34,
    ),
    size = 2.5
  ) +
  ggplot2::scale_color_viridis_c(option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Eigenvalue") +
  ggplot2::scale_x_continuous() +
  ggplot2::scale_y_continuous()  +
  ggplot2::ggtitle("Most Important Spatial Predictor from Neighborhood Distance 0 (4th Overall)") + 
  ggplot2::theme(legend.position = "bottom") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("")

p1 | p2

# Selection of Optimal Spatial Predictors
p <- spatialRF::plot_optimization(model.spatial)

#### Moran's Maps ####






# Spatial Dataframe
prj4string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
my_projection <- st_crs(prj4string)

# Convert datum into sf object
spatial_datum <- st_as_sf(datum, coords = c("x", "y"), crs = my_projection)

# Create polygon area around each zone
buf <- st_buffer(spatial_datum, dist = 35100)
plot(buf[,1], pal = c("#ff7f00", "#e377c2", "#17becf", "#336633", "#0000FF"), main = "Buffer Distance for Each Point")

# Creating spatial neighborhood based on zones
nb.datum <- poly2nb(buf, row.names = datum$Zones) # From polygons calculated above

# Simple row standardization for spatial weight matrix (SWM)
listwdatum <- nb2listw(nb.datum, zero.policy = TRUE)

# Moran Test for Arsenic
MC.env <- moran.randtest(datum[,19], listwdatum, nrepet = 999, alter = "two-sided")
MC.env

# Decomposing Moran's Coefficient
NP.As <- moranNP.randtest(datum$as, listwdatum, nrepet = 999, alter = "two-sided")
NP.As

plot(NP.As)

# MULTISPATI Analysis
pca.datum <- dudi.pca(datum[,c(19, 25:43)], scale = TRUE, scannf = FALSE, nf = 2)

moran.randtest(pca.datum$li, listw = listwdatum)

ms.datum <- adespatial::multispati(pca.datum, listw = listwdatum, scannf = FALSE)

summary(ms.datum)

g.ms.spe <- s.arrow(ms.datum$c1)








