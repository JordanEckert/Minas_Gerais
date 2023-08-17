## This work is in conjunction with Nedret Billor and J.J. Lelis

# Attach packages - Data Cleaning
library(readxl)        # Loading excel dataset
library(tidyverse)     # Data manipulation
library(dplyr)         # Pipe operator
library(spatstat.geom) # Distances

# Attach packages
library(GWmodel)      # Geographically weighted models
library(sp)           # Data management
library(spdep)        # Spatial autocorrelation
library(gstat)        # Geostatistics
library(RColorBrewer) # Visualization
library(classInt)     # Class intervals
library(gridExtra)    # Multiple plot
library(ggplot2)      # Multiple plot
library(leaflet)      # Creating geographic maps
library(corrplot)     # Correlation plots
library(Hmisc)        # Correlation analysis
library(ade4)         # Spatial analysis suite
library(adespatial)   # Spatial analysis suite
library(adegraphics)  # Spatial plotting suite
library(sf)           # Spatial analysis suite
library(umap)         # UMAP algorithm
library(spgwr)        # GW Regression
library(raster)       # Shapefiles
library(MASS)         # Regression variable selection
library(leaps)        # Regression variable selection
library(spatialRF)    # Spatial RF
library(deldir)       # Delauney tessellations

library(kableExtra)


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

#### Exploratory Data Analysis - Graphics ####

# Leaflet map of each (complete) sampled point, hovering over gives the Zone
map <- leaflet(datum) %>%
  addTiles() %>%
  addMarkers(lng = datum$Longitude, lat = datum$Latitude, 
             label = datum$Zones)
map

# Boxplots of Heavy Metals by Zones
for(cols in colnames(datum[5:24])){
  boxplot(datum[[cols]] ~ datum$Zones, xlab = "Labs", ylab = paste(cols), main = paste("Boxplot of", cols, "by Zones"))
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

cor_plot3 <- corrplot(corr5, type = "upper", method = "shade", main = "Correlation Plot of Heavy Metals and Soil Properties")
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

#### Exploratory Spatial Analysis - UMAP ####
plot.umap <- function(x, labels,
                      main="UMAP Visualization",
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
custom.config$spread <- 2
custom.config$alpha <- .5
custom.config$negative_sample_rate <- 100

# Tuned config
datum.umap <- umap(as.matrix(dist(nnwhich(datum[,5], by = datum$Zones))), 
                   custom.config)
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
plot(multi_vario_soil$d, multi_vario_soil$var,type = 'b', pch = 20, xlab = "Distance", ylab = "C(distance)", main = "Multivariate Variogram for Soil Properties")

#### Spatial Data Analysis - Spatial Weighting Matrix ####

## Our sampling design was irregularly sampled points. Therefore, step one in building the SWM is 
## to build spatial neighborhoods. We offer two ways of building these neighborhoods. The first is through the 
## Gabriel graphs. These graphs have an edge between points if the two points are closes to their midpoint, with no other given point being as close. 
## The second is through creating polygon areas based on distances from points and their overlaps. 

## Neighborhood based on the Relative Neighborhood Graph
nb.datum<- graph2nb(gabrielneigh(datum[,c(1,2)]), sym = TRUE)

#### Alternative Neighborhood Construction Method Could be Based on Distances ####
## Peeling off different zones as categories
#TO_datum <- spatial_datum[spatial_datum$Zones == "TO", ]
#SF_datum <- spatial_datum[spatial_datum$Zones == "SF", ]
#PR_datum <- spatial_datum[spatial_datum$Zones == "PR", ]
#MA_datum <- spatial_datum[spatial_datum$Zones == "MA", ]

## Changing indexes to be correct
#rownames(SF_datum) <- 35:489
#rownames(PR_datum) <- 490:568
#rownames(MA_datum) <- 569:696

## Create polygon area around each zone - Allows for adaptive polygon creation based on Zone
#buf1 <- st_buffer(TO_datum, dist = 27500)
#plot(buf1[,1], pal = c("#ff7f00", "#e377c2", "#17becf", "#336633", "#0000FF"), main = "Polygon Area for TO")
#
#buf2 <- st_buffer(SF_datum, dist = 20000)
#plot(buf2[,1], pal = c("#ff7f00", "#e377c2", "#17becf", "#336633", "#0000FF"), main = "Polygon Area for SF")

#buf3 <- st_buffer(PR_datum, dist = 35000) 
#plot(buf3[,1], pal = c("#ff7f00", "#e377c2", "#17becf", "#336633", "#0000FF"), main = "Polygon Area for PR")

#buf4 <- st_buffer(MA_datum, dist = 40000)
#plot(buf4[,1], pal = c("#ff7f00", "#e377c2", "#17becf", "#336633", "#0000FF"), main = "Polygon Area for MA")

## Combining polygon areas
#buf <- rbind(buf1, buf2, buf3, buf4)

## Creating spatial neighborhood based on zones
#nb.datum <- poly2nb(buf, row.names = spatial_datum) # From polygons calculated above

## Further differences can be explored using diffnb() if wanted

# Distances for weighting relative to other points 
dist.nb <- nbdists(nb.datum, datum[,c(1,2)])

# Function of distance 1 - d_ij/max(d_ij) for scaling
fdist <- lapply(dist.nb, function (x) 1 - x/max(dist(datum[,c(1,2)])))

# Generate Spatial Weighting Matrix
listwdatum <- nb2listw(nb.datum, fdist, style = "W")
listwdatum

#### Spatial Data Analysis - Creating Spatial Predictors ####

## Spatial predictors are orthogonal vectors stored in an object of class orthobasisSp
## Allows for Moran's Eigenvalue Maps (MEM) based on the diagonalization of a doubly-centered SWM
## MEMs maximize the Moran's coefficient of spatial autocorrelation. These predictors can be used
## to provide spatially-explicit multiscale tools (Dray et al. 2012). 

# Calculate Moran eigenvector maps
mem.datum <- mem(listwdatum)
mem.datum

# Map of MEM in Geographical Space
mxy <- as.matrix(datum[,c(1,2)]) # Matrix of coordinates
plot(mem.datum[,c(1, 2, 3, 4, 5, 10)], SpORcoords = mxy)

# Moran's I Test for each Eigenvector
MC.datum <- moran.randtest(datum[5:43], listwdatum, alter = "two-sided", nrepet = 999)
MC.datum

#### Spatial Data Analysis - Describing Spatial Patterns ####
# Moran's Coefficient Bounds
mc.bounds <- moran.bounds(listwdatum)
mc.bounds

# Graph of each individual variable
env.maps <- s1d.barchart(MC.datum$obs, labels = MC.datum$names, plot = TRUE, xlim = 1.1 * mc.bounds, paxes.draw = TRUE, pgrid.draw = FALSE)
addline(env.maps, v = mc.bounds, plot = TRUE, pline.col = 'red', pline.lty = 3)
  
# Decomposing Moran's Coefficient
NP.As <- moranNP.randtest(datum[,19], listwdatum, nrepet = 999, alter = "two-sided")
NP.As

NP.plot <- NULL
for(i in 5:43){
  NP.plot <- moranNP.randtest(datum[,i], listwdatum, nrepet = 999, alter = "two-sided")
  plot(NP.plot, main = paste("Moran Coefficient Decomposition for", colnames(datum[,i])))
}

## Positive spatial autocorrelation is when similar values cluster together on the map
## Negative spatial autocorrelation is when dissimilar values cluster together

## We see Arsenic has a statisitcally significant positive I+. 
## So, similar values of Arsenic will cluster together on the map.

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

# MC for PCA Scores
moran.randtest(pca$scores, listw = listwdatum, alter = "two-sided")

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
g.ms.spe <- s.arrow(ms.datum$c1, plot = TRUE, main = "PCA")
g.ms.spe

#### Spatial Data Analysis - Geographically Weighted Regression ####

# Scatterplots
plot(datum$As, datum$Zones)  # One zone (PR) seems to have higher quantities, or at least more spread out

for(cols in colnames(datum[25:43])){
  plot(datum$As ~ datum[[cols]], xlab = "Arsenic", ylab = paste(cols), main = paste("Plots of", cols, "vs. Arsenic"))
} # None of the soil properties have a linear relationship - GWR would probably not go well ...

# Original model - all variables
model1 <- glm(As ~ datum$Zones + datum$`pH H2O` + datum$`pH KCl` + datum$PM +
  datum$`K+` + datum$`Ca2+` + datum$`Ca2+` + datum$`Mg2+` +
  datum$`Al3+` + datum$`H+Al` + datum$SB + datum$ECEC +
  datum$CEC + datum$`V (%)` + datum$`m (%)` + datum$SOM +
  datum$PREM + datum$`Coarse sand` + datum$`Fine sand` + datum$Silt +
  datum$Clay, family = gaussian, data = datum)

summary(model1) # Summary shows a lot of non significant variables

## Let's do variable selection and see if that helps with the errors in the residual plot

# Correlation of Aresenic with Heavy metals
corr <- cor(datum[,c(19,25:43)])
cor_plot <- corrplot(corr, type = "upper", method = "shade")
cor_plot

# Ca2+ and ECEC, Ca2+ and CEC, ph H20 and m (%), V(%) and m(%), SB and CEC
# PREM and Clay, Fine sand and Clay, ph H20 and SOM are all correlated heavily.

# Look at top 3 subsets
all <- regsubsets(datum$As ~ datum$Zones + datum$`pH H2O` + datum$`pH KCl` + datum$PM +
  datum$`K+` + datum$`Ca2+` + datum$`Ca2+` + datum$`Mg2+` +
  datum$`Al3+` + datum$`H+Al` + datum$SB + datum$ECEC +
  datum$CEC + datum$`V (%)` + datum$`m (%)` + datum$SOM +
  datum$PREM + datum$`Coarse sand` + datum$`Fine sand` + datum$Silt +
  datum$Clay, data = datum, nbest = 1)

info <- summary(all)
cbind(info$which, round(cbind(rsq=info$rsq, adjr2=info$adjr2, cp=info$cp, bic=info$bic, rss=info$rss), 3))

# Stepwise Regression picks lowest AIC
null<-lm(As ~ 1, data=datum)
full<-lm(datum$As ~ datum$Zones + datum$`pH H2O` + datum$`pH KCl` + datum$PM +
  datum$`K+` + datum$`Ca2+` + datum$`Ca2+` + datum$`Mg2+` +
  datum$`Al3+` + datum$`H+Al` + datum$SB + datum$ECEC +
  datum$CEC + datum$`V (%)` + datum$`m (%)` + datum$SOM +
  datum$PREM + datum$`Coarse sand` + datum$`Fine sand` + datum$Silt +
  datum$Clay, data = datum)

stepAIC(null, scope=list(lower=null, upper=full), data=datum, direction='both')

# Secondary Regression with new variable subset
model2 <- glm(As ~ datum$Silt + datum$`pH KCl` + datum$`V (%)` +
  datum$`Al3+` + datum$Clay, data = datum)
summary(model2)

plot(model2, 1) # Plot of residual versus fitted, no spatial patterning

# Plot of residuals for obvious spatial patterning
resids<-residuals(model2)
colours <- c("navy", "blue", "red", "maroon")
map.resids <- SpatialPointsDataFrame(data=data.frame(resids), coords=cbind(datum$Longitude, datum$Latitude))
spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=1)

## From plot there is some spatial patterning of the residuals. Lets see how coefficients of the model vary across region

# Bandwidth selection
GWRbandwidth <- gwr.sel(As ~ datum$Silt + datum$`pH KCl` + datum$`V (%)` +
  datum$`Al3+` + datum$Clay, datum, coords=cbind(datum$Longitude, datum$Latitude), adapt = TRUE, longlat = TRUE)

# GWR Model
gwr.model = gwr(As~ Silt + `pH KCl` + `V (%)` +
  `Al3+` + Clay, datum, coords=cbind(datum$Longitude, datum$Latitude), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)

# GWR Model Results
gwr.model

# Attach results to original dataset
coef<-as.data.frame(gwr.model$SDF)
head(coef)

new_datum <- cbind(datum, coef$Silt, coef$X.pH.KCl., coef$X.V....., coef$X.Al3.., coef$Clay)

# Plotting GWR Coefficients

for (cols in colnames(new_datum[44:48])){
  gwr.point<-ggplot(new_datum,
                    aes(x=Longitude,y=Latitude))+geom_point(aes(colour=new_datum[[cols]])) + scale_colour_gradient2(low = "red", mid = "green", high = "blue", midpoint = median(new_datum[[cols]]), space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs")) + coord_equal()
  plot(gwr.point)
}

#### Spatial Data Analysis - Random Forest ####

# Arsenic levels
datum$AsLevel <- as.factor(ifelse(datum$As >= 8.0, "High", "Low"))

# Summary
table(datum$AsLevel)

## 202 out of the 696 are high levels of Arsenic (~30%)
## Is there a local imbalance of high levels?

# Create a scatter plot of As values
as_plot <- ggplot(datum, aes(x = Longitude, y = Latitude)) + 
  geom_point(aes(size = As, color = AsLevel), alpha = 0.7) + 
  scale_size_continuous(range = c(2, 10), name = "As Value") + 
  labs(title = "Distribution of Arsenic (As) Values Across Minas Gerais Region",
       x = "Longitude", y = "Latitude", color = "As Value") + 
  theme_minimal()

# Print the scatter plot
print(as_plot)

## Visualizations seem to show that yes there is a local imbalance.

# It's picky about the column names, so I rename variables in columns 25 to 43 by removing spaces
col_indices <- 25:43
new_col_names <- gsub(" ", "_", colnames(datum)[col_indices])  # Replace spaces with underscores

# Rename the columns
colnames(datum)[col_indices] <- new_col_names

# Distance Matrices with and without latitude (without latitude and longitude only used for graphics)
dist_mat <- as.matrix(dist(datum[,c(1,2,19,25:43)]))
dist_mat2 <- as.matrix(dist(datum[,c(19,25:43)]))

# Extract relevant columns for model training
predictor_columns <- c("Latitude", "Longitude", colnames(datum)[25:43])
predictor_columns2 <- c(colnames(datum)[25:43])

# Assesses spatial autocorrelation of response variable and predictors across thresholds
spatialRF::plot_training_df_moran(
  data = datum[,c(19, 25:43)],
  dependent.variable.name = "As",
  predictor.variable.names = predictor_columns2,
  distance.matrix = dist_mat,
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1), 
  point.color = "gray40")

## NOTE: Lower p-values and Moran's I values indicate there is no spatial autocorrelation for given variable and distance threshold

# Rename Latitude to x and Longitude to y
colnames(datum)[colnames(datum) == "Latitude"] <- "x"
colnames(datum)[colnames(datum) == "Longitude"] <- "y"

#coordinates of the cases
xy <- datum[, c("x", "y")]

# Train the non-spatial random forest model
model.non.spatial <- spatialRF::rf(
  data = datum[,c(1,2,19, 25:43)],
  dependent.variable.name = "As",
  predictor.variable.names = colnames(datum[,c(1,2,25:43)]),
  distance.matrix = as.matrix(dist(datum[,c(1,2,19,25:43)])),
  xy = xy,
  seed = 2023,
  verbose = TRUE
)

# Residuals
spatialRF::plot_residuals_diagnostics(
  model.non.spatial,
  verbose = FALSE
)

# Global Variable Importance
spatialRF::plot_importance(
  model.non.spatial,
  verbose = FALSE
)

library(randomForestExplainer)

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

# Model transferability - evaluates non spatial model spatially
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

# Local variable importance
local.importance <- spatialRF::get_importance_local(model.non.spatial)

kableExtra::kbl(
  round(local.importance[1:10,], 0),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

## Larger values indicated a larger average error when estimating a case
## with the permuted version of the variable. Aka more important variables show
## larger values

# Response curve table
reponse.curves.df <- spatialRF::get_response_curves(model.non.spatial)

kableExtra::kbl(
  head(reponse.curves.df, n = 10),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

# Response surface
spatialRF::plot_response_surface(
  model.non.spatial,
  a = "As",
  b = "pH_H2O",
)

# Evaluating random forest model spatially
model.non.spatial <- spatialRF::rf_evaluate(
  model = model.non.spatial,
  xy = xy,                  #data coordinates
  repetitions = 30,         #number of spatial folds
  training.fraction = 0.75, #training data fraction on each fold
  metrics = "r.squared",
  verbose = TRUE
)

spatialRF::plot_evaluation(model.non.spatial)

## Evaluations show poor performance across the spatial folds

# We see that there is spatial autocorrelation of residuals
spatialRF::plot_moran(
  model.non.spatial, 
  verbose = FALSE
)

# Spatial Random Forest - need to clean the dataset names and rerun everything first
library(janitor)
datum <- datum %>% clean_names()

# Train the non-spatial random forest model
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

model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  method = "mem.moran.sequential", #default method
  verbose = TRUE,
)

spatial.importance.df <- randomForestExplainer::measure_importance(
  model.spatial,
  measures = c("mean_min_depth", "no_of_nodes", "times_a_root", "p_value")
)

kableExtra::kbl(
  spatial.importance.df %>% 
    dplyr::arrange(mean_min_depth) %>% 
    dplyr::mutate(p_value = round(p_value, 4)),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

# Comparing Variable Importance

p1 <- spatialRF::plot_importance(
  model.non.spatial, 
  verbose = FALSE) + 
  ggplot2::ggtitle("Non-spatial model") 

p2 <- spatialRF::plot_importance(
  model.spatial,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model")

p1 | p2 

# But what are spatial predictors? 
##Spatial predictors, as shown below, are smooth surfaces representing 
## neighborhood among records at different spatial scales. Eigenvectors from the Moran Maps are 
## the spatial predictors. 

# Selection of spatial predictor
p <- spatialRF::plot_optimization(model.spatial)

## The spatial predictors are included in the model one by one, 
## in the order of their Moran’s I 
## (spatial predictors with Moran’s I lower than 0 are removed). 

## The selection procedure is performed by the function select_spatial_predictors_sequential(), 
## which finds the smaller subset of spatial predictors maximizing the model’s 
## R squared, and minimizing the Moran’s I of the residuals. 

## This is shown in the optimization plot (dots linked by lines represent the selected spatial predictors).

# Comparing models
comparison <- spatialRF::rf_compare(
  models = list(
    `Non-spatial` = model.non.spatial,
    `Spatial` = model.spatial
  ),
  xy = xy,
  repetitions = 5,
  training.fraction = 0.75,
  metrics = "r.squared",
  seed = 2023
)

x <- comparison$comparison.df %>% 
  dplyr::group_by(model, metric) %>% 
  dplyr::summarise(value = round(median(value), 3)) %>% 
  dplyr::arrange(metric) %>% 
  as.data.frame()

colnames(x) <- c("Model", "Metric", "Median")

kableExtra::kbl(
  x,
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

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
