## ----------------------------------- #
## US Population Density & Leaflet Plots
## ------------
## For Western United States
##
## Created by: Ian Buller
## Created on: December 30, 2017
##
## Most recently modified on: November 29, 2018
## Modified by: Ian Buller
##
## Notes:
# A) Leaflet plots of population
# B) Campsites
# ----------------------------------- #

# -------- ####
# Packages #
# -------- ####
library(raster)
library(tigris)
library(dplyr)
library(devtools)
library(rgeos)
library(sp)
library(raster)
library(geosphere)
library(ggplot2)
library(ggmap)
library(rgdal)
library(maps)
library(leaflet)
library(htmlwidgets)
devtools::install_github("jjchern/zcta")
devtools::install_github("larmarange/labelled")

# ---- ####
# Data #
# ---- ####
# From 2010 US Census by 5-Digit Zip Code
us_zcta_v2 <- zcta::zcta_county_rel_10 

# Download 2010 US Census 5-Digit Zip Code shapefile
# Western United States
# Individual States
options(tigris_use_cache = T)
az_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Arizona"))
ca_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("California"))
co_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Colorado"))
id_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Idaho"))
ks_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Kansas"))   
mt_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Montana"))   
ne_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Nebraska"))
nv_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Nevada"))   
nm_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("New Mexico"))
nd_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("North Dakota"))
ok_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Oklahoma"))
or_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Oregon"))
sd_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("South Dakota")) 
tx_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Texas"))
ut_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Utah")) 
wa_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Washington"))
wy_zcta_raw <- tigris::zctas(cb=FALSE, year = 2010, state=c("Wyoming"))   

# Combine States
wus_zcta <- rbind(az_zcta_raw,ca_zcta_raw,co_zcta_raw,id_zcta_raw,ks_zcta_raw,mt_zcta_raw,ne_zcta_raw,nv_zcta_raw,nm_zcta_raw,nd_zcta_raw,ok_zcta_raw,or_zcta_raw,sd_zcta_raw,tx_zcta_raw,ut_zcta_raw,wa_zcta_raw,wy_zcta_raw)

# Extract WUS Population Data
wus_fips <- c(04,06,08,16,20,30,31,32,35,38,40,41,46,48,49,53,56)
wus_zcta_dat <- us_zcta_v2[us_zcta_v2$state %in% wus_fips,]
wus_zcta_extract <- data.frame(wus_zcta_dat$zcta5,wus_zcta_dat$poppt)
names(wus_zcta_extract) <- c("zcta5", "poppt") 
summary(wus_zcta_extract)

# Merge WUS Population Data and Polygons
wus_zcta@data <- data.frame(wus_zcta@data, wus_zcta_extract[match(wus_zcta@data[,"ZCTA5CE10"], wus_zcta_extract[,"zcta5"]),])
summary(wus_zcta)

# Remove duplicate Zip Codes
wus_zcta_deduped <- wus_zcta[!duplicated(wus_zcta$ZCTA5CE10), 1:13]
summary(wus_zcta_deduped)

# Convert polygons to centroids
trueCentroids <- rgeos::gCentroid(wus_zcta_deduped,byid=TRUE)
str(trueCentroids)
sp::plot(trueCentroids,pch=2) # plot

# Convert to SpatialPointsDataFrame
centroids_wus <- as.data.frame(geosphere::centroid(wus_zcta_deduped))
colnames(centroids_wus) <- c("lon", "lat") 
centroids_wus <- data.frame("ID" = 1:nrow(centroids_wus), centroids_wus) # Assign ID
sp::coordinates(centroids_wus) <- c("lon", "lat") 
sp::proj4string(centroids_wus) <- sp::proj4string(wus_zcta_deduped) # assign projection
centroids_wus@data <- sp::over(x = centroids_wus, y = wus_zcta_deduped, returnList = FALSE) # extract zip code ID
sp::plot(centroids_wus)

# WARNING: Missing Data for 82 zipcodes
sp::plot(subset(centroids_wus, is.na(centroids_wus$STATEFP10))) # PLOT OF MISSINGNESS

wus_centroids <- as.data.frame(centroids_wus)
str(wus_centroids)
wus_centroids$poppt_text <- as.character(wus_centroids$poppt)

# GGPLOT of 2010 Population
ggplot2::ggplot(data=wus_centroids,ggplot2::aes(x=lon,y=lat)) # data input
    + ggplot2::geom_point(data=wus_centroids, ggplot2::aes(size=poppt)) # size of point relative to size of population
    + ggplot2::theme_bw() # black and white theme

# Campgrounds in Western United States
# Source: http://www.uscampgrounds.info/

campgrounds <- rgdal::readOGR(dsn = "~/Campgrounds_US", layer = "Campgrounds")
head(campgrounds, n = 2)
# Lower 48 US
wus_names <- c("Arizona", "California", "Colorado", "Idaho", "Kansas", "Montana", "Nebraska", "Nevada", "New Mexico", "North Dakota", "Oklahoma", "Oregon", "South Dakota", "Texas", "Utah", "Washington", "Wyoming")
campgrounds <- campgrounds[which(campgrounds$STATE %in% wus_names),]
campgrounds$popup <- ifelse(is.na(campgrounds$NAME), "Campground", as.character(campgrounds$NAME) )
sp::plot(campgrounds)

# ------------ ####
# Leaflet Plot
# ------------ ####

# Popups
df1 <- structure(list(lat = c(25, 50), long = c(-100,-100), row_rank = structure(1:2, .Label = c("1", "2"), class = "factor")), .Names = c("lat","lng", "row_rank"), row.names = c(NA, -2L), class = "data.frame")

# Create leaflet plot
wus_m1 <- leaflet::leaflet(wus_centroids) %>% # initial data = location of ZCTAs
  leaflet::setView(lng = -109, lat = 39, zoom = 5) %>% # starting location
  leaflet::addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "OSM (Default)") %>% # default basemap
  leaflet::addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>% # additional basemap
  leaflet::addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>% # additional basemap
  leaflet::addCircles(lng = ~lon, lat = ~lat, weight = 1,
             radius = ~(sqrt(poppt) * 30 + 2000), color = "#000000", popup= ~poppt_text, group = "2010 Population") %>% # additional data = population size 
  leaflet::addCircles(lng = campgrounds@coords[,1], lat = campgrounds@coords[,2], weight = 1, radius = 1000, color = "#228B22", popup= campgrounds$popup, group = "Campgrounds") %>% # additional data = campsite locations
  leaflet::addLayersControl(
    baseGroups = c("OSM (Default)", "Terrain", "Topographic"),
    overlayGroups = c("2010 Population", "Campgrounds"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% # layer selection
  leaflet::hideGroup(c("2010 Population", "Campgrounds")) %>% # no data shown (default)
  # addPolylines(lng = df1$lng, lat = df1$lat, color = "#000000") %>% # add a vertical line at the 100th Meridian 
  leaflet::addMiniMap() # add mini map 
wus_m1 # display leaflet plot

# ------ ####
# Export #
# ------ ####
htmlwidgets::saveWidget(wus_m1, file="example.html", selfcontained=TRUE)