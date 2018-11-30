## ***************************************************************** #
## Obtaining and visualizing spatial data from the U.S. Census Bureau
## ---------------------
## Using population densities in 2010 and 1950 as examples
## Western United States
##
## Created by: Ian Buller
## Created on: November 19, 2018
## 
## Recently edited by:
## Recently edited on:
##
## Notes:
# A) 
## ***************************************************************** #

# -------- ####
# Packages #
# -------- ####
library(tigris)
library(tidycensus)
library(ipumsr)

# ---- ####
# Data #
# ---- ####
# County Shapefiles (Based on 2010 County Boundaries)
wus_county <- tigris::counties(state=c("Arizona", "California", "Colorado", "Idaho", "Kansas", "Montana", "Nebraska", "Nevada", "New Mexico", "North Dakota", "Oklahoma", "Oregon", "South Dakota", "Texas", "Utah", "Washington", "Wyoming"))

# 2010 Population U.S. Cenus Bureau
census_key <- "fc5d15c784d9dfb3daeba8c20f3870c9b7059aa6" # Personal Key for Ian Buller
vars_10 <- "P001001" # Population and total land area

wus_population_data_2010 <- tidycensus::get_decennial(
  geography = "county",
  variables = vars_10,
  year = 2010,
  state = c("Arizona", "California", "Colorado", "Idaho", "Kansas", "Montana", "Nebraska", "Nevada", "New Mexico", "North Dakota", "Oklahoma", "Oregon", "South Dakota", "Texas", "Utah", "Washington", "Wyoming"),
  geometry = TRUE,
  key = census_key
  )
# 1990, the code is P0010001

# 1950 Population from IPUMS NHGIS data
# Steven Manson, Jonathan Schroeder, David Van Riper, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 13.0 nhgis0001. Minneapolis: University of Minnesota. 2018. http://doi.org/10.18128/D050.V13.0
# Must register and obtain files from: https://usa.ipums.org/usa/data.shtml
pop1950 <- ipumsr::read_nhgis("nhgis0001_csv.zip")

# adjust state and county FIPS codes to match 2010 format
pop1950$STATE_code <- substr(pop1950$STATEA,1,nchar(pop1950$STATEA)-1) # state FIPS
pop1950$COUNTY_code <- substr(pop1950$COUNTYA,1,nchar(pop1950$COUNTYA)-1) # county FIPS
pop1950$GEOID <- paste(pop1950$STATE_code, pop1950$COUNTY_code, sep="") # full FIPS

# Join three data sources
wus_merged_pop <- tigris::geo_join(wus_county, wus_population_data_2010, "GEOID", "GEOID")
wus_merged_pop <- tigris::geo_join(wus_merged_pop, pop1950, "GEOID", "GEOID")

# Oglala Lake County, South Dakota data missing from tidycensus data
# Known as Shannon County, South Dakota until 2015
wus_merged_pop$value <- ifelse(wus_merged_pop$GEOID == 46102, 13586, wus_merged_pop$value) #https://www.census.gov/quickfacts/oglalalakotacountysouthdakota
wus_merged_pop$B18001 <- ifelse(wus_merged_pop$GEOID == 46102, 5669, wus_merged_pop$B18001) #https://www.census.gov/population/cencounts/sd190090.txt

# Carson City
# Merged with Ormsby County in 1969
# https://www.census.gov/population/cencounts/nv190090.txt
wus_merged_pop$B18001 <- ifelse(wus_merged_pop$GEOID == 32510, 4172, wus_merged_pop$B18001)

# Reproject to laea
wus_merged_pop_aea <- sp::spTransform(wus_merged_pop, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

# Calculate Population Density
# Per square kilometer
wus_merged_pop_aea$DENSITY_10 <- wus_merged_pop_aea$value / (as.numeric(wus_merged_pop_aea$ALAND)/1000000)
wus_merged_pop_aea$DENSITY_50 <- as.numeric(wus_merged_pop_aea$B18001) / (as.numeric(wus_merged_pop_aea$ALAND)/1000000)

# Reclassify upper bound to 1000 people per square kilometer
wus_merged_pop_aea$DENSITY_10_reclass <- ifelse(wus_merged_pop_aea$DENSITY_10 >= 999, 999, wus_merged_pop_aea$DENSITY_10)
wus_merged_pop_aea$DENSITY_50_reclass <- ifelse(wus_merged_pop_aea$DENSITY_50 >= 999, 999, wus_merged_pop_aea$DENSITY_50)

# Calcaluate change in population density
wus_merged_pop_aea$DENSITY_5010 <-  (wus_merged_pop_aea$DENSITY_10 - wus_merged_pop_aea$DENSITY_50)/wus_merged_pop_aea$DENSITY_50 * 100

# Reclassify upper bound to 1000 percent increase in population
wus_merged_pop_aea$DENSITY_5010_reclass <- ifelse(wus_merged_pop_aea$DENSITY_5010 >= 999, 999, wus_merged_pop_aea$DENSITY_5010)

# Breaks for colorkey
at <- c(0, 200, 400, 600, 800, 1000)
at_names <- c("0", "200", "400", "600", "800", ">1000")

# Plot 2010 Population Density (sq kilometer)
png(file = "pop_2010.png", width = 2000, height = 1600)
spplot(wus_merged_pop_aea, 
       "DENSITY_10_reclass", 
       col.regions=rev(heat.colors(101)), 
       par.settings = list(axis.line = list(col =  'transparent')),
       colorkey=list(labels = list(labels = at_names, cex = 4, fontfamily='LM Roman 10', fontface = 1))
)
dev.off()

# Missing counties since 1950
# Broomfield County, CO (2001, from Boulder County)
# Cibola County, NM (1981, from Valencia County)
# La Paz County, AZ (1983, from Yuma County)
wus_merged_pop_aea_na <- wus_merged_pop_aea[is.na(wus_merged_pop_aea$DENSITY_5010_reclass),]
wus_merged_pop_aea_na$DENSITY_5010_reclass <- -9999

# Breaks for colorkey
at <- c(-80, 0, 200, 400, 600, 800, 1000)
at_names <- c("-80", "0", "200", "400", "600", "800", ">1000")
palpos <- brewer.pal(sum(at>0),"Greens")
palette <- c("Purple",palpos)

# Plot Population Change Percent
png(file = "pop_5010.png", width = 2000, height = 1600)
sp::spplot(wus_merged_pop_aea, 
           "DENSITY_5010_reclass", 
           col.regions=palette, 
           at = at,
           par.settings = list(axis.line = list(col =  'transparent')),
           colorkey=list(labels = list(labels = at_names, at = c(-81, at), cex = 4, fontfamily='LM Roman 10', fontface = 1)
           ) 
) + latticeExtra::as.layer(spplot(wus_merged_pop_aea_na, 
                                  "DENSITY_5010_reclass",
                                  col.regions = "black"), under = F)
dev.off()