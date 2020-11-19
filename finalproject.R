#read in data from activity 6
#GNPglaciers

library(raster)
library(rgdal)
library(ggplot2)
library(sp)
library(dplyr)

#read in glaciers from 1966
g1966 <- readOGR("/Users/margaretmanning/GitHub/ENVST_activity6/a06/GNPglaciers/GNPglaciers_1966.shp")

#read in glaciers in 2015
g2015 <- readOGR("/Users/margaretmanning/GitHub/ENVST_activity6/a06/GNPglaciers/GNPglaciers_2015.shp")

#map the glaciers filling in the polygons with light blue and making the borders grey
plot(g1966, col="lightblue2", border="grey50")

#preview data stored with all accompanying info/measurements for each spatial object
head(g2015@data)

#finding projection of vector object using proj4string
g1966@proj4string

#check glacier names 
g1966@data$GLACNAME
g2015@data$GLACNAME

#fix glacier names so they are the same for each dataset
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(g2015@data$GLACNAME == "Miche Wabun",
                                     "Miche Wabun Glacier",
                                     as.character(g2015@data$GLACNAME)))
#lets combine area; first work with a smaller data frame 
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)
gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

#join all data tables by glacier name 
gALL <- full_join(gdf66,gdf15, by="GLACNAME")

#calculate the % change in area from 1966 to 2015 
gALL$gdiff <- ((gALL$area66-gALL$area15)/gALL$area66)*100

#make a scatterplot of glacier area in 1966 vs. % change in area
plot(gALL$area66, gALL$gdiff, 
     xlab = "Glacier Area in 1966", 
     ylab = "% Change in Area",
     pch = 19,
     col = "salmon")

#join data with the spatial data table and overwrite into spatial data table
g1966@data <- left_join(g1966@data, gALL, by="GLACNAME")
#use spplot to shade polygons based on the % change of labels 
#first argument is the spatial object
#second is the column of data to display with the diff colors
#add a title using main
#col changes the color of the borders
spplot(g1966, "gdiff", main="% Change in Area", col="transparent")

#calculate mean and stand dev of % loss
mean(gALL$gdiff)
sd(gALL$gdiff)

#find glacier with largest and smallest % loss
max(gALL$gdiff, na.rm = TRUE)
min(gALL$gdiff, na.rm = TRUE)

#Find glaciers in 1966 with smallest and largest area
max(gALL$area66, na.rm = TRUE)
min(gALL$area66, na.rm = TRUE)

#make a map showing glacier footprints in 1966 and 2015 for glacier with the largest percent loss
#Boulder Glacier had largest % loss 
boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]
plot(boulder66, 
     main = "Change in Boulder Glacier From 1966 to 2015", 
     col = "slategrey")
plot(boulder15, 
     col = "red1",
     add = TRUE)

#make a legend
#first create list of all labels 
labels <- c("Extent in 1966", "Extent in 2015")
levels(gALL$TYPE)
labels

legend("topleft",
       legend = labels,
       fill=plotColors,
       bty="n",
       cex=.9)
plotColors <- c("slategrey", "red1")

#Pumpelly Glacier had smallest % loss
pumpelly66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
pumpelly15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]
plot(pumpelly66, 
     main = "Change in Pumpelly Glacier From 1966 to 2015", 
     col = "slategrey")
plot(pumpelly15, 
     col = "red1",
     add = TRUE)
legend("topleft",
       legend = labels,
       fill=plotColors,
       bty="n",
       cex=.9)

#NDVI data analysis
#read in NDVI data
#set up directory for NDVI data folder
dirR <- "/Users/margaretmanning/GitHub/ENVST_finalproject/a06/NDVI/"

#read in NDVI data 
NDVI_2003 <- raster(paste0(dirR,"NDVI_2003.tif"))
NDVI_2004 <- raster(paste0(dirR,"NDVI_2004.tif"))
NDVI_2005 <- raster(paste0(dirR,"NDVI_2005.tif"))
NDVI_2006 <- raster(paste0(dirR,"NDVI_2006.tif"))
NDVI_2007 <- raster(paste0(dirR,"NDVI_2007.tif"))
NDVI_2008 <- raster(paste0(dirR,"NDVI_2008.tif"))
NDVI_2009 <- raster(paste0(dirR,"NDVI_2009.tif"))
NDVI_2010 <- raster(paste0(dirR,"NDVI_2010.tif"))
NDVI_2011 <- raster(paste0(dirR,"NDVI_2011.tif"))
NDVI_2012 <- raster(paste0(dirR,"NDVI_2012.tif"))
NDVI_2013 <- raster(paste0(dirR,"NDVI_2013.tif"))
NDVI_2014 <- raster(paste0(dirR,"NDVI_2014.tif"))
NDVI_2015 <- raster(paste0(dirR,"NDVI_2015.tif"))
NDVI_2016 <- raster(paste0(dirR,"NDVI_2016.tif"))

#plot raster
plot(NDVI_2003)
plot(NDVI_2004)
NDVI_2004@crs

#rgeos
install.packages("rgeos")
library(rgeos)

#fix CS for glaciers and NDVI
glacierP <- spTransform(g1966,NDVI_2004@crs)
plot(glacierP, add=TRUE)

#add buffer 500 meters out from glacier
glacierB <- gBuffer(glacierP, width = 500, byid = TRUE)
plot(glacierB)

#polygon that's just buffer without glacier in it (500m away from largest extent of glacier in 1966)
test <- gSymdifference(glacierB, glacierP)
plot(test)

#find NDVI pixels that fall within polygon of glacier 1966 
#using extract function 
NDVIg2004 <- extract(NDVI_2004,test)
#double bracket shows which NDVI value is in glacier [[1]] 
NDVIg2004[[1]]

#first glacier in 1966 data (not projected)
plot(g1966[g1966@data$OBJECTID==1,])
#change 1966 to glacierP to see projected 

NDVIg2016 <- extract(NDVI_2016,test)
ag1 <- data.frame(NDVI=c(NDVIg2004[[1]], NDVIg2016[[1]]), 
                  year=c(rep(2004,length(NDVIg2004[[1]])), rep(2016, length(NDVIg2016[[1]]))))

#can make boxplot 
plot(ag1$NDVI~as.factor(ag1$year))

#individual glacier
glacierboulder <- glacierP[glacierP@data$GLACNAME=="Boulder Glacier",]

#add buffer
boulderbuff <- gBuffer(glacierboulder, width = 500, byid = TRUE)
plot(boulderbuff)

#buffer polygon
bbonly <- gSymdifference(boulderbuff, glacierboulder)
plot(bbonly, col="darkturquoise")

#compare 2004 to 2016
NDVIg2016B <- extract(NDVI_2016,bbonly)
NDVIg2004B <- extract(NDVI_2004,bbonly)

#plot/see the difference
bb1 <- data.frame(NDVI=c(NDVIg2004B[[1]], NDVIg2016B[[1]]), 
                  year=c(rep(2004,length(NDVIg2004B[[1]])), rep(2016, length(NDVIg2016B[[1]]))))

#can make boxplot 
plot(bb1$NDVI~as.factor(bb1$year),
     xlab="Year", ylab="NDVI", main="NDVI of Boulder Glacier in 2004 and 2016")

#ttest of Boulder glacier 2004 vs 2016
b04 <- data.frame(NDVI=c(NDVIg2004B[[1]]), 
                  year=c(rep(2004,length(NDVIg2004B[[1]]))))
b16 <- data.frame(NDVI=c(NDVIg2016B[[1]]), 
                  year=c(rep(2016, length(NDVIg2016B[[1]]))))

B_ttest <- t.test(b04$NDVI,b16$NDVI, paired = FALSE, var.equal = FALSE)
B_ttest

plot(NDVI_2004, ext=extent(-70000,-60000, 1000000,1200000),
     main="NDVI of Boulder Glacier in 2004")
plot(bbonly, add=TRUE)
plot(NDVI_2016, ext=extent(-70000,-60000, 1000000,1200000),
     main="NDVI of Boulder Glacier in 2016")
plot(bbonly, add=TRUE)

#look at Pumpelly Glacier (smallest % loss over time)
glacierpumpelly <- glacierP[glacierP@data$GLACNAME=="Pumpelly Glacier",]

#add buffer to glacier 
pumpellybuff <- gBuffer(glacierpumpelly, width = 500, byid = TRUE)
plot(pumpellybuff)

#buffer polygon
pponly <- gSymdifference(pumpellybuff, glacierpumpelly)
plot(pponly, col="salmon")

#compare 2004 to 2016
NDVIg2016P <- extract(NDVI_2016,pponly)
NDVIg2004P <- extract(NDVI_2004,pponly)

#plot/see the difference
pp1 <- data.frame(NDVI=c(NDVIg2004P[[1]], NDVIg2016P[[1]]), 
                  year=c(rep(2004,length(NDVIg2004P[[1]])), rep(2016, length(NDVIg2016P[[1]]))))

#can make boxplot 
plot(pp1$NDVI~as.factor(pp1$year),
     xlab="Year", ylab="NDVI", main="NDVI of Pumpelly Glacier in 2004 and 2016")

#ttest of Pumpelly glacier 2004 vs 2016
p04 <- data.frame(NDVI=c(NDVIg2004P[[1]]), 
                  year=c(rep(2004,length(NDVIg2004P[[1]]))))
p16 <- data.frame(NDVI=c(NDVIg2016P[[1]]), 
                  year=c(rep(2016, length(NDVIg2016P[[1]]))))

P_ttest <- t.test(p04$NDVI,p16$NDVI, paired = FALSE, var.equal = FALSE)
P_ttest

#view on NDVI rasters
plot(NDVI_2004, ext=extent(-70000,-60000, 1000000,1200000),
     main="NDVI of Pumpelly Glacier in 2004")
plot(pponly, add=TRUE)
plot(NDVI_2016, ext=extent(-70000,-60000, 1000000,1200000),
     main="NDVI of Pumpelly Glacier in 2016")
plot(pponly, add=TRUE)

#Run t-test to see change in means of NDVI from 2004-2016
#first check assumptions 
#assumption one - data are normally distributed

#calculate area for both pumpelly and boulder polygons
plot(bbonly, col="darkturquoise")
plot(pponly, col="seagreen")

#compare 2004 to 2016 (boulder)
NDVIg2016B <- extract(NDVI_2016,bbonly)
NDVIg2004B <- extract(NDVI_2004,bbonly)

#compare 2004 to 2016 (pumpelly)
NDVIg2016P <- extract(NDVI_2016,pponly)
NDVIg2004P <- extract(NDVI_2004,pponly)

install.packages("ggpubr")
library(ggpubr)

#see if visualization is bell shaped (check normal dist)
ggdensity(bb1$NDVI,
          main="Density plot of NDVI values for 2004 Boulder",
          xlab="NDVI")
ggdensity(pp1$NDVI,
          main="Density plot of NDVI values for 2004 Pumpelly",
          xlab="NDVI")

#make a qqplot to check normal distribution
ggqqplot(bb1$NDVI,
         main="Q-Q plot for Boulder Glacier")
ggqqplot(pp1$NDVI,
         main="Q-Q plot for Pumpelly Glacier")

#compare the normal distributions of other glaciers 
#Gem Glacier 
glaciergem <- glacierP[glacierP@data$GLACNAME=="Gem Glacier",]

#add buffer to gem  
gembuff <- gBuffer(glaciergem, width = 500, byid = TRUE)
plot(gembuff)

#buffer polygon of gem
ggonly <- gSymdifference(gembuff, glaciergem)
plot(ggonly, col="cadetblue")

#compare 2004 to 2016
NDVIg2016G <- extract(NDVI_2016,ggonly)
NDVIg2004G <- extract(NDVI_2004,ggonly)

#plot/see the difference
gg1 <- data.frame(NDVI=c(NDVIg2004G[[1]], NDVIg2016G[[1]]), 
                  year=c(rep(2004,length(NDVIg2004G[[1]])), rep(2016, length(NDVIg2016G[[1]]))))

#check normal dist of Gem Glacier
ggdensity(gg1$NDVI,
          main="Density plot of NDVI values for 2004 Gem",
          xlab="NDVI")

#make a qqplot to check normal distribution
ggqqplot(gg1$NDVI,
         main="Q-Q plot for Gem Glacier")

#Old Sun Glacier
glacieros <- glacierP[glacierP@data$GLACNAME=="Old Sun Glacier",]

#add buffer to OS  
osbuff <- gBuffer(glacieros, width = 500, byid = TRUE)
plot(osbuff)

#buffer polygon of gem
osonly <- gSymdifference(osbuff, glacieros)
plot(osonly, col="seagreen")

#compare 2004 to 2016
NDVIg2016OS <- extract(NDVI_2016,osonly)
NDVIg2004OS <- extract(NDVI_2004,osonly)

#plot/see the difference
os1 <- data.frame(NDVI=c(NDVIg2004OS[[1]], NDVIg2016OS[[1]]), 
                  year=c(rep(2004,length(NDVIg2004OS[[1]])), rep(2016, length(NDVIg2016OS[[1]]))))

#check normal dist of Gem Glacier
ggdensity(os1$NDVI,
          main="Density plot of NDVI values for 2004 Old Sun",
          xlab="NDVI")

#make a qqplot to check normal distribution
ggqqplot(os1$NDVI,
         main="Q-Q plot for Old Sun Glacier")

#Rainbow Glacier
glacierR <- glacierP[glacierP@data$GLACNAME=="Rainbow Glacier",]

#add buffer to OS  
rrbuff <- gBuffer(glacierR, width = 500, byid = TRUE)
plot(rrbuff)

#buffer polygon of gem
rronly <- gSymdifference(rrbuff, glacierR)
plot(rronly, col="seagreen")

#compare 2004 to 2016
NDVIg2016R <- extract(NDVI_2016,rronly)
NDVIg2004R <- extract(NDVI_2004,rronly)

#plot/see the difference
rr1 <- data.frame(NDVI=c(NDVIg2004R[[1]], NDVIg2016R[[1]]), 
                  year=c(rep(2004,length(NDVIg2004R[[1]])), rep(2016, length(NDVIg2016R[[1]]))))

#check normal dist of Gem Glacier
ggdensity(rr1$NDVI,
          main="Density plot of NDVI values for 2004 Rainbow",
          xlab="NDVI")

#make a qqplot to check normal distribution
ggqqplot(rr1$NDVI,
         main="Q-Q plot for Rainbow Glacier")

#ttest of Boulder NDVI vs Rainbow NDVI 
BR_ttest <- t.test(bb1$NDVI,rr1$NDVI, paired = FALSE, var.equal = FALSE)
BR_ttest



