
##############
#title: Code to analyse data and plot maps presented in the infographic submitted to the SCAR-ABI competition
#Author: Jen Freer, British Antarctic Survey
#Date: 17/11/23
#contact: jenfree@bas.ac.uk
##############

##### packages ######
library(tidyverse)
library(sf)
library(lwgeom)
library(ggplot2)
library(scales)
library(ggspatial)
library(mapproj)
library(rnaturalearth)
library(viridis)
library(ggpmisc)
library(raster)
library(sp)
library(stars)
library(RColorBrewer)


##### read data ######
setwd("C:/Users/jenfree/OneDrive - NERC/General - BAS Ecosystems/SCAR_infographic_comp/code") # set wd to folder "/files"

# shapefile to clip to CCAMLR area 48 
#DATA FROM: https://gis.ccamlr.org/
areas48<-st_read("./area_shapefile/asd-shapefile-WGS84.shp") 
areas48<-areas48[c(2,4,5,6,10),] # only require subareas 48.1-48.5

#krillbase records
#DATA FROM: https://www.bas.ac.uk/project/krillbase/ 
dat<-read.csv("./krillbase_data/krillbase_data.csv")

#raster of temperature change
#DATA FROM: 
temp<-raster("./sst_data/sst_dif_summer_ycat1-3.tif") 

#create empty raster grid
ras_dom<-raster(xmn=-80, xmx=-10, ymn=-70, ymx=-50,
                crs= 4326,
                resolution=c(5,5), vals=NA)

temp<-flip(temp, direction='y') #flip coords
temp<-crop(temp,ras_dom) # crop 
        
temp_star<-st_as_stars(temp) %>% st_set_crs(4326) %>% na.omit() #convert to stars object for plotting 



##### filter data only data with standardised krill values ######

dat<-subset(dat, !is.na(dat$STANDARDISED_KRILL_UNDER_1M2))
summary(dat)


##### filter data only data within subarea 48 ######

#make krill data spatial object
dat2 <- dat %>% 
  sf::st_as_sf(coords = c("LONGITUDE","LATITUDE")) %>% 
  sf::st_set_crs(4326) 

#get intersection of area shapefile and krill data
within_48<-sapply(st_intersects(dat2, areas48),function(x){length(x)==0})
dat2$within_48<-within_48

#subset krill data to those within area shapefile
dat_48<-subset(dat2, dat2$within_48 =="FALSE")

#remove 
rm(dat,dat2)

##### assign year and latitude category ######


dat_48<-dat_48 %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

dat_48$yearcat[dat_48$SEASON >=1925 & dat_48$SEASON <1955]<-1
dat_48$yearcat[dat_48$SEASON >=1995 & dat_48$SEASON <2017]<-2
dat_48$latcat[dat_48$lat > -60]<-"N"
dat_48$latcat[dat_48$lat <= -60]<-"S"
dat_48$count<-1

#new objects for each yearcat

dat_48_1 <- dat_48 %>% filter(yearcat == 1)
dat_48_2 <- dat_48 %>% filter(yearcat == 2)

###### get fraction of krill density in each yearcat, north and south of 60oS ######

summary<-dat_48 %>%
  group_by(yearcat, latcat) %>%
  summarise(sum = sum(STANDARDISED_KRILL_UNDER_1M2))


summary$total[summary$yearcat == 1]<-sum(dat_48_1$STANDARDISED_KRILL_UNDER_1M2)
summary$total[summary$yearcat == 2]<-sum(dat_48_2$STANDARDISED_KRILL_UNDER_1M2)

summary$fraction<-(summary$sum/summary$total)*100

##### convert point data to raster grid ######

dat_48_1_sp <- sf:::as_Spatial(dat_48_1)
dat_48_2_sp <- sf:::as_Spatial(dat_48_2)

rm(dat_48_1,dat_48_2)

# create raster to show number of data points per grid cell, yearcat1
r1 <- rasterize(dat_48_1_sp, field="count", ras_dom, fun="sum")
plot(r1)
freq(r1)

#remove cells with fewer than 10 data points
r1 <- reclassify(r1, c(0,9,NA, 9,1000,1))
plot(r1)

#repeat yearcat2
r2 <- rasterize(dat_48_2_sp, field="count", ras_dom, fun="sum")
plot(r2)
freq(r2)
r2 <- reclassify(r2, c(0,9,NA, 9,2000,1))
plot(r2)


# create raster to show krill density per grid cell, yearcat1, and use previous raster to remove cells with low data
r1_krill <- rasterize(dat_48_1_sp, field="STANDARDISED_KRILL_UNDER_1M2", ras_dom, fun=mean, na.rm=T)
plot(r1_krill)
r1_krill_mask  <- mask(r1_krill,r1)

# repeat for yearcat2
r2_krill <- rasterize(dat_48_2_sp, field="STANDARDISED_KRILL_UNDER_1M2", ras_dom,fun=mean, na.rm=T)
plot(r2_krill)
r2_krill_mask  <- mask(r2_krill,r2)

#subtract yearcat2 from yearcat1 to get difference in krill density between time periods. Use log scale for easier representation
r_dif<-log(r2_krill_mask)-log(r1_krill_mask)

#convert to stars object for plotting
r_dif_star<-st_as_stars(r_dif) %>% na.omit()

#####plot maps ######


#get world shapefile 

world_sf <-  rnaturalearth::ne_countries(
  scale = "large",
  returnclass = "sf") 

#plot krill density difference

yrcat_dif_map <- ggplot() +
  geom_stars(data = r_dif_star) +
  geom_sf(data = world_sf, fill= "grey80") +
  coord_sf(xlim = c(-75, -25), ylim = c(-70, -50)) +
  scale_fill_gradient2(high ="#03051AFF", mid = "#B41658FF",low = "#FAEBDDFF", midpoint = 0,na.value=NA)+
  guides(fill=guide_colourbar(barwidth=30,label.position="bottom")) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_line(colour = "grey", linewidth=0.5),
    axis.ticks = element_line(colour = "grey", linewidth=0.5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position="bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=20))
yrcat_dif_map

#save
ggsave(file = "C:/Users/jenfree/OneDrive - NERC/General - BAS Ecosystems/SCAR_infographic_comp/images/yearcatdif4.png", yrcat_dif_map, scale=2,device= "png", dpi = 500, width=4, height=3)
dev.set(dev.next())

#get colorblind-friendly divergent palette
cols1<-brewer.pal(3,"OrRd")
cols2<-rev(brewer.pal(3,"BuPu"))

#plot map of temperature change
temp_map <- ggplot() +
  geom_stars(data = temp_star) +
  geom_sf(data = world_sf, fill= "grey80") +
  coord_sf(xlim = c(-75, -25), ylim = c(-70, -50)) +
  scale_fill_gradientn(na.value=NA,colours =c(cols2, "#F2F2F2", cols1) )+
  guides(fill=guide_colourbar(barwidth=30,label.position="bottom")) +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_line(colour = "grey", linewidth=0.5),
    axis.ticks = element_line(colour = "grey", linewidth=0.5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position="bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=20))
temp_map

#save
ggsave(file = "C:/Users/jenfree/OneDrive - NERC/General - BAS Ecosystems/SCAR_infographic_comp/images/temp5.png", temp_map, scale=2,device= "png", dpi = 500, width=4, height=3)
dev.set(dev.next())





