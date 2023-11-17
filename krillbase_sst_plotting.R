library(tidyverse)
library(sf)
library(lwgeom)
library(here)
library(ggplot2)
library(scales)
library(ggspatial)
library(mapproj)
library(ggOceanMaps)
library(rnaturalearth)
library(viridis)
library(ggpmisc)
library (patchwork)
library(adehabitatHR)
library(raster)
library(sp)
##### read data ######

areas48<-st_read("C:/Users/jenfree/OneDrive - NERC/PEW/shapefiles/ccamlr_area_48/asd-shapefile-WGS84.shp")
areas48<-areas48[c(2,4,5,6,10),]
#areas48 <-areas48 %>% sf::st_transform("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

setwd("C:/Users/jenfree/Desktop/krill_infographic/krillbase") # set wd to folder "/files"

dat<-read.csv("krillbase_data.csv")
summary(dat)

dat2 <- dat %>% 
  sf::st_as_sf(coords = c("LONGITUDE","LATITUDE")) %>% 
  sf::st_set_crs(4326) #%>% 
  #sf::st_transform("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

within_48<-sapply(st_intersects(dat2, areas48),function(x){length(x)==0})
dat2$within_48<-within_48

dat_48<-subset(dat2, dat2$within_48 =="FALSE")

dat_48$yearcat<- "yrcat1"
dat_48$yearcat[dat_48$SEASON >=1976 & dat_48$SEASON <1995]<-"yrcat2"
dat_48$yearcat[dat_48$SEASON >=1995 & dat_48$SEASON <2017]<-"yrcat3"
dat_48 %>% count(yearcat)

dat_48sp <- sf:::as_Spatial(dat_48)

#kde.output <- kernelUD(dat_48sp[dat_48sp$yearcat == "1",], h="href", grid = 1000)
#plot(kde.output)

ras_dom<-raster(xmn=-60, xmx=-20, ymn=-80, ymx=-50,
                crs= 4326,
                resolution=c(1,1), vals=NA)




#####zos75map######


dt <- expand.grid(lon = c(-20, -60), lat = c(-80, -50))

zos_75map<-basemap(data = dt,bathymetry=F,rotate=T)+#bathy.style = "rbg"
  geom_sf(data = areas48, col="black",fill = NA, linewidth=1, alpha=0.5)+ ##00BCD8
  layer_spatial(dat_48, mapping = aes(size=STANDARDISED_KRILL_UNDER_1M2,col=STANDARDISED_KRILL_UNDER_1M2),pch=19,alpha=0.6) +
  scale_size_continuous(name= "STANDARDISED_KRILL_UNDER_1M2", range=c(0.5,12)) +
  scale_colour_viridis(name= "STANDARDISED_KRILL_UNDER_1M2", option="magma") +
  facet_wrap(~yearcat)+
  guides( colour = guide_legend()) +
  #annotation_scale(location = "br", width_hint = 0.25,height = unit(0.2, "cm"),text_cex = 0.5) +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA),
    panel.grid = element_line(colour = "grey"),
    strip.text.x = element_text(size = 12),
    strip.text.y = element_text(size = 12),
    axis.line = element_line(colour = "grey"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position="right")
zos_75map

ggsave(file = "C:/Users/jenfree/Desktop/krill_infographic/krillbase_yearcat.png", zos_75map, scale=2,device= "png", dpi = 500, width=6, height=3)
dev.set(dev.next())

