library(tidyverse)

load(file = "/home/julian/Documents/WGFBIT/Barents_Sea/region_grid.RData")


a <- gr %>%
  group_by(MSFDhab) %>%
  summarise(otter = mean(Otter_SAR_2018, na.rm = TRUE)) %>%
  st_set_geometry(NULL)


b <- gr %>%
  group_by(MSFDhab) %>%
  summarise(otter = mean(rbs2018, na.rm = TRUE)) %>%
  st_set_geometry(NULL)


 # get colorscales
 bluegreen <- c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
 yellowred <- c("#ffffb2","#fed976","#feb24c","#fd8d3c","#f03b20","#bd0026")
 purples   <- c("#f2f0f7", "#dadaeb","#bcbddc","#9e9ac8","#756bb1","#54278f")

library(rworldmap)
library(broom)
 worldMap <- getMap(resolution = "high")
 worldMap@data$id = rownames(worldMap@data)
 worldMap.points = tidy(x = worldMap, region = "id")

   coordslim <- c(min(gr$lon) - 1,max(gr$lon) + 1,min(gr$lat) - 1,max(gr$lat) + 1)
   coordxmap <- round(seq(min(gr$lon),max(gr$lon),length.out = 4))
   coordymap <- round(seq(min(gr$lat),max(gr$lat),length.out = 4))

## Median longitude
gr$cat <- round(gr$medLong)
gr$cat[gr$cat == 13] <- 12

figmap <- ggplot() +
  geom_point(data=gr, aes(x=lon, y=lat, colour=factor(cat)),
#  geom_point(data=gr, aes(x=lon, y=lat, colour=medLong),
             shape = 15, size=1) +
  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey") +
 scale_colour_manual(values=bluegreen, na.value = "grey50", name  ="median longevity",
labels = c("7", "8", "9", "10", "11", "12+")) +
   theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
        legend.text   = element_text(size=11),
        legend.title  = element_text(size=11)) +
  scale_x_continuous(breaks=coordxmap)+
  scale_y_continuous(breaks=coordymap)+
  coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4])) +
  guides(colour = guide_legend(override.aes = list(size=5)))

ggsave("/home/julian/Documents/WGFBIT/Barents_Sea/Barents_mean_longevity.png", plot = figmap, width = 15, height = 13, units = "cm")

## State 2018
   quat<-c(-1,0,0.1,0.3,0.5,0.7,0.9,1.01)
     gr$cat<- as.factor(cut(gr$rbs2018,quat,right=FALSE))

figmap <- ggplot() +
  geom_point(data=gr, aes(x=lon, y=lat, colour=factor(cat)),shape=15,size = 1,na.rm=T) +
       scale_colour_manual(values=rev(yellowred),na.value = "grey50",name  ="State (PD model)",
                           labels=c("0-0.1","0.1-0.3","0.3-0.5","0.5-0.7","0.7-0.9","0.9-1"))
     figmap <- figmap +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
     figmap <- figmap +   theme(panel.background=element_blank(),
                                axis.text.y   = element_text(size=16),
                                axis.text.x   = element_text(size=16),
                                axis.title.y  = element_text(size=16),
                                axis.title.x  = element_text(size=16),
                                panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                                legend.text   = element_text(size=11),
                                legend.title  = element_text(size=11))+
       scale_x_continuous(breaks=coordxmap)+
       scale_y_continuous(breaks=coordymap)+
       coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
     figmap<- figmap +   guides(colour = guide_legend(override.aes = list(size=5)))


ggsave("/home/julian/Documents/WGFBIT/Barents_Sea/Barents_State_2018.png", plot = figmap, width = 15, height = 13, units = "cm")

## Fishing effort

quat<-c(-1,0,0.1,0.5,1,5,10,100)
gr$cat<- as.factor(cut(gr$Otter_SAR_2018,quat,right=FALSE))

figmap <- ggplot() + geom_point(data=gr, aes(x=lon, y=lat, colour=factor(cat)),shape=15,size=1,na.rm=T) +
  scale_colour_manual(values=c("grey50", purples), na.value = "grey50",name  ="Surf abrasion",
                      labels=c("No fishing", "0-0.1", "0.1-0.5","0.5-1","1-5","5-10",">10","no #FIXME: shing")) +
  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey") +
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
        legend.text   = element_text(size=11),
        legend.title  = element_text(size=11))+
  scale_x_continuous(breaks=coordxmap)+
  scale_y_continuous(breaks=coordymap)+
  coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4])) +
  guides(colour = guide_legend(override.aes = list(size=5)))

ggsave("/home/julian/Documents/WGFBIT/Barents_Sea/Barents_SAR_2018.png", plot = figmap, width = 15, height = 13, units = "cm")
