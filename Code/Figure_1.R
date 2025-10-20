##Preliminary- Code for manuscript Marine heatwaves drive range contraction and alternative states
## By Nur Arafeh Dalmau, started September 2022. Last updated late 2024.
## Creates Figure 1

library(dplyr)
library(ncdf4)
library(hablar)
library(naniar)
library(ggplot2)

##Load kelp data (Download data here: https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.74)
kelp <-  nc_open("Landsat/data here") 

## Important: Due to the size of Landsat data (2gb). 
## We provide the processed data used for this figure.

## Read processed before-after kelp data merged at 0.1 degree
Region <- read.csv("Data/Landsat_Processed/Before_After_0.1_Landsat.csv")

##We use baseline from 2000-2013, because before 2000 there are some regions with many NAs in the south
###Estimate baseline area at 0.1 degree bins

Before <- Region %>% filter (Year < 2014, Year > 1999)  %>% group_by (Latitude) %>%
  summarise (Area = mean (Area, na.rm = T)) %>% mutate (Area2 = sqrt(Area))
Before$Time <- "Before"


After <- Region %>%  filter (Year > 2016)  %>% group_by (Latitude) %>%
  summarise (Area = mean (Area, na.rm = T)) %>% mutate (Area2 = sqrt(Area))
After$Time <- "After"

Before$Area <- Before$Area*-1 
Before$Area2 <- Before$Area2*-1 

BeAf <- rbind (Before, After) 
##Plot

BeAf$Time <- factor(BeAf$Time, levels = c("Before", "After"))

p2 <- ggplot(BeAf, aes(x = Latitude , y = Area, fill = Time))+
  geom_bar(stat = "identity",  color = "black", size = 0.5, alpha = .6) +
  coord_flip() +
  xlab("Latitude") + ylab("Area"~(m^{-2}))+
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(colour = "black", size = 0.4),
    #axis.text.y = element_blank(),
    #axis.ticks.y = element_blank(),
    axis.line.x = element_line(colour = "black", size = 0.4)
  ) +
  scale_fill_manual(values = c("Before" = "#1A237E", "After" = "#B71C1C"))

p2
