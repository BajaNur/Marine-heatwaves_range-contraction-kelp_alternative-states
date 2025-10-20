##Preliminary- Code for manuscript Marine heatwaves drive range contraction and alternative states
## By Nur Arafeh Dalmau, started September 2022. Last updated late 2024.
## Creates Figure 1

# Part of the code comes from Arafeh-Dalmau et al. Frontiers in Marine Science 2019
# Original code written by Dave S. Adapted by Nur for new paper.
# Original code May 2019, updated late 2022. Final version late 2024.

library(tidyverse)   
library(cowplot)
library(tsoutliers)
library(TSA)
library(forecast)
library(ncdf4)
library(lubridate)


###Load species data
Eisenia_Ensenada <- read.csv ("Data/Ecklonia_North_2011_2023.csv")
Urchins_Ensenada <- read.csv ("Data/Urchins_North_2011_2023.csv")
Eisenia_Eugenia <- read.csv ("Data/Ecklonia_Eugenia_2006_2023.csv")
Urchins_Eugenia <- read.csv ("Data/Urchins_Eugenia_2006_2023.csv")
Eisenia_Asuncion <- read.csv ("Data/Ecklonia_Asuncion_1994_2023.csv")
Urchins_Asuncion <- read.csv ("Data/Urchins_Asucnion_2022_2023.csv")

##Load kelp data
Ensenada <- read.csv("Data/Landsat_Processed/Landsat_Ensenada.csv")
Punta_Eugenia <- read.csv("Data/Landsat_Processed/Landsat_Punta_Eugenia.csv")
Asuncion <- read.csv("Data/Landsat_Processed/Landsat_Asuncion.csv")

##I started by working seperately for the three regions. Leave 1999, before MHWs.

rldat_En <- Ensenada %>% filter (Year > 1990)
rldat_Eu <- Punta_Eugenia %>% filter (Year > 1999) 
rldat_Eu2 <- Punta_Eugenia %>% filter (Year > 1990) 
rldat_As <- Asuncion %>% filter (Year > 1992)
rldat_As2 <- Asuncion %>% filter (Year > 1990)


# How unusual are kelp values after the MHW?
ggplot(rldat_As) +
  geom_line(aes(x = Quarter, y = Area), alpha = .3) +
  geom_point(aes(x = Quarter, y = Area)) +
  facet_wrap(vars(Location)) +
  theme_bw()

with(rldat_As, table(Quarter, Year, Location)) # OK, so regular and balanced

tsEN <- ts(rldat_En$Area[which(rldat_En$Location == "Ensenada")])
cbind(n = 1:132, rldat_En[which(rldat_En$Location == "Ensenada"),])[,1:4]
ENout <- tso(tsEN, maxit.iloop = 1000, maxit.oloop = 1000)
ENout
plot(ENout)


# Extract time values for the ts object
quarter_times <- seq(from = 1991, to = 2023.75, by = 0.25)

# Build the data frame of outliers with Quarter info
outliers_df_EN <- data.frame(
  Index = ENout$outliers$ind,
  Quarter = quarter_times[ENout$outliers$ind],
  type = ENout$outliers$type,
  value = ENout$outliers$coefhat,
  ypos = max(tsEN, na.rm = TRUE) * 0.95
)


tsEu <- ts(rldat_Eu$Area[which(rldat_Eu$Location == "Punta_Eugenia")])
cbind(n = 1:96, rldat_Eu[which(rldat_Eu$Location == "Punta_Eugenia"),])[,1:4]
Euout <- tso(tsEu)
Euout
plot(Euout)

# Extract time values for the ts object
quarter_times_Eu <- seq(from = 2000, to = 2023.75, by = 0.25)

# Build the data frame of outliers with Quarter info
outliers_df_Eu <- data.frame(
  Index = Euout$outliers$ind,
  Quarter = quarter_times_Eu[Euout$outliers$ind],
  type = Euout$outliers$type,
  value = Euout$outliers$coefhat,
  ypos = max(tsEu, na.rm = TRUE) * 0.95
)

tsAs <- ts(rldat_As$Area[which(rldat_As$Location == "Asuncion")])
cbind(n = 1:124, rldat_As[which(rldat_As$Location == "Asuncion"),])[,1:4]
Asout <- tso(tsAs, maxit.iloop = 1000, maxit.oloop = 1000)
Asout
plot(Asout)

# 4. Extract time values for the ts object
quarter_times_As <- seq(from = 1993, to = 2023.75, by = 0.25)

# 5. Build the data frame of outliers with Quarter info
outliers_df_AS <- data.frame(
  Index = Asout$outliers$ind,
  Quarter = quarter_times_As[Asout$outliers$ind],
  type = Asout$outliers$type,
  value = Asout$outliers$coefhat,
  ypos = max(tsAs, na.rm = TRUE) * 0.95
)


rldat_En$effects <- NA # Make a new, empty variable within which to place time series effects
rldat_En$effects <-  as.numeric(ENout$effects)
rldat_En$Effects <- rldat_En$effects + abs(min(rldat_En$effects, na.rm = TRUE))

rldat_En2 <-rldat_En %>% group_by(Year) %>% summarise(area_m = mean(Area)) 

rldat_Eu$effects <- NA # Make a new, empty variable within which to place time series effects
rldat_Eu$effects <-  as.numeric(Euout$effects)
rldat_Eu$Effects <- rldat_Eu$effects + mean(rldat_Eu$Area,na.rm = TRUE)

rldat_Eu2 <-rldat_Eu2 %>% group_by(Year) %>% mutate(area_m = mean(Area)) 


rldat_As$effects <- NA # Make a new, empty variable within which to place time series effects
rldat_As$effects <-  as.numeric(Asout$effects)
rldat_As$Effects <- rldat_As$effects + abs(min(rldat_As$effects, na.rm = TRUE))

rldat_As2 <-rldat_As %>% group_by(Year) %>% mutate(area_m = mean(Area)) 


##Plot. Adjust Urchins and Ecklonia counts with the Kelp area (for visualization)

Eisenia_Ensenada2 <- Eisenia_Ensenada %>% mutate(m = Density_F*490000)
Urchins_Ensenada2 <- Urchins_Ensenada %>% mutate(m = mean*490000)
Eisenia_Eugenia2 <- Eisenia_Eugenia %>% mutate(m = mean*3000000)
Urchins_Eugenia2 <- Urchins_Eugenia %>% mutate(m = mean*3000000)
Eisenia_Asuncion2 <- Eisenia_Asuncion %>% mutate(m = Density_E*470000)
Urchins_Asuncion2 <- Urchins_Asuncion %>% mutate(m2 = m*470000)

##Plot time series of species, remote sensing and ARIMA

P_En <- ggplot(rldat_En) +
  annotate("text", x = 1991, y = Inf, vjust = 1.8,  # vjust > 1 moves it downward from top edge
           label = "Ensenada", hjust = 0.1, size = 4) +
geom_rect(xmin = 2014, xmax = 2017, ymin = -Inf, ymax = Inf, fill = "#CC3333", alpha = 0.002) +
  geom_rect(xmin = 1997, xmax = 1999, ymin = -Inf, ymax = Inf, fill = "#CC3333", alpha = 0.002) +
  xlab("Year") + ylab("Giant Kelp area"~(m^{2}))+#geom_point(size = 2, aes(x = Quarter, y = Area), colour = "#7CB342") +
  geom_line(size= 1, aes(x = Quarter, y = Area), colour = "#00a36bff", alpha = .5) +
  geom_step(aes(x = Quarter, y = Effects), colour = "#D32F2F", size = 0.5) +
  geom_line(data = Urchins_Ensenada2, size = 1.5, aes(x = Year, y = m), colour = "#B284BE", alpha = .75) +
  geom_line(data = Eisenia_Ensenada2, size=1.5, aes(x = Year, y = m), colour = "#5DADE2", alpha = .75) +
  xlab("") + ylab("")+
  scale_x_continuous(breaks = seq(1990, 2023, by = 2)) +  # Show ticks every 2 years
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
                     sec.axis = sec_axis(~.x/490000, name = ""))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
  )

P_En

P_Eu <- ggplot(rldat_Eu2) +
  annotate("text", x = 1991, y = Inf, vjust = 1.8,  # vjust > 1 moves it downward from top edge
           label = "Punta Eugenia", hjust = 0.1, size = 4) +
  #geom_point(size = 1, aes(x = Quarter, y = Area), colour = "#00a36bff") +
  geom_rect(xmin = 2014, xmax = 2017, ymin = -Inf, ymax = Inf, fill = "#CC3333", alpha = 0.0055) +
  geom_rect(xmin = 1997, xmax = 1999, ymin = -Inf, ymax = Inf, fill = "#CC3333", alpha = 0.0055) +
  geom_line(size=1, aes(x = Quarter, y = Area), colour = "#00a36bff", alpha = .5) +
  geom_step(data = rldat_Eu, aes(x = Quarter, y = Effects), colour = "#D32F2F", size = 0.5) +
  geom_line(data = Urchins_Eugenia2, size = 1.5, aes(x = ANIO, y = m), colour = "#B284BE", alpha = 0.75) +
  geom_line(data = Eisenia_Eugenia2, size = 1.5, aes(x = ANIO, y = m), colour = "#5DADE2", alpha =0.75) +
  xlab("") + ylab("Giant Kelp Area"~(m^{2}))+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))+
  theme(legend.position="none") +
  scale_x_continuous(breaks = seq(1990, 2023, by = 2)) +  # Show ticks every 2 years
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
                     sec.axis = sec_axis(~.x/3000000, name = "Density" ~(m^{-2})))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
  )
    
P_Eu


P_As <- ggplot(rldat_As2) +
  annotate("text", x = 1991, y = Inf, vjust = 1.8,  # vjust > 1 moves it downward from top edge
           label = "Bahía Asunción", hjust = 0.1, size = 4) +
  #geom_point(size = 2, aes(x = Quarter, y = Area), colour = "#7CB342") +
  geom_rect(xmin = 1997, xmax = 1999, ymin = -Inf, ymax = Inf, fill = "#CC3333", alpha = 0.0055) +
  geom_rect(xmin = 2014, xmax = 2017, ymin = -Inf, ymax = Inf, fill = "#CC3333", alpha = 0.0055) +
  geom_line(data = Urchins_Asuncion2, size = 1.5, aes(x = Year, y = m2), colour = "#B284BE", alpha = 0.75) +
  geom_line(size=1, aes(x = Quarter, y = Area), colour = "#00a36bff", alpha = .5)+
  geom_step(data = rldat_As, aes(x = Quarter, y = Effects), colour = "#D32F2F", size = 0.5) +
  #geom_point(data = Ecklonia_A, size = 2, aes(x = Year, y = m), colour = "#B284BE") +
  geom_line(data = Eisenia_Asuncion2, size= 1.5, aes(x = Year, y = m), colour = "#5DADE2", alpha = 0.75) +
xlab("Year") + ylab("")+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))+
  theme(legend.position="none") +
  scale_x_continuous(
    breaks = seq(1990, 2023, by = 2)
  )+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
                     sec.axis = sec_axis(~.x/470000, name = ""))+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
  )

P_As



L_Imp <- plot_grid(
  P_En, P_Eu, P_As,
  ncol = 1,
  align = "v",
  axis = "lr",
  vjust = 1,
  labels = c("a", "b", "c"),   # <-- add panel labels here
  label_size = 14,             # <-- adjust size if needed
  label_fontface = "bold"      # <-- optional: make labels bold
)

L_Imp

##Export 900X700

