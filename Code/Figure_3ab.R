##Preliminary- Code for manuscript Marine heatwaves drive range contraction and alternative states
## By Nur Arafeh Dalmau, started September 2022. Last updated late 2024.

## Creates Figure 3ab



library (dplyr)
library(stringr)
library(ggpubr)
library(devtools)
library(readr)
library(gridExtra)
library(gtable)
library(cowplot)
library(tidyr)
library(ggplot2)

#Load data
D_Mac <- read.csv("Data/Species/Giant_Kelp_T_22_23.csv")
D_Eck <- read.csv("Data/Species/Palm_Kelp_T_22_23.csv")
D_Lob <- read.csv("Data/Species/Lobster_T_22_23.csv")
D_She <- read.csv("Data/Species/Sheephead_T_22_23.csv")
D_Urc <- read.csv("Data/Species/Urchins_T_22_23.csv")

## Estimate site means and CI for each species

##Giant Kelp

D_Mac2 <- D_Mac %>% group_by (Site, Year, Lat) %>% 
  summarise (mean = mean(Density), sd=sd(Density), CI = 1.96*(sd(Density)/sqrt((count))), 
             CI_upper = (CI + mean), CI_lower = (mean-CI)) %>% 
  ungroup() %>%
  distinct() %>% 
  na.omit()

D_Mac2[D_Mac2 < 0] <- 0

##Palm Kelp

D_Eck2 <- D_Eck %>% group_by (Site, Year, Lat) %>% 
  summarise (mean = mean(Density), sd=sd(Density), CI = 1.96*(sd(Density)/sqrt((count))), 
             CI_upper = (CI + mean), CI_lower = (mean-CI)) %>% 
  ungroup() %>%
  distinct() %>% 
  na.omit()

D_Eck2[D_Eck2 < 0] <- 0

#Lobster

D_Lob2 <- D_Lob %>% group_by (Site, Year, Lat) %>% 
  summarise (mean = mean(Density), sd=sd(Density), CI = 1.96*(sd(Density)/sqrt((count))), 
             CI_upper = (CI + mean), CI_lower = (mean-CI)) %>% 
  ungroup() %>%
  distinct() %>% 
  na.omit()

D_Lob2[D_Lob2 < 0] <- 0

##Sheephead

D_She2 <- D_She %>% group_by (Site, Year, Lat) %>% 
  summarise (mean = mean(Density_Biomass), sd=sd(Density_Biomass), CI = 1.96*(sd(Density_Biomass)/sqrt((count))), 
             CI_upper = (CI + mean), CI_lower = (mean-CI)) %>% 
  ungroup() %>%
  distinct() %>% 
  na.omit()

D_She2[D_She2 < 0] <- 0

##Urchins

D_Urc2 <- D_Urc %>% group_by (Site, Year, Lat) %>% 
  summarise (mean = mean(Density), sd=sd(Density), CI = 1.96*(sd(Density)/sqrt((count))), 
             CI_upper = (CI + mean), CI_lower = (mean-CI)) %>% 
  ungroup() %>%
  distinct() %>% 
  na.omit()

D_Urc2[D_Urc2 < 0] <- 0

#Macrocystis

p1 <- ggplot(D_Mac2, aes(Lat)) +
  geom_errorbar(size = 0.3, aes(ymin=CI_lower, ymax=CI_upper, width=0.03)) +
  geom_point(size = 2, aes (y=mean, shape = factor(Year), fill = factor(Year)), stroke = 0.2, color = "black")+
  #For stroke and empty circle  geom_point(size = 3, aes(y= mean), shape = 1, colour = "#7CB342", stroke = 1.5) +
  xlab("") + ylab("Giant Kelp  density"~(m^{-2})) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.x = element_blank(),
                axis.title.y = element_text(size = 8, margin = margin(t = -1)),
                axis.title.x = element_blank()
                ) +
  scale_shape_manual(values = c("2022" = 24, "2023" = 21)) +
  scale_fill_manual(values = c("2022" = alpha("#7CB342", 0.5), "2023" = alpha("#7CB342", 1))) +
                      guides(shape = "none", fill = "none") # Remove the legend for shape and fill) # Different opacities for different years

p1


#Ecklonia

p2 <- ggplot(D_Eck2, aes(Lat)) +
  geom_errorbar(size = 0.3, aes(ymin=CI_lower, ymax=CI_upper, width=0.03)) +
  geom_point(size = 2, aes (y=mean, shape = factor(Year), fill = factor(Year)), stroke = 0.2, color = "black")+
  #For stroke and empty circle  geom_point(size = 3, aes(y= mean), shape = 1, colour = "#7CB342", stroke = 1.5) +
  xlab("") + ylab("Palm Kelp  density"~(m^{-2})) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 8, margin = margin(t = -1)),
        axis.title.x = element_blank()
  ) +
  scale_shape_manual(values = c("2022" = 24, "2023" = 21)) +
  scale_fill_manual(values = c("2022" = alpha("#C0CA00", 0.5), "2023" = alpha("#C0CA00", 1))) +
                      guides(shape = "none", fill = "none") # Remove the legend for shape and fill) # Different opacities for different years

p2


#Lobsters

p3 <- ggplot(D_Lob2, aes(Lat)) +
  geom_errorbar(size = 0.3, aes(ymin=CI_lower, ymax=CI_upper, width=0.03)) +
  geom_point(size = 2, aes (y=mean, shape = factor(Year), fill = factor(Year)), stroke = 0.2, color = "black")+
  #For stroke and empty circle  geom_point(size = 3, aes(y= mean), shape = 1, colour = "#7CB342", stroke = 1.5) +
  xlab("") + ylab("Lobster  density"~(m^{-2})) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 8, margin = margin(t = -1)),
        axis.title.x = element_blank()
  ) +
  scale_shape_manual(values = c("2022" = 24, "2023" = 21)) +
  scale_fill_manual(values = c("2022" = alpha("#FFAF00", 0.5), "2023" = alpha("#FFAF00", 1))) +
  guides(shape = "none", fill = "none") # Remove the legend for shape and fill) # Different opacities for different years

p3


#Sheephead

p4 <- ggplot(D_She2, aes(Lat)) +
  geom_errorbar(size = 0.3, aes(ymin=CI_lower, ymax=CI_upper, width=0.03)) +
  geom_point(size = 2, aes (y=mean, shape = factor(Year), fill = factor(Year)), stroke = 0.2, color = "black")+
  #For stroke and empty circle  geom_point(size = 3, aes(y= mean), shape = 1, colour = "#7CB342", stroke = 1.5) +
  xlab("") + ylab("Sheephead  biomass"~(m^{-2})) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 8, margin = margin(t = -1)),
        axis.title.x = element_blank(),
  ) +
  scale_shape_manual(values = c("2022" = 24, "2023" = 21, "2024" = 22)) +
  scale_fill_manual(values = c("2022" = alpha("#FF7043", 0.5), "2023" = alpha("#FF7043", 1),  "2024" = alpha("#FF7043", 0.75))) +
  guides(shape = "none", fill = "none") # Remove the legend for shape and fill) # Different opacities for different years

p4

#Urchins

p5 <- ggplot(D_Urc2, aes(Lat)) +
  geom_errorbar(size = 0.3, aes(ymin=CI_lower, ymax=CI_upper, width=0.03)) +
  geom_point(size = 2, aes (y=mean, shape = factor(Year), fill = factor(Year)), stroke = 0.2, color = "black")+
  #For stroke and empty circle  geom_point(size = 3, aes(y= mean), shape = 1, colour = "#7CB342", stroke = 1.5) +
  xlab("") + ylab("Urchins density"~(m^{-2})) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 8, margin = margin(t = -1)),
        axis.title.x = element_blank()
  ) +
  scale_shape_manual(values = c("2022" = 24, "2023" = 21)) +
  scale_fill_manual(values = c("2022" = alpha("#00AAC1", 0.5), "2023" = alpha("#00AAC1", 1))) +
  guides(shape = "none", fill = "none") +
  scale_x_continuous(breaks = seq(floor(min(D_Urc2$Lat)), ceiling(max(D_Urc2$Lat)), by = 0.5)) # Set the breaks for the x-axis
# Remove the legend for shape and fill) # Different opacities for different years

p5


######Figure 3b

##Prepare data 

Lob_urc <- merge (D_Lob, D_Urc, 
                  by = c("Site", "Sub_site", "Transect", "Year")) %>%
  select(-"Sum_abundance.y", -"Sum_abundance.x")


####Plot Urchins vs Lobster

p6 <- ggplot(Lob_urc, aes(Density.y)) +
  geom_point(size = 1.5, aes(y= Density.x), shape = 1, color = "#FFAF00", stroke = 0.6) +
  geom_line(size = 1, data = fit_dat, # With the fit data
            aes(x = x, y = fit), # Plot the fitted line
            col = "#00AAC1") +
  xlab("Sea Urchins density "~(m^{-2})) + ylab("Lobster density   "~(m^{-2})) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    #axis.text.x = element_text(size = 10),
    #axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10, margin = margin(t = -1)),
    axis.title.y = element_text(size = 10, margin = margin(r = 1))
  )
p6


##Urchins vs Sheephead size

She_urc <- merge (sheephead_f_2, Urchins, 
                  by = c("Site", "Sub_site", "Transect", "Year")) %>%
  select(-"Density_S", -"Sum_abundance", -"Sum_S", -"B")


p7 <- ggplot(She_urc, aes(Density)) +
  geom_point(size = 1.5, aes(y= Density_Biomass), shape = 1, color = "#FF7043", stroke = 0.6) +
  geom_line(size = 1, data = fit_dat, # With the fit data
            aes(x = x, y = fit), # Plot the fitted line
            col = "#00AAC1") +
  xlab("Sea Urchins density "~(m^{-2})) + ylab("Sheephead biomass   g"~(m^{-2})) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size = 1),
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank(),
        #axis.text.x = element_text(size = 10),
        #axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, margin = margin(t = -1)),
        axis.title.y = element_text(size = 10, margin = margin(r = 1))
  )
p7

#Mac urchins

Mac_Urc <- merge (D_Mac, D_Urc, 
                  by = c("Site", "Sub_site", "Transect", "Year")) 


p8 <- ggplot(Mac_Urc, aes(Density.y)) +
  geom_point(size = 1.5, aes(y= Density.x), shape = 1, color = "#7CB342", stroke = 0.6) +
  geom_line(size = 1, data = fit_dat, # With the fit data
            aes(x = x, y = fit), # Plot the fitted line
            col = "#00AAC1") +
  xlab("Sea Urchins density "~(m^{-2})) + ylab("Giant Kelp density   "~(m^{-2})) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.text.x = element_text(size = 10),
    #axis.text.x = element_text(size = 10),
    #axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10, margin = margin(t = -1)),
    axis.title.y = element_text(size = 10, margin = margin(r = 1))
  )

p8


#Ecklonia vs Mac

Mac_Palm <- merge (D_Mac, D_Eck, 
                  by = c("Site", "Sub_site", "Transect", "Year"))


p9 <- ggplot(Mac_Palm, aes(Density.y)) +
  geom_point(size = 1.5, aes(y= Density.x), shape = 1, color = "#C0CA00", stroke = 0.6) +
  geom_line(size = 1, data = fit_dat, # With the fit data
            aes(x = x, y = fit), # Plot the fitted line
            col = "#00AAC1") +
  xlab("Palm Kelp density   "~(m^{-2})) + ylab("Giant Kelp density "~(m^{-2})) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    #axis.text.x = element_text(size = 10),
    #axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10, margin = margin(t = -1)),
    axis.title.y = element_text(size = 10, margin = margin(r = 1))
  )

p9


##Giant Kelp vs Sheephead size-----NOt worth it

#Fig 6X4
# Combine the plots in a column using cowplot
plot_grid(p8, p9, p6, p7, ncol = 2, align = "v", axis = "lr", vjust = 1)

#Fig 7.2X4.3
# Combine the plots in a column using cowplot
plot_grid(p1, p2, p3, p4,p5, ncol = 1, align = "v", axis = "lr", vjust = 1)



