##Preliminary- Code for manuscript Marine heatwaves drive range contraction and alternative states
## By Nur Arafeh Dalmau, started September 2023. Last updated late 2024.
## Creates Figure 2
## We provide processed data for MHW and Landsat

library (dplyr)
library (ggplot2)

###Figure 2a

MHWs <- read.csv("Data/MHWs/Cummulative_Int.csv") %>%
  filter(year > 1999) %>%
  group_by(region, year) %>%
  summarise(
    Cum_Int_reg = mean(Cum_Int),
    sd_Cum_Int = sd(Cum_Int),
    n_Cum_Int = n(),
    CI_lower = Cum_Int_reg - 1.96 * (sd_Cum_Int / sqrt(n_Cum_Int)),
    CI_upper = Cum_Int_reg + 1.96 * (sd_Cum_Int / sqrt(n_Cum_Int))
  ) %>%
  ungroup() %>%
  mutate(period = case_when(
    year >= 2000 & year <= 2013 ~ "before",
    year >= 2014 & year <= 2016 ~ "during",
    year >= 2017 & year <= 2023 ~ "after"
  ))

                                    
# Load Kelp data
Kelp <- read.csv("Data/Landsat_Processed/Timeseries_kelp_ecoregion.csv")

# Merge the two dataframes based on "year" and "region"
combined_df <- merge(MHWs, Kelp, by = c("year", "region")) %>% select(-X)

Northern_Baja <- combined_df %>% filter (region == "Northern Baja California") %>%
  mutate(MHWs = (1.39*(Cum_Int_reg)), area_hect = (area/10000)) 
Central_Baja <- combined_df %>% filter (region == "Central Baja California") %>%
  mutate(MHWs = (1.39*(Cum_Int_reg)), area_hect = (area/10000)) 

##### Define the limits for the primary y-axis (Cumulative Marine Heatwave intensity)
y_min_MHWs <- 0
y_max_MHWs <- 1250
multiplier <- 1.39


## Specify color
MHW_color <- rgb(148/255, 49/255, 198/255)
kelp_color <- rgb(0/255, 163/255, 107/255)

# Northern Baja Plot: Only Left Y-axis title

P_N_BC <- ggplot(Northern_Baja) +
  geom_line(size=1, aes(x = year, y = area_hect), colour = kelp_color) +
  geom_col(aes(x = year, y = MHWs), fill = MHW_color, alpha = 0.5, width = 0.85) +
  xlab("Year") + 
  scale_y_continuous(name = "Giant Kelp Area (hectares)",  # Only show left y-axis title and values
                     limits = c(y_min_MHWs, y_max_MHWs),
                     expand = expansion(mult = c(0.05, 0.05)),
                     sec.axis = sec_axis(~.x/multiplier, name = NULL)) +  # Remove right y-axis title and values
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y.right = element_blank(),  # Hide right y-axis values
        axis.ticks.y.right = element_blank()  # Hide right y-axis ticks
  )+
  annotate("text", x = min(Northern_Baja$year) + 1, y = y_max_MHWs, 
           label = "Northern Baja California", 
           hjust = 0, vjust = 1, size = 4, fontface = "bold")
P_N_BC

# Central Baja Plot: Only Right Y-axis values
P_C_BC <- ggplot(Central_Baja) +
  geom_line(size=1, aes(x = year, y = area_hect), colour = kelp_color) +
  geom_col(aes(x = year, y = MHWs), fill = MHW_color, alpha = 0.5, width = 0.85) +
  xlab("Year") + 
  scale_y_continuous(name = NULL,  # Remove left y-axis title and values
                     limits = c(y_min_MHWs, y_max_MHWs),
                     expand = expansion(mult = c(0.05, 0.05)),
                     sec.axis = sec_axis(~.x/multiplier, name = "Cumulative MHW Intensity (°C days)")) +  # Only show right y-axis title and values
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y.left = element_blank(),  # Hide left y-axis values
        axis.ticks.y.left = element_blank()  # Hide left y-axis ticks
  ) +
  annotate("text", x = min(Northern_Baja$year) + 1, y = y_max_MHWs, 
           label = "Central Baja California", 
           hjust = 0, vjust = 1, size = 4, fontface = "bold")  # Add text annotation
P_C_BC

##EXpoert 12X3.5_ 
