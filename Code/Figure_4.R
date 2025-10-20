##Preliminary- Code for manuscript Marine heatwaves drive range contraction and alternative states
## By Nur Arafeh Dalmau, started mid 2024. Last updated May 2025.
## Creates Figure 4


library(dplyr)
library(FNN)
library(nnet)
library(tidyr)
library(ggplot2)

##Load data
Data <- read.csv ("Range/Dominance_mean_max_sst.csv")
Site <- read_csv("Range/Expedition_22_23_Sites_Tra.csv") 
unique_sites <- Site %>%
  distinct(Site, Sub_site, Latitude, Longitud)

df <- Data %>% group_by(Site, Sub_site) %>%
  summarise(
    Ecklonia = mean(Ecklonia) * 0.5*60,
    Giant_Kelp = mean(Macrocystis) * 5*60,
    Urchins = mean(Urchins) * 0.4*60,
    Gravity_sq = mean(gravity_value),
    SST_mean = mean(sst_mean),
    SST_max = mean(sst_max)
  ) %>% distinct() 

# Join spatial coordinates by Site and Sub_site
df2 <- df %>%
  left_join(unique_sites, by = c("Site", "Sub_site"))

#df2$Gravity_sq <- (df2$Gravity_sq - mean(df2$Gravity_sq, na.rm = TRUE)) / sd(df2$Gravity_sq, na.rm = TRUE)
#df2$SST_max <- (df2$SST_max - mean(df2$SST_max, na.rm = TRUE)) / sd(df2$SST_max, na.rm = TRUE)
#df2$SST_mean <- (df2$SST_mean - mean(df2$SST_mean, na.rm = TRUE)) / sd(df2$SST_mean, na.rm = TRUE)

df2 <- df %>%
  # First, remove rows where all biomasses are 0
  filter(!(Ecklonia == 0 & Giant_Kelp == 0 & Urchins == 0)) %>%
  
  # Then assign the dominant State
  mutate(
    State = case_when(
      Ecklonia >= Giant_Kelp & Ecklonia >= Urchins ~ "Palm Kelp",
      Giant_Kelp >= Ecklonia & Giant_Kelp >= Urchins ~ "Giant Kelp",
      Urchins >= Ecklonia & Urchins >= Giant_Kelp ~ "Urchins"
    )
  )


df2$State <- relevel(as.factor(df2$State), ref = "Giant Kelp")

model_relevel <- nnet::multinom(State ~ Gravity_sq + SST_max, data = df2)
summary(model_relevel)

z <- summary(model_relevel)$coefficients / summary(model_relevel)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
round(p_values, 5)


grid <- expand.grid(
  SST_max = seq(min(df2$SST_max), max(df2$SST_max), length.out = 100),
  Gravity_sq = seq(min(df2$Gravity_sq), max(df2$Gravity_sq), length.out = 100)
)

probs <- predict(model_relevel, newdata = grid, type = "probs")
probs_df <- cbind(grid, probs)

# Reshape for plotting
probs_long <- pivot_longer(probs_df, 
                           cols = c("Palm Kelp", "Giant Kelp", "Urchins"),
                           names_to = "State", values_to = "Probability")

# Convert to factor with desired order
probs_long$State <- factor(probs_long$State, levels = c("Palm Kelp", "Giant Kelp", "Urchins"))


##Plot
ggplot(probs_long, aes(x = SST_max, y = Gravity_sq, fill = Probability)) +
  geom_tile() +
  facet_wrap(~ State) +
  scale_fill_viridis_c(
    guide = guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black"
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "Mean Annual Sea Surface Temperature (°C)",
    y = "Gravity of Human Impact"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, family = "sans"),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.spacing = unit(0.6, "lines"), 
    axis.ticks = element_line(),
    axis.text.x = element_text(size = 12, margin = margin(0, 0, 0, 0), family = "sans"),
    axis.text.y = element_text(size = 12, margin = margin(0, 0, 0, 0), family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans"),
    axis.title.y = element_text(size = 14, family = "sans"),
    legend.title = element_text(size = 13, family = "sans"),
    legend.text = element_text(size = 11, family = "sans")
  )
##Export 9.2X3.5
