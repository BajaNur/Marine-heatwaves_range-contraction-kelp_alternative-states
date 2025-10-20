##Preliminary- Code for manuscript Marine heatwaves drive range contraction and alternative states
## By David Schoeman and Nur Arafeh Dalmau, started in 2024. Last updated May 2025.
## Statistical analyses looking at the preconditions for the three regions in Baja California

library(tidyverse)
library(glmmTMB)
library(sdmTMB)
library(ggeffects)
library(emmeans)
library(DHARMa)


## Code to check if long term datasets provide insights in whether preconditions for the three
## alternative states where there. We only look at Ensenada (north) and Nativiad region
# because the southern region we just have the means and not the raw data.

# Data --------------------------------------------------------------------

#############Ensenada region (north). Urchins dominating.

# Data from 2011-2013, 2016, 2019-2023. At six sites across Ensenada region (Around 50kms the furthest two sites)
# Decided to work on a simpler model without mesh, given the few sites we have, the non-consistency in yeras surveyd, and the proximity of sites. 

### --------------Urchins --------------------------
Urchins_Ensenada <- read_csv("Data/For_Models/Urchins_Enseada_Model.csv") %>%
  mutate(
    Year = as.integer(Year)   # Needs to be a factor
  )  %>%  # Add in Ecoregion, which was missing
  mutate(
    across(where(is.character), factor),  # Convert all character columns to factors
    Category = case_when(  # Add MHW status category
      as.numeric(as.character(Year)) <= 2013 ~ "Before",
      as.numeric(as.character(Year)) >= 2019 ~ "After",
      TRUE ~ NA_character_  # Add this to prevent errors for intermediate years
    ),
    Category = factor(Category, levels = c("Before", "After"))
  )

Urchins_Ensenada <- Urchins_Ensenada %>%
  filter(as.numeric(as.character(Year)) != 2016) ## Exclude because only one year "DURING"

## Just checking the means as a general reference, so we check the estimtaed means are not too weird.
Urchins <- Urchins_Ensenada %>% group_by(Site, Year, Category) %>% 
  summarise (m = mean(Density)) %>% 
  group_by(Category) %>%
  summarise (m2 = mean(m))

# GLMM
## We have three years before (2011-2013) and five after (2019-2023). We can't include
# year with temporal autocorrelation because of the non-consecutive sample. 
# we include year as a random effect to capture year variability.

Urchins_E_glm <- glmmTMB::glmmTMB(
  Density ~ Category + (1|Year) + (1|Site),
  family = glmmTMB::tweedie(link = "log"),
  data = Urchins_Ensenada
)


summary (Urchins_E_glm)

# Without year?
Urchins_E_glm2 <- glmmTMB::glmmTMB(
  Density ~ Category + (1|Site),
  family = glmmTMB::tweedie(link = "log"),
  data = Urchins_Ensenada
)

summary (Urchins_E_glm2)

AIC(Urchins_E_glm, Urchins_E_glm2)# keep all

car::Anova(Urchins_E_glm, type = "III")

simulationOutput_urchins_E <- simulateResiduals(Urchins_E_glm, plot = F) 
plot(simulationOutput_urchins_E) ## Good

emmeans(Urchins_E_glm, pairwise ~ Category, type = "response")#Urchins increased after MHWs

### --------------Palm Kelp --------------------------

Palm_Ensenada <- read_csv("Data/For_Models/Ecklonia_Enseada_Model.csv") %>%
  mutate(Year = as.factor(Year)   # Needs to be a factor
  )  %>%  # Add in Ecoregion, which was missing
  mutate(
    across(where(is.character), factor),  # Convert all character columns to factors
    Category = case_when(  # Add MHW status category
      as.numeric(as.character(Year)) <= 2013 ~ "Before",
      as.numeric(as.character(Year)) >= 2019 ~ "After",
      TRUE ~ NA_character_  # Add this to prevent errors for intermediate years
    ),
    Category = factor(Category, levels = c("Before", "After"))
  )

Palm_Ensenada <- Palm_Ensenada %>%
  filter(as.numeric(as.character(Year)) != 2016)

# Checking raw means.
Palm <- Palm_Ensenada %>% group_by(Site, Year, Category) %>% 
  summarise (m = mean(density_eck)) %>% 
  group_by(Category) %>%
  summarise (m2 = mean(m))


Palm_E_glm <- glmmTMB::glmmTMB(
  density_eck ~ Category + (1|Year) + (1|Site),
  family = glmmTMB::tweedie(link = "log"),
  data = Palm_Ensenada
)

summary (Palm_E_glm)

#Do we need Year?
Palm_E_glm2 <- glmmTMB::glmmTMB(
  density_eck ~ Category + (1|Site),
  family = glmmTMB::tweedie(link = "log"),
  data = Palm_Ensenada
)

summary (Palm_E_glm2)

AIC(Palm_E_glm, Palm_E_glm2)#Keep all

car::Anova(Palm_E_glm, type = "III")

simulationOutput_Palm_E <- simulateResiduals(Palm_E_glm, plot = F) 
plot(simulationOutput_Palm_E) ## Good

emmeans(Palm_E_glm, pairwise ~ Category, type = "response")# No difference before-after

#################Natividad region (center region). Giant Kelp dominating.
##This data is different. We have 5 sites located in the same island (Island is a few kms length)
##Data continous from 2006-2023. Same data used for Caro's FE paper.

### --------------Urchins --------------------------

Urchins_Natividad <- read_csv("Data/For_Models/Urchins_Natividad_Model.csv") %>%
  mutate(
    ANIO = as.factor(ANIO)   # Needs to be a factor
  )  %>%  # Add in Ecoregion, which was missing
  mutate(Density = (Sum_Abundance/60),
    across(where(is.character), factor),  # Convert all character columns to factors
    Category = case_when(  # Add MHW status category
      as.numeric(as.character(ANIO)) <= 2013 ~ "Before",
      as.numeric(as.character(ANIO)) %in% 2014:2017 ~ "During",
      as.numeric(as.character(ANIO)) >= 2016 ~ "After",
      TRUE ~ NA_character_  # Add this to prevent errors for intermediate years
    ),
    Category = factor(Category, levels = c("Before", "During", "After"))
  )

# For sanity, check raw means
Urchins2 <- Urchins_Natividad %>% group_by(SITIO, ANIO, Category) %>% 
  summarise (m = mean(Density)) %>% 
  group_by(Category) %>%
  summarise (m2 = mean(m))

### Here we can include temporal autocorrelation.
Urchins_N_glm <- glmmTMB::glmmTMB(
  Density ~ Category + (1|SITIO) + ar1(0 + ANIO | SITIO),
  family = glmmTMB::tweedie(link = "log"),
  data = Urchins_Natividad
)

summary (Urchins_N_glm)

## Do we need to keep Site level variation as a random effect?

Urchins_N_glm2 <- glmmTMB::glmmTMB(
  Density ~ Category + ar1(0 + ANIO | SITIO),
  family = glmmTMB::tweedie(link = "log"),
  data = Urchins_Natividad
)

summary (Urchins_N_glm2)

AIC(Urchins_N_glm, Urchins_N_glm2) ## Lower AIC with Site as a random effect

car::Anova(Urchins_N_glm, type = "III")

simulationOutput_urchins <- simulateResiduals(Urchins_N_glm, plot = F) 
plot(simulationOutput_urchins) ## Good

emmeans(Urchins_N_glm, pairwise ~ Category, type = "response")## Urchins did not signicantly increase after MHWs. Urchin densities were very low


### --------------Palm kelp --------------------------

Palm_Natividad <- read_csv("Data/For_Models/Ecklonia_Natividad_Model.csv") %>%
  mutate(Density = (ABUNDANCIA/60),
    ANIO = as.factor(ANIO)   # Needs to be a factor
  )  %>%  # Add in Ecoregion, which was missing
  mutate(
    across(where(is.character), factor),  # Convert all character columns to factors
    Category = case_when(  # Add MHW status category
      as.numeric(as.character(ANIO)) <= 2013 ~ "Before",
      as.numeric(as.character(ANIO)) %in% 2014:2017 ~ "During",
      as.numeric(as.character(ANIO)) >= 2016 ~ "After",
      TRUE ~ NA_character_  # Add this to prevent errors for intermediate years
    ),
    Category = factor(Category, levels = c("Before", "During", "After"))
  )


##Check raw means for sanity.

Palm2 <- Palm_Natividad %>% group_by(SITIO, ANIO, Category) %>% 
  summarise (m = mean(Density)) %>% 
  group_by(Category) %>%
  summarise (m2 = mean(m))

## GLM
Palm_N_glm <- glmmTMB::glmmTMB(
  Density ~ Category + (1|SITIO) + ar1(0 + ANIO | SITIO),
  family = glmmTMB::tweedie(link = "log"),
  data = Palm_Natividad
)

summary (Palm_N_glm)

Palm_N_glm2 <- glmmTMB::glmmTMB(
  Density ~ Category + ar1(0 + ANIO | SITIO),
  family = glmmTMB::tweedie(link = "log"),
  data = Palm_Natividad
)

summary (Palm_N_glm2)


AIC(Palm_N_glm, Palm_N_glm2) ## Better without Site as a random effect, but we keep

car::Anova(Palm_N_glm, type = "III")

emmeans(Palm_N_glm, pairwise ~ Category, type = "response")

simulationOutput_palm_N <- simulateResiduals(Palm_N_glm2, plot = F) 
plot(simulationOutput_palm_N) ## Decent

