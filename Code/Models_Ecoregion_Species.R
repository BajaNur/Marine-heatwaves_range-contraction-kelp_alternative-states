##Preliminary- Code for manuscript Marine heatwaves drive range contraction and alternative states
## By David Schoeman and Nur Arafeh Dalmau, started in 2024. Last updated May 2025.
## Statistical analyses looking at ecoregion patterns of species in Baja California



# Packages ---------------------------------------------------------------------

  library(tidyverse)
  library(glmmTMB)
  library(sdmTMB)
  library(ggeffects)
  library(emmeans)


# Data --------------------------------------------------------------------

##Giant Kelp
lk_up <- read_csv("Data/For_Models/Ecoregion_lookup.csv") # Made this from old files because Ecoregion was missing in updated files
Giant <- read_csv("Data/For_Models/Giant.csv") %>% 
  add_utm_columns(ll_names = c("Longitud", "Latitude"), units = "km") %>%  # Convert deg to UTM (km)
  mutate(tr_id = paste(Site, Sub_site, Transect, sep = "_"), # Make unique identifiers for transects
         Year = factor(Year)) %>% # Needs to be a factor
  left_join(., lk_up) %>% # Add in Ecoregion, which was missing
  mutate_if(is.character, factor) # All character variables to factor

## What is the difference between northern and central region, in terms of trophic -------------
# cascades and kelp competiton. 

# GLMM, with Giant kelp as an example
# Spatial structure
mesh_giant <- make_mesh(Giant, xy_cols = c("X", "Y"), cutoff = .5) # Assuming that no points are closer together than 0.5 km
plot(mesh_giant) # Looks OK

Giant_m <- sdmTMB(
  Sum_abundance ~ Ecoregion * Year + 
    (1|Site), # We can add sub_site, but it does not improve AIC
  data = Giant,
  family = nbinom1(link = "log"), 
 mesh = mesh_giant,
  spatial = "on"
)
summary(Giant_m)
sanity(Giant_m) # Looks OK
tidy(Giant_m, conf.int = TRUE)

# Do we need such a complex model?
Giant_m2 <- update(Giant_m, ~. -Ecoregion:Year) # Model without the interaction
Giant_m3 <- update(Giant_m2, ~. -Year) # Model with only Ecoregion
Giant_m4 <- update(Giant_m2, ~. -Ecoregion) # Model with only Year
Giant_m5 <- update(Giant_m3, ~. -Ecoregion) # Model with only intercept (i.e., the null model)

AIC(Giant_m, Giant_m2, Giant_m3, Giant_m4, Giant_m5) %>% # Which is best
  arrange(AIC) # The most complex one

# Inspect residuals  
r <- simulate(Giant_m, nsim = 500, type = "mle-mvn")
dharma_residuals(r, Giant_m, return_DHARMa = TRUE) %>% 
  plot()
# Not bad

# Pairwise tests    
emmeans(Giant_m, pairwise ~ Ecoregion|Year, type = "response") # For each year, there are more in central than north
emmeans(Giant_m, pairwise ~ Year | Ecoregion, type = "response") # For north there is a decline from 2022 to 2023, but not in central

# Plots
pdat <- ggpredict(Giant_m, terms = list(Ecoregion = unique(Giant$Ecoregion),
                                        Year = unique(Giant$Year))) %>% 
  data.frame()
ggplot() +
  geom_errorbar(data = pdat,
                aes(x = group, ymin = conf.low, ymax = conf.high, col = group),
                width = 0.05,
                alpha = .25) +
  geom_point(data = pdat,
             aes(x = group, y = predicted, col = group)) +
  labs(x = "Ecoregion",
       y = "Count",
       colour = "Year") +
  theme_bw() +
  facet_grid(vars(x), scales = "free")


###Palm Kelp

Palm <- read_csv("Data/For_Models/Palm.csv") %>% 
    add_utm_columns(ll_names = c("Longitud", "Latitude"), units = "km") %>%  # Convert deg to UTM (km)
    mutate(tr_id = paste(Site, Sub_site, Transect, sep = "_"), # Make unique identifiers for transects
           Year = factor(Year)) %>% # Needs to be a factor
    left_join(., lk_up) %>% # Add in Ecoregion, which was missing
    mutate_if(is.character, factor) # All character variables to factor

# Spatial structure
mesh_palm <- make_mesh(Palm, xy_cols = c("X", "Y"), cutoff = .5) # Assuming that no points are closer together than 0.5 km
plot(mesh_palm) # Looks OK

## What is the difference between northern and central region, in terms of trophic -------------
# cascades and kelp competiton. 

# Model
  
    Palm_m <- sdmTMB(
      Sum_abundance ~ Ecoregion * Year + 
        (1|Site), # Transect nested within Site ***convergence fails with more complex random effects
      data = Palm,
      family = nbinom1(link = "log"), 
      mesh = mesh_palm,
      spatial = "on"
      )
    summary(Palm_m)
    sanity(Palm_m) # Looks OK
    tidy(Palm_m, conf.int = TRUE)
    
    # Do we need such a complex model?
      Palm_m2 <- update(Palm_m, ~. -Ecoregion:Year) # Model without the interaction
      Palm_m3 <- update(Palm_m2, ~. -Year) # Model with only Ecoregion
      Palm_m4 <- update(Palm_m2, ~. -Ecoregion) # Model with only Year
      Palm_m5 <- update(Palm_m3, ~. -Ecoregion) # Model with only intercept (i.e., the null model)
      
      AIC(Palm_m, Palm_m2, Palm_m3, Palm_m4, Palm_m5) %>% # Which is best
        arrange(AIC) # We can remove year
    
    # Inspect residuals  
      r_P <- simulate(Palm_m3, nsim = 500, type = "mle-mvn")
      residuals_P <-  dharma_residuals(r_P, Palm_m3, return_DHARMa = TRUE) 
          plot(residuals_P)
        # Not terrible
        
        # Save plot to PNG
        png(filename = "Figures/Residuals_model_Palm.png", 
            width = 8, 
            height = 4.5,
            units = "in", 
            res = 600)
        
        plot(residuals_P)  # Plot happens inside the png device
        
        dev.off()
        
  # Pairwise tests    
    emmeans(Palm_m, pairwise ~ Ecoregion | Year, type = "response") # For each year, there are more in central than north
    emmeans(Palm_m, pairwise ~ Year | Ecoregion, type = "response") # For north there is a decline from 2022 to 2023, but not in central

    # Plots
    pdat <- ggpredict(Palm_m, terms = list(Ecoregion = unique(Palm$Ecoregion),
                                             Year = unique(Palm$Year))) %>% 
      data.frame()
    ggplot() +
      geom_errorbar(data = pdat,
                    aes(x = group, ymin = conf.low, ymax = conf.high, col = group),
                    width = 0.05,
                    alpha = .25) +
      geom_point(data = pdat,
                 aes(x = group, y = predicted, col = group)) +
      labs(x = "Ecoregion",
           y = "Count",
           colour = "Year") +
      theme_bw() +
      facet_grid(vars(x), scales = "free")
    
    

###Lobster 
    Lobster <- read_csv("Data/For_Models/Lobster.csv") %>% 
      add_utm_columns(ll_names = c("Longitud", "Latitude"), units = "km") %>%  # Convert deg to UTM (km)
      mutate(tr_id = paste(Site, Sub_site, Transect, sep = "_"), # Make unique identifiers for transects
             Year = factor(Year)) %>% # Needs to be a factor
      left_join(., lk_up) %>% # Add in Ecoregion, which was missing
      mutate_if(is.character, factor) # All character variables to factor
    
    ## What is the difference between northern and central region, in terms of trophic -------------
    # Spatial structure
mesh_lobster <- make_mesh(Lobster, xy_cols = c("X", "Y"), cutoff = .5) # Assuming that no points are closer together than 0.5 km
plot(mesh_lobster) # Looks OK
        # cascades and kelp competiton. 
    
###Model
    
    Lobster_m <- sdmTMB(
      Abundance_new ~ Ecoregion * Year + 
        (1|Site), # Transect nested within Site ***convergence fails with more complex random effects
      data = Lobster,
      family = nbinom1(link = "log"), 
      mesh = mesh_lobster,
      spatial = "on"
    )
    summary(Lobster_m)
    sanity(Lobster_m) # Looks OK
    tidy(Lobster_m, conf.int = TRUE)
    
    # Do we need such a complex model?
    Lobster_m2 <- update(Lobster_m, ~. -Ecoregion:Year) # Model without the interaction
    Lobster_m3 <- update(Lobster_m2, ~. -Year) # Model with only Ecoregion
    Lobster_m4 <- update(Lobster_m2, ~. -Ecoregion) # Model with only Year
    Lobster_m5 <- update(Lobster_m3, ~. -Ecoregion) # Model with only intercept (i.e., the null model)
    
    AIC(Lobster_m, Lobster_m2, Lobster_m3, Lobster_m4, Lobster_m5) %>% # Which is best
      arrange(AIC) # The most simple one is the null model. But we need to at least keep ecoreigon.. So model 3
    
    # Inspect residuals  
    r_L <- simulate(Lobster_m3, nsim = 500, type = "mle-mvn")
    residuals_L <- dharma_residuals(r_L, Lobster_m3, return_DHARMa = TRUE) 
      plot(residuals_L)
    # Not bad
    # Save plot to PNG
    png(filename = "Figures/Residuals_model_Lobster.png", 
        width = 8, 
        height = 4.5,
        units = "in", 
        res = 600)
    
    plot(residuals_L)  # Plot happens inside the png device
    
    dev.off()
    
    # Pairwise tests    
    emmeans(Lobster_m3, pairwise ~ Ecoregion, type = "response") # For each year, there are more in central than north

    # Plots
    pdat <- ggpredict(Lobster_m3, terms = "Ecoregion") %>% 
      data.frame()
    ggplot() +
      geom_errorbar(data = pdat,
                    aes(x = group, ymin = conf.low, ymax = conf.high, col = group),
                    width = 0.05,
                    alpha = .25) +
      geom_point(data = pdat,
                 aes(x = group, y = predicted, col = group)) +
      labs(x = "Ecoregion",
           y = "Count") +
      theme_bw() +
      facet_grid(vars(x), scales = "free")
    
 
    
###Sheephead Kelp

    
    Sheephead <- read_csv("Data/For_Models/Sheephead.csv") %>% 
      add_utm_columns(ll_names = c("Longitud", "Latitude"), units = "km") %>%  # Convert deg to UTM (km)
      mutate(tr_id = paste(Site, Sub_site, Transect, sep = "_"), # Make unique identifiers for transects
             Year = factor(Year)) %>% # Needs to be a factor
      left_join(., lk_up) %>% # Add in Ecoregion, which was missing
      mutate_if(is.character, factor) # All character variables to factor
    
    ## What is the difference between northern and central region, in terms of trophic -------------
    # cascades and kelp competiton. 
        # Spatial structure
    mesh_she <- make_mesh(Sheephead, xy_cols = c("X", "Y"), cutoff = .5) # Assuming that no points are closer together than 0.5 km
    plot(mesh_she) # Looks OK
    
 ######Model
    
    She_m <- sdmTMB(
      Density_Biomass ~ Ecoregion * Year + 
        (1|Site), # Transect nested within Site ***convergence fails with more complex random effects
      data = Sheephead,
      family = nbinom1(link = "log"), 
      mesh = mesh_she,
      spatial = "on"
    )
    summary(She_m)
    sanity(She_m) # Issues with the mesh, remove?
    tidy(She_m, conf.int = TRUE)
    
    #Remove mesh
    She_m2 <- sdmTMB(
      Density_Biomass ~ Ecoregion * Year + 
        (1|Site), # Transect nested within Site ***convergence fails with more complex random effects
      data = Sheephead,
      family = nbinom1(link = "log"), 
      mesh = mesh_she,
      spatial = "off"
    )
    summary(She_m2)
    sanity(She_m2) #No issues now
    tidy(She_m2, conf.int = TRUE)
    
    AIC(She_m, She_m2) #AIC better with mesh. So lets keep the mesh
    
    # Do we need such a complex model?
    She_m2 <- update(She_m, ~. -Ecoregion:Year) # Model without the interaction
    She_m3 <- update(She_m2, ~. -Year) # Model with only Ecoregion
    She_m4 <- update(She_m2, ~. -Ecoregion) # Model with only Year
    She_m5 <- update(She_m3, ~. -Ecoregion) # Model with only intercept (i.e., the null model)
    
    AIC(She_m, She_m2, She_m3, She_m4, She_m5) %>% # Which is best
      arrange(AIC) # Keep everything
    
    # Inspect residuals  
    r_S <- simulate(She_m, nsim = 500, type = "mle-mvn")
    residuals_S <- dharma_residuals(r_S, She_m, return_DHARMa = TRUE)
      plot(residuals_S)
    # Not terrible
      
      png(filename = "Figures/Residuals_model_Sheephead.png", 
          width = 8, 
          height = 4.5,
          units = "in", 
          res = 600)
      
      plot(residuals_S)  # Plot happens inside the png device
      
      dev.off()
      
    # Pairwise tests    
    emmeans(She_m, pairwise ~ Ecoregion | Year, type = "response") # For each year, there are more in central than north
    emmeans(She_m, pairwise ~ Year | Ecoregion, type = "response") # For north there is a decline from 2022 to 2023, but not in central
    
    # Plots
    pdat <- ggpredict(She_m, terms = "Ecoregion") %>% 
      data.frame()
    ggplot() +
      geom_errorbar(data = pdat,
                    aes(x = group, ymin = conf.low, ymax = conf.high, col = group),
                    width = 0.05,
                    alpha = .25) +
      geom_point(data = pdat,
                 aes(x = group, y = predicted, col = group)) +
      labs(x = "Ecoregion",
           y = "Count") +
      theme_bw() +
      facet_grid(vars(x), scales = "free")
    

###Urchins
    
    Urchins <- read_csv("Data/For_Models/Urchins.csv") %>% 
      add_utm_columns(ll_names = c("Longitud", "Latitude"), units = "km") %>%  # Convert deg to UTM (km)
      mutate(tr_id = paste(Site, Sub_site, Transect, sep = "_"), # Make unique identifiers for transects
             Year = factor(Year)) %>% # Needs to be a factor
      left_join(., lk_up) %>% # Add in Ecoregion, which was missing
      mutate_if(is.character, factor) # All character variables to factor
    
    ## What is the difference between northern and central region, in terms of trophic -------------
    # cascades and kelp competiton. 
   # Spatial structure
    mesh_urc <- make_mesh(Urchins, xy_cols = c("X", "Y"), cutoff = .5) # Assuming that no points are closer together than 0.5 km
    plot(mesh_urc) # Looks OK 
    
    ##### Model
  
    Urc_m <- sdmTMB(
      Sum_abundance ~ Ecoregion * Year + 
        (1|Site), # Transect nested within Site ***convergence fails with more complex random effects
      data = Urchins,
      family = nbinom1(link = "log"), 
      mesh = mesh_urc,
      spatial = "on"
    )
    summary(Urc_m)
    sanity(Urc_m) # Issues with the mesh, remove?
    tidy(Urc_m, conf.int = TRUE)
    
    ##Tweedie or binomial?
    Urc_m2 <- sdmTMB(
      Sum_abundance ~ Ecoregion * Year + 
        (1|Site), # Transect nested within Site ***convergence fails with more complex random effects
      data = Urchins,
      family = tweedie(link = "log"), 
      mesh = mesh_urc,
      spatial = "on"
    )
    summary(Urc_m2)
    sanity(Urc_m2) # Issues with the mesh, remove?
    tidy(Urc_m2, conf.int = TRUE)
    
    AIC(Urc_m, Urc_m2)# Tweedie best AIC and best model fit.

    # Inspect residuals  
    r_U <- simulate(Urc_m, nsim = 500, type = "mle-mvn")
    residuals_U <- dharma_residuals(r_U, Urc_m, return_DHARMa = TRUE) 
      plot(residuals_U) # not great
    
    #Inspect residuals  
    r_U2 <- simulate(Urc_m2, nsim = 500, type = "mle-mvn")
    residuals_U2 <- dharma_residuals(r_U2, Urc_m2, return_DHARMa = TRUE) 
      plot(residuals_U2) # better
      
      png(filename = "Figures/Residuals_model_Urchins.png", 
          width = 8, 
          height = 4.5,
          units = "in", 
          res = 600)
      
      plot(residuals_U2)  # Plot happens inside the png device
      
      dev.off()
      
    
    # Do we need such a complex model?
    Urc_m3 <- update(Urc_m2, ~. -Ecoregion:Year) # Model without the interaction
    Urc_m4 <- update(Urc_m3, ~. -Year) # Model with only Ecoregion
    Urc_m5 <- update(Urc_m3, ~. -Ecoregion) # Model with only Year
    Urc_m6 <- update(Urc_m4, ~. -Ecoregion) # Model with only intercept (i.e., the null model)
    
    AIC(Urc_m2, Urc_m3, Urc_m4, Urc_m5, Urc_m6) %>% # Which is best
      arrange(AIC) # Keep everything
    
    
    
    # Pairwise tests    
    emmeans(Urc_m2, pairwise ~ Ecoregion | Year, type = "response") # For each year, there are more in central than north
    emmeans(Urc_m2, pairwise ~ Year | Ecoregion, type = "response") # For north there is a decline from 2022 to 2023, but not in central
    
    # Plots
    pdat <- ggpredict(Urc_m2, terms = list(Ecoregion = unique(Urchins$Ecoregion),
                                           Year = unique(Urchins$Year))) %>% 
      data.frame()
    ggplot() +
      geom_errorbar(data = pdat,
                    aes(x = group, ymin = conf.low, ymax = conf.high, col = group),
                    width = 0.05,
                    alpha = .25) +
      geom_point(data = pdat,
                 aes(x = group, y = predicted, col = group)) +
      labs(x = "Ecoregion",
           y = "Count",
           colour = "Year") +
      theme_bw() +
      facet_grid(vars(x), scales = "free")
    