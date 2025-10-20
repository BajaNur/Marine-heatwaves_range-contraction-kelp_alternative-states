##Preliminary- Code for manuscript Marine heatwaves drive range contraction and alternative states
## Creates Figure 2b and model kelp dynamics by ecoregion using GAMMs

###Range Contraction paper. 
# Written by Dave S, based on work by Nur AD
  # Finalized May 2025
    # Analyses for landsat data. Was kelp impacted by the MHWs, did they recover, and is there a difference in the response by ecoregions?

###Data is processed for kelp from Landsat.

# Packages ----------------------------------------------------------------

  library(emmeans)
  library(DHARMa)
  library(sdmTMB)
  library(tidyverse)
  library(ggeffects)


# Replicating Nur's analysis ----------------------------------------------

  # Data
    kelp_mhw <- read_csv("Data/Landsat_Processed/kelp_data_per_year.csv") %>%
      mutate(PixelID = factor(PixelID),
             area = median_area,
             region = factor(region),
             region = fct_relevel(region, "Northern Baja California", "Central Baja California"),
             period = fct_relevel(period, "Before", "During", "After"),
             year_fac = factor(year)) %>%
      add_utm_columns(ll_names = c("long", "lat"), units = "km") # Convert deg to UTM (km)
  
  # Spatial structure
    mesh <- make_mesh(kelp_mhw, xy_cols = c("X", "Y"), cutoff = 1) # No points are closer together than 1 km, so this should do it
    plot(mesh) # Looks OK

  # GAMM
    # Fit    
      st_mod <- sdmTMB(
        area ~ s(year, by = region) +  (1|PixelID), 
        data = kelp_mhw, mesh = mesh,
        family = tweedie(link = "log"), 
        spatial = "on") # Fails, when including AR1 by year
      sanity(st_mod) # Looks OK
      # write_rds(st_mod, "GAMM_approach.rds")
      
      # Plot
      # st_mod <- read_rds("GAMM_approach.rds")
      pdat <- ggpredict(st_mod, terms = list(year = seq(min(kelp_mhw$year), max(kelp_mhw$year), length.out = 250),
                                             region = unique(kelp_mhw$region))) %>% 
        data.frame()
      
      
     
  ggplot() +
  geom_ribbon(data = pdat, aes(x = x, ymin = conf.low, ymax = conf.high, 
                               fill = group), alpha = .25) +
        geom_line(data = pdat, aes(x = x, y = predicted, colour = group)) +
        labs(x = "Year", y = "Kelp area (ha)", colour = "Region", fill = "Region") +
        theme_bw() + facet_grid(vars(group), scales = "free")

  r_Landsat <- simulate(st_mod, nsim = 500, type = "mle-mvn")
  residuals_Landsat <- dharma_residuals(r_Landsat, st_mod, return_DHARMa = TRUE) %>% 
        plot()

  png(filename = "Figures/Residuals_model_Landsat.png", 
      width = 8, 
      height = 4.5,
      units = "in", 
      res = 600)
  
  plot(residuals_Landsat)  # Plot happens inside the png device
  
  dev.off()
      
  # Plot
      ggplot() +
        # Marine heatwave shading (2014–2016)
        geom_rect(aes(xmin = 2013.5, xmax = 2016.5, ymin = -Inf, ymax = Inf),
                  fill = "#F5B7B1", alpha = 0.5) +
        # Confidence ribbons
        geom_ribbon(
          data = pdat,
          aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
          alpha = 0.2
        ) +
        # Prediction lines
        geom_line(
          data = pdat,
          aes(x = x, y = predicted, color = group),
          size = 0.45
        ) +
        labs(
          x = "Year",
          y = "Model response – Kelp area (m²)",
          color = "Region",
          fill = "Region"
        ) +
        scale_color_manual(values = c("Northern" = "#1B9E77", "Central" = "#4682B4")) +
        scale_fill_manual(values = c("Northern" = "#1B9E77", "Central" = "#4682B4")) +
        theme_classic() +
        theme(
          legend.position = c(0.95, 0.95),
          legend.justification = c(1, 1),
          legend.background = element_rect(fill = "white", color = NA),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          text = element_text(size = 12)
        )
  
  
      #Check size of how I did this plot for 1a.
      #6.2X 3
  
