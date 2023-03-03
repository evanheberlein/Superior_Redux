library(tidyverse)
library(janitor)
library(ggplot2)

superior_tidy <- read_csv("SuperiorR.csv") %>% 
  clean_names() %>% 
  mutate(species_type = na_if(species_type, "NA")) %>% 
  mutate(species = na_if(species, "NA")) 

superior_figure <- superior_tidy %>% 
  select(-elevation_cm_above_msl) %>% 
  drop_na() %>% 
  complete(species_type, year, plot_id) %>% 
  mutate(species_type = 
           replace(species_type, species_type == "emergent", "Emergent")) %>% 
  mutate(species_type = 
           replace(species_type, species_type == "submerged", "Submerged")) %>%
  mutate(species_type = 
           replace(species_type, species_type == "floating", "Floating")) %>% 
  mutate(plot_id = replace(plot_id, plot_id == 1, "Berm")) %>% 
  mutate(plot_id = replace(plot_id, plot_id == 2, "Shallow")) %>% 
  mutate(plot_id = replace(plot_id, plot_id == 3, "Deep")) %>% 
  mutate(plot_id = replace(plot_id, plot_id == 4, "Edge")) %>%  
  mutate(species_type = species_type %>% 
           factor() %>% 
           fct_relevel("Emergent","Submerged")) %>% 
  mutate(plot_id = plot_id %>% 
           factor() %>% 
           fct_relevel("Berm", "Shallow", "Deep", "Edge"))

superior_summary <- superior_figure %>% 
  group_by(plot_id, year, species_type) %>% 
  summarize(
    mean_pct = mean(percent_cover),
    sd_pct = sd(percent_cover),
    sample_size = n(),
    se_pct = sd(percent_cover)/sqrt(n()),
    var_pct = var(percent_cover)
  )

mean_pct <- data_frame(superior_summary$mean_pct)
sd_pct <- data_frame(superior_summary$sd_pct)

veg_figure <- ggplot() +
  geom_col(data = superior_figure,
           aes(x = plot_id,
               y = percent_cover,
               fill = fct_relevel(species_type)),
           na.rm = TRUE,
           position = position_dodge(.8)) +
  facet_grid(~year) +
  scale_fill_manual(values=c("#a1dab4", "#41b6c4", "#253494")) + #ColorBrewer
  labs(x = "Plot type",
       y = "% cover",
       fill = "Vegetation type") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_y_continuous(expand = c(0,0)) #+
  #geom_errorbar(data = superior_summary, aes(x = year, ymin = mean_pct - sd_pct, ymax = mean_pct + sd_pct))

veg_figure

# Figure - try jitter/beeswarm (widens like violin - could be overwhelming)
# Box plots in a square (make axis >100)
# Relevant paper: https://joss.theoj.org/papers/10.21105/joss.00037.pdf

veg_figure <- ggplot() +
  geom_boxplot(data = superior_figure,
           aes(x = plot_id,
               y = percent_cover,
               fill = fct_relevel(species_type)),
           na.rm = TRUE) +
  facet_wrap(~year,
             nrow = 2) +
  scale_fill_manual(values=c("#a1dab4", "#41b6c4", "#253494")) + #ColorBrewer
  labs(x = "Plot type",
       y = "% cover",
       fill = "Vegetation type") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,110))

veg_figure

# Facet by plot id to show from year to year better, then can free y for easier viz? - switch year and plot_id in code
# Put vertical gridlines (light gray) b/w each column