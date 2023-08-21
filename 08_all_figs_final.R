## INIT -----------------------
source("setup.R")
load("rdata/scenarios/scenarios_areas_2022.RData")
load("rdata/ghg/gwp_combined_2023.RData")
load("rdata/bird_results_2023.RData")
load("rdata/food/food_results_basic_2022.RData")
load("rdata/food/food_gapclosed_simple.RData")


## UPDATE SCENARIO NAMES --------------
name_labels <- tibble(name = c("BAU", "BNZP", "Widespread Engagement", "Widespread Engagement (waders)", "Widespread Engagement (soils)", "NbS", "NbS-bioenergy", "NbS-bioenergy (waders)", "NbS-bioenergy (soils)", "NbS+organic", "NbS+HNVf", "OnFarmSharing", "OnFarmBNZP", "networks_a", "networks_b", "networks_c"),
                      label = c("0: Baseline", "3: Balanced Pathway", "4: Widespread Engagement", "4w: Widespread Engagement (waders)", "4s: Widespread Engagement (soil)", "6: NBS with biomass crops", "5: NBS", "5w: NBS (waders)", "5s: NBS (soil)", "7: NBS with organic farming", "8: NBS extra", "1: On Farm Measures (Organic)", "2: On Farm Measures (Balanced)", "25YP_random", "25YP_agriculture", "25YP_people"),
                      scenario = c("0", "1", "2", "2w", "2s", "3", "4", "4w", "4s", "5", "8", "6", "7", "25YP_random", "25YP_agriculure", "25YP_people"))


## LAND USE AREAS ------------------------------
# Sum across secondary categories
scenarios_change_areas <- scenarios_change_areas[, keyby = .(year, lcm_from, lcm_to, country, scenario, name),
                                                 .(area = sum(area))]
scenarios_areas <- scenarios_areas[, keyby = .(year, lcm, country, scenario, name),
                                   .(area = sum(area))]

# Reformat data for alluvial plot
alluvial_data <- scenarios_change_areas %>%
  # Changing land
  filter(lcm_from != lcm_to) %>% 
  group_by(scenario, name, lcm_from, lcm_to, country) %>% 
  summarise(area = sum(area), .groups = "drop") %>% 
  # Add unchanged land
  bind_rows(left_join(scenarios_areas %>% 
                        filter(year == 2015) %>% 
                        group_by(scenario, name, lcm, country) %>% 
                        summarise(area = sum(area), .groups = "drop"),
                      scenarios_change_areas %>% 
                        filter(lcm_from != lcm_to) %>% 
                        group_by(scenario, name, lcm_from, country) %>% 
                        summarise(area = sum(area), .groups = "drop"),
                      by = c('scenario', 'name', 'lcm' = 'lcm_from', 'country')) %>%
              mutate(area.y = ifelse(is.na(area.y), 0, area.y),
                     area = area.x - area.y,
                     lcm_from = lcm, lcm_to = lcm) %>% 
              select(scenario, name, lcm_from, lcm_to, area, country)) %>%
  # Recode levels
  mutate(lcm_from = fct_recode(lcm_from, "SNG" = "a.g", "AH" = "a.h", "AH" = "a.h_organic", "AH" = "a.h_palud",
                               "B/F" = "b.g", "Built" = "b.t", 
                               "BW" = "b.w", "SNG" = "c.g", 
                               "CW" = "c.w", "CW" = "c.w_pinewood", "B/F" = "f.s", 
                               "SNG" = "h.r", "IG" = "i.g", "IG" = "i.g_organic", "IG" = "i.g_palud",
                               "SNG" = "n.g", "SM" = "s.m",
                               "SNG" = "a.g_new", "SNG" = "c.g_new", "SNG" = "n.g_new",
                               "AF" = "a.h_silvoa", "AF" = "i.g_silvop", "AF" = "a.h_organic_silvoa", "AF" = "i.g_organic_silvop",
                               "WP" = "a.g_woodpa", "WP" = "c.g_woodpa", "WP" = "n.g_woodpa", "WP" = "h.r_woodpa",
                               "BioE" = "a.h_energy", "BioE" = "i.g_energy"),
         lcm_from = fct_relevel(lcm_from, "Built", "AH", "IG", "SNG", 
                                "B/F", "SM", "AF", "BW", "CW")) %>% 
  mutate(lcm_to = fct_recode(lcm_to, "SNG" = "a.g", "AH" = "a.h", "AH" = "a.h_organic", "AH" = "a.h_palud",
                             "B/F" = "b.g", "Built" = "b.t", 
                             "BW" = "b.w", "SNG" = "c.g", 
                             "CW" = "c.w", "CW" = "c.w_pinewood", "B/F" = "f.s", 
                             "SNG" = "h.r", "IG" = "i.g", "IG" = "i.g_organic", "IG" = "i.g_palud",
                             "SNG" = "n.g", "SM" = "s.m",
                             "SNG" = "a.g_new", "SNG" = "c.g_new", "SNG" = "n.g_new",
                             "AF" = "a.h_silvoa", "AF" = "i.g_silvop", "AF" = "a.h_organic_silvoa", "AF" = "i.g_organic_silvop",
                             "WP" = "a.g_woodpa", "WP" = "c.g_woodpa", "WP" = "n.g_woodpa", "WP" = "h.r_woodpa",
                             "BioE" = "a.h_energy", "BioE" = "i.g_energy"),
         lcm_to = fct_relevel(lcm_to, "Built", "AH", "IG", "BioE", "SNG", 
                              "B/F",  "SM", "AF", "WP", " BW", "CW")) %>% 
  # Sum across new levels
  group_by(name, scenario, lcm_from, lcm_to, country) %>% 
  summarise(area = sum(area), .groups = "drop") %>% 
  # Remove coastal/freshwater/rock/saltmarsh
  filter(!lcm_from %in% c("c.l", "f.w", "r.k", "SM")) %>%
  filter(!lcm_to %in% c("c.l", "f.w", "r.k", "SM")) %>% 
  left_join(name_labels, by = c("name", "scenario"))


# Create function
alluvial_plot_fun <- function(data, country){
  # If UK, sum across 4 countries
  if(country == "UK"){
    data <- data %>% 
      group_by(scenario, name, label, lcm_from, lcm_to) %>% 
      summarise(area = sum(area), .groups = "drop")
  }
  
  # If one country, filter
  if(country != "UK"){
    data <- data %>% 
      filter(country == !!country)
  }
  
  # Tidy data
  data <- data %>%
    group_by(scenario, name, label, lcm_from, lcm_to) %>% 
    summarise(area = sum(area) / 1000, .groups = "drop") %>% 
    group_by(scenario, name) %>% 
    filter(area > 0.001 * sum(area)) %>% 
    ungroup() 
  
  # Plot
  ggplot(data, aes(y = area, axis1 = lcm_from, axis2 = lcm_to)) +
    geom_alluvium(knot.pos = 0.1, width = 0.1, aes(fill = lcm_from), alpha = 0.65) +
    geom_stratum(width = 0.15, fill = "grey90", color = "grey70") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
    scale_x_continuous(breaks = c(1, 2), labels = c("2015", "2050"), expand = c(0, 0)) +
    scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
    scale_fill_viridis_d(option = "B", direction = 1,
                         labels = c("Built", "AH = Arable & hort.", "IG = Improved grassland", "SNG = Semi-natural grassland", 
                                    "B/F = Bog or fen", "BW = Broadleaved woodland", "CW = Coniferous woodland")) +
    geom_vline(aes(xintercept = 2), alpha = 0) +
    labs(x = NULL, y = "", fill = NULL) +
    facet_wrap(~label, scales = "free") +  
    theme(panel.spacing.y = unit(0.5, "lines"),
          panel.spacing.x = unit(0.5, "lines"),
          legend.position = "top",
          axis.text.x = element_text(angle = 25, hjust = 1))
  
}

legend_df <- tibble(label = c(c("Built", "AH = Arable & hort.", "IG = Improved grassland", "SNG = Semi-natural grassland", 
                                   "B/F = Bog or fen", "BW = Broadleaved woodland", "CW = Coniferous woodland"),
                                 c("AF = Agroforestry", "BioE = Bioenergy crops", "WP = Wood pasture")),
                       fill = c(c(viridis(n = 7, option = "B", direction = 1)),
                                c("white", "white", "white"))) %>% 
  mutate(label = fct_relevel(label, "Built", "AH = Arable & hort.", "IG = Improved grassland", "SNG = Semi-natural grassland", 
                            "B/F = Bog or fen", "BW = Broadleaved woodland", "CW = Coniferous woodland", 
                            "AF = Agroforestry", "BioE = Bioenergy crops", "WP = Wood pasture"))
  

g_legend <- ggplot(legend_df) + 
  geom_col(aes(x = label, y = 1, fill = label)) + 
  scale_fill_manual(values = legend.table$fill, name = NULL, guide = guide_legend(nrow = 3)) + 
  coord_flip()  + 
  theme(legend.position = "top",
        legend.key = element_rect(color = NA),
        plot.margin = unit(c(3,3,3,3), "lines")) 

# Plot - UK
g1 <- alluvial_plot_fun(alluvial_data %>% filter(nchar(scenario) == 1), "UK")

plot_grid(get_legend(g_legend),
          g1 + guides(fill = F),
          ncol = 1,
          rel_heights = c(1, 8))
ggsave("figs/all_figs/final_manuscript/fig2_final.tiff", width = 6.5, height = 8)



## GHGs ----------------------------------
# Fig 3 plot
ghg_plot_fun <- function(data, country = "UK"){
  if(country != "UK"){
    data <- data %>% filter(country == !!country)
  }
  
  # Tidy input data
  data <- data %>%
    filter(ag_sector == 1 | is.na(ag_sector)) %>% 
    filter(option == "None") %>% 
    mutate(label = fct_rev(label)) 
  
  
  # Plot total GHGs
  g1 <- data %>%
    group_by(name, scenario, label, year) %>% 
    summarise(gwp_t = sum(gwp_t) / 1e6, .groups = "drop") %>%
    mutate(gwp_t_baseline = gwp_t[year == 2015 & name == "BAU"]) %>%
    ungroup() %>%
    filter(year == 2050) %>% 
    ggplot(aes(x = label, col = label)) +
    geom_segment(aes(xend = label, y = gwp_t, yend = gwp_t_baseline), linewidth = 4) +
    geom_hline(aes(yintercept = gwp_t_baseline), size = 0.25) +
    geom_text(aes(y = gwp_t, label = paste0(ifelse(gwp_t >= gwp_t_baseline, "+", ""), round((gwp_t-gwp_t_baseline)/gwp_t_baseline, 3) * 100, "%"), hjust = ifelse(gwp_t >= gwp_t_baseline, -0.1, 1.1)), size = 2, col = 1) +
    geom_text(aes(y = gwp_t_baseline, x = -Inf, label = "2015"), angle = -90, col = 1, hjust = 1.1, vjust = -0.5, size = 2) +
    scale_color_viridis_d(begin = 0.1, end = 0.9, guide = FALSE, direction = -1) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x = NULL, y = expression(paste("Annual net GHG emissions in 2050 (Mt C", O[2], "e)"))) +
    theme(panel.spacing.x = unit(1, "lines"),
          legend.position = "top",
          legend.justification = "left") +
    coord_flip() + 
    facet_wrap(~"Total GHGs")
  
  # Facet by sub-sector
  g2 <- data %>%
    mutate(category = case_when(category_1 == "Agriculture" ~ "Agriculture",
                                category_1 == "Peatland" ~ "Peatlands",
                                TRUE ~ "Woodlands (etc.)")) %>% 
    group_by(name, scenario, label, year, category) %>% 
    summarise(gwp_t = sum(gwp_t) / 1e6, .groups = "drop") %>%
    group_by(category) %>% 
    mutate(gwp_t_baseline = gwp_t[year == 2015 & name == "BAU"]) %>%
    ungroup() %>%
    filter(year == 2050) %>% 
    ggplot(aes(x = label, col = label)) +
    geom_segment(aes(xend = label, y = gwp_t, yend = gwp_t_baseline), linewidth = 4) +
    geom_hline(aes(yintercept = gwp_t_baseline), size = 0.25) +
    geom_text(aes(y = gwp_t, label = paste0(ifelse(gwp_t >= gwp_t_baseline, "+", ""), round(ifelse(gwp_t_baseline < 0, -1, 1) * (gwp_t-gwp_t_baseline)/gwp_t_baseline, 3) * 100, "%"), hjust = ifelse(gwp_t >= gwp_t_baseline, -0.1, 1.1)), size = 2, col = 1) +
    geom_text(aes(y = gwp_t_baseline, x = -Inf, label = "2015"), angle = -90, col = 1, hjust = 1.1, vjust = -0.5, size = 2) +
    scale_color_viridis_d(begin = 0.1, end = 0.9, guide = FALSE, direction = -1) +
    scale_y_continuous(expand = expansion(mult = c(0.38, 0.3))) +
    labs(x = NULL, y = expression(paste("Annual net GHG emissions in 2050 (Mt C", O[2], "e)"))) +
    theme(panel.spacing.x = unit(1, "lines"),
          legend.position = "top",
          legend.justification = "left") +
    coord_flip() +
    facet_wrap(~category, nrow = 1, scales = "free_x")
  
  # Facet by GHG
  g3 <- data %>%
    mutate(ghg = ifelse(is.na(ghg), "CO2", ghg)) %>% 
    group_by(name, scenario, label, year, ghg) %>% 
    summarise(gwp_t = sum(gwp_t) / 1e6, .groups = "drop") %>%
    group_by(ghg) %>% 
    mutate(gwp_t_baseline = gwp_t[year == 2015 & name == "BAU"]) %>%
    ungroup() %>%
    filter(year == 2050) %>%
    mutate(ghg = case_when(ghg == "CO2" ~ "CO[2]",
                           ghg == "N2O" ~ "paste(N[2], O)",
                           ghg == "CH4" ~ "CH[4]")) %>% 
    ggplot(aes(x = label, col = label)) +
    geom_segment(aes(xend = label, y = gwp_t, yend = gwp_t_baseline), linewidth = 4) +
    geom_hline(aes(yintercept = gwp_t_baseline), size = 0.25) +
    geom_text(aes(y = gwp_t, label = paste0(ifelse(gwp_t >= gwp_t_baseline, "+", ""), round(ifelse(gwp_t_baseline < 0, -1, 1) * (gwp_t-gwp_t_baseline)/gwp_t_baseline, 3) * 100, "%"), hjust = ifelse(gwp_t >= gwp_t_baseline, -0.1, 1.1)), size = 2, col = 1) +
    geom_text(aes(y = gwp_t_baseline, x = -Inf, label = "2015"), angle = -90, col = 1, hjust = 1.1, vjust = -0.5, size = 2) +
    scale_color_viridis_d(begin = 0.1, end = 0.9, guide = FALSE, direction = -1) +
    scale_y_continuous(expand = expansion(mult = c(0.55, 0.58))) +
    labs(x = NULL, y = expression(paste("Annual net GHG emissions in 2050 (Mt C", O[2], "e)"))) +
    theme(panel.spacing.x = unit(1, "lines"),
          legend.position = "top",
          legend.justification = "left") +
    coord_flip() +
    facet_wrap(~ghg, nrow = 1, scales = "free_x", labeller = label_parsed)
  
  plot_grid(g1, g2, g3, ncol = 1, labels = LETTERS[1:3])
  
}

# Plot UK
gwp_combined$gwp_combined %>%
  left_join(name_labels, by = c("name", "scenario")) %>% 
  filter(nchar(scenario) == 1) %>% 
  ghg_plot_fun()
ggsave("figs/all_figs/final_manuscript/fig3_final.tiff", width = 6, height = 6.5)



## BIRDS ---------------------
# Fig 4 plot
bird_plot_fun <- function(data, country){
  data <- data %>% 
    filter(country == !!country) %>%
    filter(!group %in% c("Farmland waders", "Farmland birds", "Woodland birds", "RSPB Priority Species", "BoCC Red/Amber (old)")) %>%
    mutate(group = paste0(group, "\n(n = ", n, ")")) %>% 
    mutate(label = fct_rev(label)) %>% 
    group_by(group, scenario, name, label) %>% 
    summarise(min = min(pop_rel) - 1,
              max = max(pop_rel) - 1,
              val = exp(mean(log(pop_rel))) - 1,
              .groups = "drop")
  
  g1 <- data %>% 
    filter(grepl("All species", group)) %>% 
    ggplot(aes(x = label, y = val, col = label)) + 
    geom_segment(aes(xend = label, y = val, yend = 0), linewidth = 3.5) +
    geom_hline(aes(yintercept = 0), size = 0.25) +
    geom_segment(aes(xend = label, y = min, yend = max), linewidth = 0.5, position = position_nudge(0.35, 0)) +
    geom_text(aes(y = val, label = paste0(ifelse(val >= 0, "+", ""), round(val, 3) * 100, "%"), hjust = ifelse(val >= 0, -0.1, 1.1)), size = 2, col = 1) +
    geom_text(aes(y = 0, x = -Inf, label = "2015"), angle = -90, col = 1, hjust = 1.1, vjust = -0.5, size = 2) +
    scale_color_viridis_d(begin = 0.1, end = 0.9, guide = FALSE, direction = -1) +
    labs(x = NULL, y = "Geometric mean change in breeding bird habitat index") +
    theme(panel.spacing.x = unit(1, "lines")) +
    facet_wrap(~group, ncol = 2) +
    coord_flip() + 
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0.1, 0.1)))
  
  g2 <- data %>% 
    filter(grepl("BoCC", group) | grepl("specialists", group)) %>% 
    ggplot(aes(x = label, y = val, col = label)) + 
    geom_segment(aes(xend = label, y = val, yend = 0), linewidth = 3.5) +
    geom_hline(aes(yintercept = 0), size = 0.25) +
    geom_segment(aes(xend = label, y = min, yend = max), linewidth = 0.5, position = position_nudge(0.35, 0)) +
    geom_text(aes(y = val, label = paste0(ifelse(val >= 0, "+", ""), round(val, 3) * 100, "%"), hjust = ifelse(val >= 0, -0.1, 1.1)), size = 2, col = 1) +
    geom_text(aes(y = 0, x = -Inf, label = "2015"), angle = -90, col = 1, hjust = 1.1, vjust = -0.5, size = 2) +
    scale_color_viridis_d(begin = 0.1, end = 0.9, guide = FALSE, direction = -1) +
    labs(x = NULL, y = "Geometric mean change in breeding bird habitat index") +
    theme(panel.spacing.x = unit(1, "lines")) +
    facet_wrap(~group, ncol = 3, scales = "free_x") +
    coord_flip() + 
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0.5, 0.5)))
  
  plot_grid(g1, g2, ncol = 1, labels = LETTERS[1:2])
  
}

pop_sum %>% 
  left_join(name_labels, by = "scenario") %>% 
  filter(nchar(scenario) == 1 & nchar(method) == 1 & species_subset == "Restricted" & !yield_growth) %>% 
  bird_plot_fun(country = "UK")
ggsave("figs/all_figs/final_manuscript/fig4_final.tiff", width = 6, height = 5)



## FOOD --------------------------
# Fig 5 plot
food_plot_fun <- function(data, country){
  if(country != "UK") data <- data %>% filter(country == !!country) 
  
  data_1 <- data %>%
    group_by(name, scenario, year) %>%
    summarise(energy = sum(energy)/1e6,
              protein = sum(protein)/1e6, 
              .groups = "drop") %>%
    mutate(energy_baseline = energy[year == 2015 & scenario == "0"],
           protein_baseline = protein[year == 2015 & scenario == "0"]) %>% 
    filter(year == 2050) %>% 
    left_join(name_labels, by = "name") %>% 
    mutate(label = fct_rev(label)) 
  
  data_2 <- data %>% 
    mutate(product = case_when(grepl("Beef", product) | grepl("Milk", product) | grepl("Sheep", product) ~ "Animal-based products",
                               product %in% c("Eggs", "Pig", "Poultry") ~ "Animal-based products",
                               TRUE ~ "Plant-based products")) %>% 
    group_by(name, scenario, year, product) %>%
    summarise(energy = sum(energy)/1e6,
              protein = sum(protein)/1e6, 
              .groups = "drop") %>%
    group_by(product) %>% 
    mutate(energy_baseline = energy[year == 2015 & scenario == "0"],
           protein_baseline = protein[year == 2015 & scenario == "0"]) %>%
    ungroup() %>% 
    filter(year == 2050) %>% 
    left_join(name_labels, by = "name") %>% 
    mutate(label = fct_rev(label)) 
  
  
  g1 <- data_1 %>% 
    ggplot(aes(x = label, col = label)) +
    geom_segment(aes(xend = label, y = energy, yend = energy_baseline), linewidth = 4) +
    geom_hline(aes(yintercept = energy_baseline), size = 0.25) +
    geom_text(aes(y = energy, label = paste0(ifelse(energy >= energy_baseline, "+", ""), round((energy-energy_baseline)/energy_baseline, 3) * 100, "%"), hjust = ifelse(energy >= energy_baseline, -0.1, 1.1)), size = 2, col = 1) +
    geom_text(aes(y = energy_baseline, x = -Inf, label = "2015"), angle = -90, col = 1, hjust = 1.1, vjust = -0.5, size = 2) +
    scale_color_viridis_d(begin = 0.1, end = 0.9, guide = FALSE, direction = -1) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.05))) +
    labs(x = NULL, y = "Food production in 2050 (trillion kcal)") +
    theme(panel.spacing.x = unit(1, "lines"),
          legend.position = "top",
          legend.justification = "left") +
    coord_flip() + 
    facet_wrap(~"All food products")
  
  g2 <- data_2 %>% 
    ggplot(aes(x = label, col = label)) +
    geom_segment(aes(xend = label, y = energy, yend = energy_baseline), linewidth = 4) +
    geom_hline(aes(yintercept = energy_baseline), size = 0.25) +
    geom_text(aes(y = energy, label = paste0(ifelse(energy >= energy_baseline, "+", ""), round((energy-energy_baseline)/energy_baseline, 3) * 100, "%"), hjust = ifelse(energy >= energy_baseline, -0.1, 1.1)), size = 2, col = 1) +
    geom_text(aes(y = energy_baseline, x = -Inf, label = "2015"), angle = -90, col = 1, hjust = 1.1, vjust = -0.5, size = 2) +
    scale_color_viridis_d(begin = 0.1, end = 0.9, guide = FALSE, direction = -1) +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.1))) +
    labs(x = NULL, y = "Food production in 2050 (trillion kcal)") +
    theme(panel.spacing.x = unit(1, "lines"),
          legend.position = "top",
          legend.justification = "left") +
    coord_flip() + 
    facet_wrap(~product, scales = "free_x")
  
  plot_grid(g1, g2, ncol = 1, labels = LETTERS[1:2])
}

# Plot UK
food_results_basic %>% 
  mutate(food_type_sum = purrr::map(food, ~.[[1]])) %>%
  select(-food) %>%
  unnest(cols = c(data, food_type_sum)) %>% 
  filter(nchar(scenario) == 1) %>% 
  food_plot_fun("UK")
ggsave("figs/all_figs/final_manuscript/fig5_final.tiff", width = 6, height = 5)



## HWPs -----------------------
# Fig 6 plot
hwp_plot_fun <- function(data, country){
  if(country != "UK"){
    data <- data %>% filter(country == !!country)
  }
  
  data <-  data %>% 
    mutate(label = fct_rev(label)) 
  
  data_1 <- data %>%
    group_by(scenario, name, label) %>% 
    summarise(hwp_baseline = sum(hwp[year >= 2015 & year < 2020]) / 5/1e6,
              hwp = sum(hwp[year >= 2045 & year < 2055]) / 10/1e6, 
              .groups = "drop") 
  
  data_2 <- data %>%
    mutate(hwp_type = ifelse(grepl("fuel", hwp_type), "Fuel", "Timber")) %>% 
    group_by(scenario, name, label, hwp_type) %>% 
    summarise(hwp_baseline = sum(hwp[year >= 2015 & year < 2020]) / 5/1e6,
              hwp = sum(hwp[year >= 2045 & year < 2055]) / 10/1e6, 
              .groups = "drop") 
  
  data_3 <- data %>%
    mutate(hwp_type = ifelse(grepl("fuel", hwp_type), "Fuel", "Timber")) %>% 
    group_by(scenario, name, label, hwp_type) %>% 
    summarise(hwp_baseline = sum(hwp[year >= 2015 & year < 2020]) / 5/1e6,
              hwp = sum(hwp[year >= 2091 & year < 2100]) / 10/1e6, 
              .groups = "drop") 
  
  
  g1 <- data_1 %>% 
    ggplot(aes(x = label, col = label)) +
    geom_segment(aes(xend = label, y = hwp, yend = hwp_baseline), linewidth = 4) +
    geom_hline(aes(yintercept = hwp_baseline), size = 0.25) +
    geom_text(aes(y = hwp, label = paste0(ifelse(hwp >= hwp_baseline, "+", ""), round((hwp-hwp_baseline)/hwp_baseline, 3) * 100, "%"), hjust = ifelse(hwp >= hwp_baseline, -0.1, 1.1)), size = 2, col = 1) +
    geom_text(aes(y = hwp_baseline, x = -Inf, label = "2015"), angle = -90, col = 1, hjust = 1.1, vjust = -0.5, size = 2) +
    scale_color_viridis_d(begin = 0.1, end = 0.9, guide = FALSE, direction = -1) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.15))) +
    labs(x = NULL, y = "Mean annual fuel and timber production 2045-2054 (Mt)") +
    theme(panel.spacing.x = unit(1, "lines"),
          legend.position = "top",
          legend.justification = "left") +
    coord_flip() + 
    facet_wrap(~"All forest products")
  
  g2 <- data_2 %>% 
    ggplot(aes(x = label, col = label)) +
    geom_segment(aes(xend = label, y = hwp, yend = hwp_baseline), linewidth = 4) +
    geom_hline(aes(yintercept = hwp_baseline), size = 0.25) +
    geom_text(aes(y = hwp, label = paste0(ifelse(hwp >= hwp_baseline, "+", ""), round((hwp-hwp_baseline)/hwp_baseline, 3) * 100, "%"), hjust = ifelse(hwp >= hwp_baseline, -0.1, 1.1)), size = 2, col = 1) +
    geom_text(aes(y = hwp_baseline, x = -Inf, label = "2015"), angle = -90, col = 1, hjust = 1.1, vjust = -0.5, size = 2) +
    scale_color_viridis_d(begin = 0.1, end = 0.9, guide = FALSE, direction = -1) +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.25))) +
    labs(x = NULL, y = "Mean annual fuel or timber production 2045-2054 (Mt)") +
    theme(panel.spacing.x = unit(1, "lines"),
          legend.position = "top",
          legend.justification = "left") +
    coord_flip() + 
    facet_wrap(~hwp_type, scales = "free_x")
  
  g3 <- data_3 %>% 
    ggplot(aes(x = label, col = label)) +
    geom_segment(aes(xend = label, y = hwp, yend = hwp_baseline), linewidth = 4) +
    geom_hline(aes(yintercept = hwp_baseline), size = 0.25) +
    geom_text(aes(y = hwp, label = paste0(ifelse(hwp >= hwp_baseline, "+", ""), round((hwp-hwp_baseline)/hwp_baseline, 3) * 100, "%"), hjust = ifelse(hwp >= hwp_baseline, -0.1, 1.1)), size = 2, col = 1) +
    geom_text(aes(y = hwp_baseline, x = -Inf, label = "2015"), angle = -90, col = 1, hjust = 1.1, vjust = -0.5, size = 2) +
    scale_color_viridis_d(begin = 0.1, end = 0.9, guide = FALSE, direction = -1) +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.25))) +
    labs(x = NULL, y = "Mean annual fuel or timber production 2091-2100 (Mt)") +
    theme(panel.spacing.x = unit(1, "lines"),
          legend.position = "top",
          legend.justification = "left") +
    coord_flip() + 
    facet_wrap(~hwp_type, scales = "free_x")
  
  plot_grid(g1, g2, g3, ncol = 1, labels = LETTERS[1:3])
  
}

# Plot UK
gwp_combined$hwp_combined %>% 
  left_join(name_labels, by = c("name", "scenario")) %>% 
  filter(nchar(scenario) == 1) %>% 
  hwp_plot_fun("UK")
ggsave("figs/all_figs/final_manuscript/fig6_final.tiff", width = 6, height = 6.5)


## SUMMARY PLOT -----------
# Fig 7
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

bind_rows(gwp_combined$gwp_combined %>%
            left_join(name_labels, by = c("name", "scenario")) %>% 
            filter(nchar(scenario) == 1) %>% 
            filter(ag_sector == 1 | is.na(ag_sector)) %>% 
            filter(option == "None") %>% 
            group_by(name, scenario, label, year) %>% 
            summarise(val = sum(gwp_t) / 1e6, 
                      .groups = "drop") %>%
            filter(year == 2050) %>% 
            select(-year) %>% 
            mutate(axis = "GHG",
                   val = val/max(val),
                   val = -val+1),
          food_results_basic %>% 
            mutate(food_type_sum = purrr::map(food, ~.[[1]])) %>%
            select(-food) %>%
            unnest(cols = c(data, food_type_sum)) %>% 
            filter(nchar(scenario) == 1) %>% 
            group_by(name, scenario, year) %>%
            summarise(val = sum(energy)/1e6,
                      .groups = "drop") %>%
            filter(year == 2050)  %>% 
            select(-year) %>% 
            left_join(name_labels, by = c("scenario", "name")) %>% 
            mutate(axis = "Food",
                   val = val/max(val)),
          pop_sum %>% 
            filter(country == "UK") %>%
            left_join(name_labels, by = "scenario") %>% 
            filter(nchar(scenario) == 1 & nchar(method) == 1 & species_subset == "Restricted") %>% 
            filter(group == "All species") %>%
            mutate(group = paste0(group, "\n(n = ", n, ")")) %>% 
            group_by(group, scenario, name, label) %>% 
            summarise(val = exp(mean(log(pop_rel))) - 1,
                      .groups = "drop") %>% 
            mutate(axis = "Birds",
                   val = val/max(val)),
          gwp_combined$hwp_combined %>% 
            left_join(name_labels, by = c("name", "scenario")) %>% 
            filter(nchar(scenario) == 1) %>% 
            group_by(scenario, name, label) %>% 
            summarise(val = sum(hwp[year >= 2045 & year < 2055]) / 10/1e6,
                      val2 = sum(hwp[year >= 2091 & year < 2100]) / 10/1e6,
                      .groups = "drop") %>% 
            mutate(axis = "HWP",
                   val = val / max(val),
                   val2 = val2 / max(val2))) %>%
  mutate(val2 = ifelse(is.na(val2), val, val2)) %>% 
  ggplot(aes(x = axis, y = val, col = label, group = label)) +
  geom_polygon(data = crossing(tibble(axis = c("GHG", "Food", "Birds", "HWP"), val = c(1, 1, 1, 1)), label = filter(name_labels, nchar(scenario) == 1)$label),
               fill = 1, col = NA, alpha = 0.1) +
  geom_polygon(fill = NA, linewidth = 0.5) +
  geom_polygon(aes(y = val2), fill = NA, linewidth = 0.5, linetype = 2) +
  coord_radar() + 
  facet_wrap(~label) +
  theme(panel.grid.major.x = element_line(color ="gray70", size = 0.2),
        panel.grid.major.y = element_line(color ="gray70", size = 0.2)) +
  scale_color_viridis_d(guide = F, end = 0.8, option = "A") +
  labs(x = NULL, y = "Relative value")
ggsave("figs/all_figs/final_manuscript/fig7_final.tiff", width = 6, height = 6)