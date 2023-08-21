# INIT --------------------------
source("setup.R")

# GWP conversion factors
gwp100 <- read_csv("data/ghg/gwp100.csv", skip = 1) %>% filter(notes == "AR4") %>% select(-notes)



# SOIL -------------------------------
# Read soil stock change params
luc_soil <- read_csv("data/ghg/ghg_luc_soil.csv", skip = 1) %>% select(-source, -units)

# LCM crosswalk
lcm_crosswalk <- tibble(from_to = c("Forestland", "Forestland", "Cropland", "Settlement", "Grassland", "Grassland", "Grassland", "Grassland", "Grassland", "Grassland", "Grassland", "Miscanthus", "SRC", "SRF"),
                        lcm = c("b.w", "c.w", "a.h", "b.t", "i.g", "n.g", "c.g", "a.g", "h.r", "b.g", "f.s", "a.h_energy_misc", "a.h_energy_src", "i.g_energy"))


# Calculate annual flux to/from soil following LUC
gwp_luc_soil <- luc_soil %>%
  # Convert C to CO2
  mutate(gwp_t = soil_c * 44/12,
         ghg = "CO2") %>% 
  select(-soil_c, -n2o) %>% 
  # Join with LCM crosswalk
  left_join(lcm_crosswalk, by = c("from" = "from_to")) %>% 
  rename(lcm_from = lcm) %>% 
  left_join(lcm_crosswalk, by = c("to" = "from_to")) %>% 
  rename(lcm_to = lcm) %>% 
  select(lcm_from, lcm_to, country, gwp_t, time, ghg) %>% 
  # Cross with all years
  crossing(year = 0:80) %>%
  # Calculate annual flux
  group_by(country, lcm_from, lcm_to) %>% 
  mutate(stock = gwp_t * (1 - exp(year / (time / log(0.01)))),
         gwp_t = stock - lag(stock),
         gwp_t = ifelse(is.na(gwp_t), 0, gwp_t)) %>%
  ungroup() %>% 
  select(-stock, -time)

# Add N2O (mineralised due to loss of SOC. = 0 if SOC gain)
gwp_luc_soil <- gwp_luc_soil %>%
  mutate(n2o = ifelse(gwp_t < 0, 0, gwp_t * unique(luc_soil$n2o)),
         ghg = "N2O") %>% 
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = n2o * gwp100) %>% 
  select(-n2o, -gwp100) %>% 
  bind_rows(gwp_luc_soil, .)


# NON-FOREST BIOMASS -------------------------------
# Read biomass stock change params
luc_biomass <- read_csv("data/ghg/ghg_luc_biomass.csv", skip = 1) %>% select(-source, -units, -landuse)

gwp_luc_biomass <- luc_biomass %>% 
  # Convert C to CO2
  mutate(gwp_t = carbon * 44/12,
         ghg = "CO2") %>% 
  select(-carbon) %>% 
  # Cross with self
  left_join(., ., by = "ghg", suffix = c("_from", "_to")) %>% 
  filter(lcm_from != lcm_to) %>%
  filter(!lcm_to %in% c("a.h_energy_misc", "a.h_energy_src") | lcm_from == "a.h") %>% 
  filter(!lcm_to %in% c("i.g_energy") | lcm_from == "i.g") %>% 
  filter(!lcm_from %in% c("a.h_energy_misc", "a.h_energy_src", "i.g_energy")) %>% 
  # Calculate instant change in biomass
  mutate(gwp_t = gwp_t_from - gwp_t_to) %>% 
  select(lcm_from, lcm_to, gwp_t, ghg)



# SAVE ----------------------
ghg_pars_luc <- list(gwp_luc_soil = gwp_luc_soil, 
                     gwp_luc_biomass = gwp_luc_biomass)
save(ghg_pars_luc, file = "rdata/ghg/ghg_pars_luc.RData")