# INIT --------------------------
source("setup.R")

# GWP conversion factors
gwp100 <- read_csv("data/ghg/gwp100.csv", skip = 1) %>% filter(notes == "AR4") %>% select(-notes)



## PEATLAND GHG FLUX --------------------------
# GHG fluxes per condition category
ghg_peat <- read_csv("data/ghg/peat/ghg_peat.csv", skip = 1) %>% select(-notes)

# Convert to GWP
gwp_peat <- ghg_peat %>%
  left_join(gwp100, by = "ghg") %>%
  # Sum up for each gas 
  group_by(ghg, condition_category) %>% 
  summarise(gwp_t = sum(ghg_t * gwp100)) %>%
  ungroup() %>% 
  mutate(usage = "per hectare of each peat condition category") 


## PEATLAND CONDITION AREAS --------------------------
# Get baseline area of each peat condition category
peat_condition_areas <- read_csv("data/ghg/peat/ghg_peat_areas.csv", skip = 1) %>% 
  select(condition_category, country, area_2013) %>% 
  rename('area' = area_2013)

# Get LCM / peat category cross-walk
peat_condition_codes <- read_csv("data/ghg/peat/ghg_peat_condition_lcm.csv", skip = 1) %>% select(-flux_category, -lcm_layer) %>% unique() 

# Get GWP of each LCM over peat
gwp_peat <- peat_condition_codes %>%
  inner_join(peat_condition_areas, by = 'condition_category') %>% 
  # Get baseline proportional area of each condition class within each LCM
  group_by(lcm, country) %>% 
  mutate(prop = area/sum(area)) %>% 
  ungroup() %>% 
  select(-area) %>%
  mutate(condition = case_when(condition_category %in% c("Drained eroded modified bog", "Undrained eroded modified bog") ~ "Modified",
                               condition_category %in% c("Extracted domestic (fuel peat)", "Extracted industrial (horticultural)") ~ "Extraction",
                               TRUE ~ "Existing")) %>%
  # Add rows for new rewetted bog/fen
  bind_rows(tibble(lcm = c("f.s", "b.g"),
                   condition_category = c("Rewetted fen", "Rewetted bog"),
                   condition = c("New (rewetted)", "New (rewetted)"),
                   prop = 1) %>% 
              crossing(country = c("England", "Scotland", "Wales", "Northern Ireland"))) %>% 
  # Add rows for paludiculture and sustainably managed cropland
  bind_rows(tibble(lcm = c("a.h_palud", "i.g_palud", "a.h"),
                   condition_category = c("Paludiculture", "Paludiculture", "Cropland (sustainable management)"),
                   condition = c("Paludiculture", "Paludiculture", "Cropland (sustainable management)"),
                   prop = 1) %>% 
              crossing(country = c("England", "Scotland", "Wales", "Northern Ireland"))) %>% 
  # Calcuate area-weighted GWP of each condition class
  left_join(gwp_peat, by = 'condition_category') %>%
  group_by(lcm, ghg, country, condition) %>% 
  summarise(gwp_t = sum(prop/sum(prop) * gwp_t),
            prop = sum(prop)) %>% 
  filter(prop != 0) %>% # Removes Welsh extraction sites, for which area = 0
  ungroup() %>% 
  # Tidy
  mutate(category = "Peatland") %>% 
  select(category, country, gwp_t, ghg, condition, prop, lcm)


# SAVE ----------------------
ghg_pars_peat <- list(gwp_peat = gwp_peat)
save(ghg_pars_peat, file = "rdata/ghg/ghg_pars_peat.RData")
