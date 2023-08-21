# INIT --------------------------
source("setup.R")

# GWP conversion factors
gwp100 <- read_csv("data/ghg/gwp100.csv", skip = 1) %>% filter(notes == "AR4") %>% select(-notes)

# GET EMISSIONS FACTORS ----------------------
# GHG parameters for Miscanthus & SRC
ghg_bioenergy <- read_csv("data/ghg/ghg_bioenergy_new.csv", skip = 1) %>% select(-notes) 

# Synthetic N EFs
synthn_efs <- read_csv("data/ghg/ag/synthn_efs.csv", skip = 1) %>% select(-source) 

# Do soil C
soil <- ghg_bioenergy %>% 
  filter(param == "soil_c") %>% 
  select(-rotation, -units, -param) %>% 
  crossing(year = 0:80) %>% 
  mutate(gwp_t = ghg_t * (1 - exp(year / (t / log(0.01)))),
         ghg = "CO2",
         category_1 = "Bioenergy crops",
         category_2 = "Bioenergy crops - soil") %>% 
  group_by(bioenergy_crop) %>% 
  mutate(gwp_t = gwp_t - lag(gwp_t, default = 0)) %>% 
  ungroup() %>% 
  select(-ghg_t, -t)

# Do N2O (assume from immobilization? not sure if this needs including)
n2o <- ghg_bioenergy %>%
  filter(param == "n2o") %>% 
  select(-rotation, -units, -param) %>% 
  crossing(year = 0:80) %>% 
  mutate(gwp_t = ghg_t * (1 - exp(year / (t / log(0.01)))),
         ghg = "N2O",
         category_1 = "Bioenergy crops",
         category_2 = "Bioenergy crops - n2o") %>% 
  group_by(bioenergy_crop) %>% 
  mutate(gwp_t = gwp_t - lag(gwp_t, default = 0)) %>% 
  ungroup() %>% 
  select(-ghg_t, -t)

# Do biomass
biomass <- ghg_bioenergy %>% 
  filter(param %in% c("agb_c", "bgb_c")) %>% 
  group_by(bioenergy_crop, t) %>% 
  summarise(ghg_t = sum(ghg_t), .groups = "drop") %>%
  crossing(year = 0:80) %>% 
  mutate(gwp_t = ifelse(year >= t, ghg_t, ghg_t * (year / t)),
         ghg = "CO2",
         category_1 = "Bioenergy crops",
         category_2 = "Bioenergy crops - biomass") %>% 
  group_by(bioenergy_crop) %>% 
  mutate(gwp_t = gwp_t - lag(gwp_t, default = 0),
         gwp_t = -gwp_t / (12/44)) %>% 
  ungroup() %>% 
  select(-ghg_t, -t)

# Do fertiliser
fert <- ghg_bioenergy %>% 
  filter(param == "kg_n") %>%
  rename(kg_n = ghg_t) %>% 
  select(-units, -param) %>% 
  crossing(year = 0:80) %>%
  group_by(bioenergy_crop) %>% 
  mutate(kg_n = kg_n * rep(c(rep(0, times = unique(t)), rep(1, times = unique(rotation-t))), times = 10)[1:81]) %>%  
  # Join with EFs for N fert
  crossing(synthn_efs %>%
             left_join(gwp100, by = "ghg") %>% 
             group_by(ghg) %>% 
             summarise(gwp_fert = sum(ghg_t * gwp100 * frac)) %>% 
             ungroup()) %>% 
  mutate(gwp_t = kg_n * gwp_fert,
         category_1 = "Bioenergy crops",
         category_2 = "Bioenergy crops - fertiliser") %>% 
  select(-kg_n, -gwp_fert, -t, -rotation)

# Combine
gwp_bioenergy <- bind_rows(soil, 
                           biomass,
                           # n2o,
                           fert) %>% 
  rename(age = year)


# SAVE ----------------------
ghg_pars_bioenergy <- list(gwp_bioenergy = gwp_bioenergy)
save(ghg_pars_bioenergy, file = "rdata/ghg/ghg_pars_bioenergy.RData")