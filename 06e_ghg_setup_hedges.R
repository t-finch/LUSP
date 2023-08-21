# INIT --------------------------
source("setup.R")

# GWP conversion factors
gwp100 <- read_csv("data/ghg/gwp100.csv", skip = 1) %>% filter(notes == "AR4") %>% select(-notes)



# GET EMISSIONS FACTORs ----------------------
ghg_hedges <- read_csv("data/ghg/ghg_hedge.csv", skip = 1) %>% select(-notes)


gwp_hedges <- ghg_hedges %>% 
  # Convert to annual CO2 flux
  mutate(ghg = "CO2") %>% 
  mutate(gwp_t = -c_density/t * 44/12) %>%
  # Cross with max number of years (2020-2100)
  crossing(age = seq(0, 80, by = 1)) %>% 
  mutate(gwp_t = ifelse(age < t, gwp_t, 0)) %>%
  # Tidy
  select(age, ghg, gwp_t)
  
# BASELINE HEAD LENGTH -----------------
# Per country, km
hedge_length <- read_csv("data/hedges/cs_hedge_length.csv", skip = 1) %>% select(-notes)

# SAVE ----------------------
ghg_pars_hedges <- list(gwp_hedges = gwp_hedges, hedge_length = hedge_length)
save(ghg_pars_hedges, file = "rdata/ghg/ghg_pars_hedges.RData")
