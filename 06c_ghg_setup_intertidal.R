# INIT --------------------------
source("setup.R")


# GET EMISSIONS FACTORs ----------------------
# Load intertidal habitat params
ghg_intertidal <- read_csv("data/ghg/ghg_intertidal.csv", skip = 1)

# Get GWP for existing and new intertidal
gwp_intertidal <- bind_rows(ghg_intertidal %>% 
                              mutate(category = "intertidal_existing",
                                     gwp = saltmarsh_flux) %>% 
                              select(category, gwp, ghg) %>% 
                              unique(),
                            ghg_intertidal %>% 
                              mutate(category = "intertidal_new",
                                     gwp = saltmarsh_flux * saltmarsh_frac + mudflat_flux * mudflat_frac) %>% 
                              select(category, gwp, ghg) %>% 
                              unique())


# GET AREA OF EXISTING SALTMARSH ----------------------
# Baseline area (2015) 
intertidal_existing_ha <- ghg_intertidal %>% 
  mutate(annual_loss = annual_loss * baseline_ha / sum(baseline_ha)) %>% 
  crossing(year = seq(2015, 2100, by = 1)) %>% 
  mutate(intertidal_existing_ha = baseline_ha - annual_loss * (year - 2015),
         intertidal_existing_ha = ifelse(intertidal_existing_ha < 0, 0, intertidal_existing_ha)) %>% # declining according to annual loss rate 
  select(year, country, intertidal_existing_ha)


# SAVE ----------------------
ghg_pars_intertidal <- list(gwp_intertidal = gwp_intertidal, 
                            intertidal_existing_ha = intertidal_existing_ha)

save(ghg_pars_intertidal, file = "rdata/ghg/ghg_pars_intertidal.RData")


