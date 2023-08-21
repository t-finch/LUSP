# INIT --------------------------
source("setup.R")

# GWP conversion factors
gwp100 <- read_csv("data/ghg/gwp100.csv", skip = 1) %>% filter(notes == "AR4") %>% select(-notes)



## BASELINE FOOD PROPERTIES --------------------------
# Get food results for 2015
load("rdata/food/food_results_basic_2022.RData")
food_baseline <- food_results_basic$food[[1]]
rm(food_results_basic)

# Add rows for deer, goats and horses (as sheep) - for allocating to each country
food_baseline$feed_nutrient_consumption <- food_baseline$feed_nutrient_consumption %>% 
  filter(animal == "Sheep") %>%
  select(-animal) %>% 
  crossing(animal = c("Deer", "Goats", "Horses")) %>% 
  group_by(animal) %>% 
  mutate_at(vars(me, ge, cp), ~ . / sum(.)) %>% 
  ungroup() %>% 
  bind_rows(food_baseline$feed_nutrient_consumption, .)

# Combine Eggs & Poultry
food_baseline$feed_nutrient_consumption <- food_baseline$feed_nutrient_consumption %>%
  # Combine Eggs & Poultry
  mutate(animal = ifelse(animal == "Eggs", "Poultry", animal)) %>%
  group_by(country, animal) %>%
  summarise_all(sum) %>%
  ungroup()




## BASELINE HUMAN POPULATION --------------------------
# Projections
pop <- read_csv("data/population.csv", skip = 1) %>% select(-source, -notes) %>% 
  # Use UN medium, which comes close (73.0m) to ONS projection of 72.4m by 2043 https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/nationalpopulationprojections/2018based
  filter(est %in% c("estimate", "medium variant")) %>%
  filter(!(year == 2020 & est == "estimate")) %>% 
  select(year, pop) %>% 
  mutate(pop_rel = pop/pop[year == 2015]) #%>% 
# filter(year %in% seq(2015, 2100, by = 5)) 

# Distribution across 4 countries
pop_countries <- read_csv("data/population_countries.csv", skip = 1) %>% select(-source, -year) %>% 
  mutate(pop = pop / sum(pop)) 






## AGRICULTURE - LIVESTOCK --------------------------
# Manure management system info
manure_systems <- read_csv("data/ghg/ag/manure_systems.csv", skip = 1) %>% select(-notes)

## CH4 emissions from enteric fermentation
gwp_entericferm_ch4 <- read_csv("data/ghg/ag/entericferm_ch4.csv", skip = 1) %>% select(-source)

# Calculate GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_entericferm_ch4 <- gwp_entericferm_ch4 %>% 
  # Calculate GWP100
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Get baseline GE consumption
  left_join(food_baseline$feed_nutrient_consumption, by = "animal") %>%
  rename("baseline_ge" = ge) %>% 
  # Allocate GWP to each country
  group_by(animal) %>% 
  mutate(gwp_t = gwp_t * baseline_ge / sum(baseline_ge)) %>%
  ungroup() %>% 
  # Tidy
  mutate(baseline_ge = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_ge)) %>% 
  select(category, ag_sector, country, animal, gwp_t, ghg, baseline_ge)



## CH4 emissions from manure management
gwp_manureman_ch4 <- read_csv("data/ghg/ag/manureman_ch4.csv", skip = 1) %>% select(-source)

# Calculate GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_manureman_ch4 <- gwp_manureman_ch4 %>%
  # Calculate GWP100
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Get baseline ME consumption
  left_join(food_baseline$feed_nutrient_consumption, by = "animal") %>%
  rename("baseline_me" = me) %>% 
  # Allocate GWP to each country
  group_by(animal) %>% 
  mutate(gwp_t = gwp_t * baseline_me / sum(baseline_me)) %>%
  ungroup() %>% 
  # Tidy
  mutate(baseline_me = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_me)) %>% 
  select(category, ag_sector, country, animal, gwp_t, ghg, baseline_me) %>% 
  # Add multiplier for grass-fed ruminants (MCF lower for pasture than other manure systems) 
  bind_rows((.) %>% 
              filter(animal %in% c("Beef", "Milk", "Sheep")) %>%  
              left_join(manure_systems %>% 
                          filter(animal %in% c("Beef", "Milk", "Sheep")) %>%
                          group_by(animal) %>% 
                          mutate(prop = kg_n / sum(kg_n))%>% 
                          summarise(grassfed_multiplier = mcf[manure_system == "Pasture"] / sum(prop * mcf), .groups = "drop"),
                        by = "animal") %>% 
              mutate(animal = paste0(animal, " (grass-fed)"))) %>% 
  mutate(grassfed_multiplier = ifelse(is.na(grassfed_multiplier), 1, grassfed_multiplier))




## Direct N2O emissions from manure management
gwp_manureman_directn2o <- read_csv("data/ghg/ag/manureman_directn2o.csv", skip = 1) %>% select(-source)

# Calculate GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_manureman_directn2o <- gwp_manureman_directn2o %>%
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Get baseline ME consumption
  left_join(food_baseline$feed_nutrient_consumption, by = "animal") %>%
  rename("baseline_cp" = cp) %>% 
  # Allocate GWP to each country
  group_by(animal) %>% 
  mutate(gwp_t = gwp_t * baseline_cp / sum(baseline_cp)) %>%
  ungroup() %>% 
  # Tidy
  mutate(baseline_cp = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_cp)) %>% 
  select(category, ag_sector, country, animal, gwp_t, ghg, baseline_cp)
# No need for grassfed multiplier - column 'animal' excludes grass fed, as no handled manure



## Indirect N2O emissions from manure management
gwp_manureman_indirectn2o <- read_csv("data/ghg/ag/manureman_indirectn2o.csv", skip = 1) %>% select(-source)

# Calculate GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_manureman_indirectn2o <- manure_systems %>%
  # Consider only indoor systems
  filter(manure_system %in% c("Liquid", "Solid", "Other")) %>% 
  group_by(animal) %>% 
  # Calculate (approx) fraction of total vol/leach attributable to each livestock type
  summarise(n_vol = sum(kg_n * frac_gasm),
            n_leach = sum(kg_n * frac_leach)) %>% 
  ungroup() %>% 
  mutate(prop_n_vol = n_vol / sum(n_vol),
         prop_n_leach = n_leach / sum(n_leach)) %>% 
  select(-n_vol, -n_leach) %>% 
  # Join with baseline emissions
  crossing(gwp_manureman_indirectn2o) %>% 
  mutate(prop = ifelse(category == "Indirect N2O manure management - leach", prop_n_leach, prop_n_vol)) %>%
  # Calculate GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100 * prop) %>% 
  # Get baseline CP content
  inner_join(food_baseline$feed_nutrient_consumption %>% select(country, animal, cp), by = "animal") %>% 
  rename('baseline_cp' = cp) %>%
  # Allocate GWP to each country
  group_by(animal, category) %>% 
  mutate(gwp_t = gwp_t * baseline_cp / sum(baseline_cp),
         ag_sector = 1) %>% 
  ungroup() %>% 
  # Tidy
  mutate(baseline_cp = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_cp)) %>% 
  select(category, ag_sector, country, animal, gwp_t, ghg, baseline_cp) 
# No need for grassfed multiplier - column 'animal' excludes grass fed, as no handled manure


## AGRICULTURE - SOIL ---------------------------------
## N2O emissions from agricultural soils
gwp_agsoils_n2o <- read_csv("data/ghg/ag/agsoils_n2o.csv", skip = 1) %>% select(-source, -usage)

# Applied manure info
manure_agsoils <- read_csv("data/ghg/ag/manure_agsoils.csv", skip = 1) %>% select(-notes)

# Deposited urine/dung info
urinedung_agsoils <- read_csv("data/ghg/ag/urinedung_agsoils.csv", skip = 1) %>% select(-notes)

# Synthetic N fertiliser application rates 
synthn_rates <- read_csv("data/ghg/ag/synthn_rates.csv", skip = 1) %>% select(-source, -notes, -units, -area) 

# Synthetic N EFs
synthn_efs <- read_csv("data/ghg/ag/synthn_efs.csv", skip = 1) %>% select(-source) 

# Baseline N inputs to soil (for indirect N2O)
agsoils_inputs <- read_csv("data/ghg/ag/agsoils_inputs.csv", skip = 1) 

# Update synthn_rates for organic farming
synthn_rates <- synthn_rates %>% 
  crossing(organic = c(TRUE, FALSE)) %>% 
  mutate(rate = ifelse(organic, 0, rate))



## Derived baseline indirect N2O emissions per source
gwp_agsoils_indirect_n2o <- agsoils_inputs %>%
  # Calculate (approx) fraction of total vol/leach attributable to each source
  mutate(prop_n_vol = (kg_n * frac_gas) / sum(kg_n * frac_gas),
         prop_n_leach = (kg_n * frac_leach) / sum(kg_n * frac_leach)) %>% 
  select(-frac_gas, -frac_leach, -ef4, -ef5) %>% 
  rename(category1 = category) %>% 
  # Join with baseline emissions
  crossing(filter(gwp_agsoils_n2o, category %in% c("Indirect N2O soils - leach", "Indirect N2O soils - vol"))) %>% 
  # Allocate total indirect N2O emissions to each source 
  mutate(ghg_t = ghg_t * ifelse(category == "Indirect N2O soils - leach", prop_n_leach, prop_n_vol)) %>% 
  mutate(category = paste0(category1, " - indirect ", gsub("Indirect N2O soils - ", "", category))) %>%
  # Tidy
  select(category, units, ghg_t, ghg, ag_sector) 



## Calculate manure GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_manure_agsoils_n2o <- manure_agsoils %>% 
  # Calculate (approx) fraction of total spread manure N2O attributable to each livestock type
  group_by(animal) %>% 
  summarise(n = sum(kg_n * (1 - frac_loss) * ef1)) %>%
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  select(-n) %>% 
  # Join with baseline emissions - direct and indirect
  crossing(bind_rows(filter(gwp_agsoils_n2o, category == "Animal manure applied"),
                     filter(gwp_agsoils_indirect_n2o, category %in% c("Animal manure applied - indirect leach", "Animal manure applied - indirect vol")))) %>% 
  # Calculate GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100 * prop) %>% 
  # Get baseline CP content
  inner_join(food_baseline$feed_nutrient_consumption %>% select(country, animal, cp), by = "animal") %>% 
  rename('baseline_cp' = cp) %>%
  # Allocate GWP to each country
  group_by(animal, category) %>% 
  mutate(gwp_t = gwp_t * baseline_cp / sum(baseline_cp),
         ag_sector = 1) %>% 
  ungroup() %>% 
  # Tidy
  mutate(baseline_cp = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_cp)) %>% 
  select(category, ag_sector, country, animal, gwp_t, ghg, baseline_cp) 
# No need for grassfed multiplier - column 'animal' excludes grass fed, as no handled manure


## Calculate urine/dung GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_urinedung_agsoils_n2o <- urinedung_agsoils %>%
  # Calculate (approx) fraction of total spread manure N2O attributable to each livestock type
  mutate(prop = (kg_n * ef3) / sum(kg_n * ef3)) %>% 
  select(-kg_n, -ef3) %>% 
  # Join with baseline emissions - direct and indirect
  crossing(bind_rows(filter(gwp_agsoils_n2o, category == "Urine and dung deposited"),
                     filter(gwp_agsoils_indirect_n2o, category %in% c("Urine and dung deposited - indirect leach", "Urine and dung deposited - indirect vol")))) %>% 
  # Calculate GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100 * prop) %>% 
  # Get baseline CP content
  inner_join(food_baseline$feed_nutrient_consumption %>% select(country, animal, cp), by = "animal") %>% 
  rename('baseline_cp' = cp) %>%
  # Allocate GWP to each country
  group_by(animal, category) %>% 
  mutate(gwp_t = gwp_t * baseline_cp / sum(baseline_cp),
         ag_sector = 1) %>% 
  ungroup() %>% 
  # Tidy
  mutate(baseline_cp = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_cp)) %>% 
  select(category, ag_sector, country, animal, gwp_t, ghg, baseline_cp) %>%
  # Add multiplier for grass-fed ruminants (more N deposited as urine/dung)
  bind_rows((.) %>% 
              filter(animal %in% c("Beef", "Milk", "Sheep")) %>%  
              left_join(manure_systems %>% 
                          filter(animal %in% c("Beef", "Milk", "Sheep")) %>%
                          group_by(animal) %>% 
                          mutate(prop = kg_n / sum(kg_n))%>% 
                          summarise(grassfed_multiplier = 1 / prop[manure_system == "Pasture"], .groups = "drop"),
                        by = "animal") %>% 
              mutate(animal = paste0(animal, " (grass-fed)"))) %>% 
  mutate(grassfed_multiplier = ifelse(is.na(grassfed_multiplier), 1, grassfed_multiplier))


## Calculate synthetic N GWP100 (t CO2e) - PER HECTARE OF EACH CROP
gwp_synthn_agsoils_n2o <- synthn_efs %>%
  filter(category == "Soil N2O from inorganic N") %>%
  # Derived EF for indirect N2O
  bind_rows(gwp_agsoils_indirect_n2o %>% 
              filter(category %in% c("Inorganic N fertiliser - indirect leach", "Inorganic N fertiliser - indirect vol")) %>%
              mutate(ghg_t = ghg_t / agsoils_inputs$kg_n[agsoils_inputs$category == "Inorganic N fertiliser"],
                     units = "t N2O / kg N",
                     frac = 1)) %>% 
  # Convert to GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100 * frac) %>% 
  select(category, ag_sector, gwp_t, ghg) %>%
  # Cross with application rates
  crossing(synthn_rates) 



## Calculate sewage N GWP100 (t CO2e) - PROPORTIONAL TO POP GROWTH
gwp_sewage_agsoils_n2o <- bind_rows(filter(gwp_agsoils_n2o, category == "Sewage sludge applied"),
                                    filter(gwp_agsoils_indirect_n2o, category %in% c("Sewage sludge applied - indirect leach", "Sewage sludge applied - indirect vol"))) %>% 
  # Calculate GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Allocate to 4 countries on basis of 2015 population
  crossing(pop_countries) %>% 
  mutate(gwp_t = gwp_t * pop) %>% 
  # Get baseline population size
  mutate(baseline_pop = !!pop$pop[pop$year == 2015]) %>% 
  select(category, ag_sector, country, gwp_t, ghg, baseline_pop) 



## Calculate crop residue N GWP100 (t CO2e) - PROPORTIONAL TO CROP TONNAGE
gwp_cropresidues_agsoils_n2o <- bind_rows(filter(gwp_agsoils_n2o, category == "Crop residues"),
                                          filter(gwp_agsoils_indirect_n2o, category %in% c("Crop residues - indirect leach", "Crop residues - indirect vol"))) %>% 
  # Calculate GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Get baseline crop tonnage
  crossing(food_baseline$food_type_sum %>% 
             filter(product %in% c("Cereal", "Fruit & vegetables", "Legumes", "Oilseeds", "Potatoes", "Sugar beet")) %>% 
             group_by(country) %>% 
             summarise(baseline_tonnage = sum(tonnage))) %>%
  # Allocate GWP to each country
  group_by(category) %>% 
  mutate(gwp_t = gwp_t * baseline_tonnage / sum(baseline_tonnage)) %>%
  ungroup() %>% 
  # Tidy
  select(category, ag_sector, country, gwp_t, ghg, baseline_tonnage)



## AGRICULTURE - OTHER ---------------------------
# Read baseline emissions
gwp_otherag <- read_csv("data/ghg/ag/otherag_efs.csv", skip = 1) %>% select(-source, -usage) 



## Pesticides
gwp_pesticides <- gwp_otherag %>%
  filter(category == "Pesticide manufacture and breakdown") %>% 
  # Convert to GWP - per ha of cropland
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Add organic row (no pesticides)
  crossing(organic = c(TRUE, FALSE)) %>% 
  mutate(gwp_t = ifelse(organic, 0, gwp_t)) %>% 
  # Tidy
  select(category, organic, ag_sector, gwp_t, ghg) 



## Other ag - liming and energy 
gwp_otherag <- gwp_otherag %>% 
  filter(category != "Pesticide manufacture and breakdown") %>% 
  # Convert to GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Cross with total tonnage
  crossing(food_baseline$food_type_sum %>% 
             filter(product %in% c("Cereal", "Fruit & vegetables", "Legumes", "Oilseeds", "Potatoes", "Sugar beet")) %>% 
             group_by(country) %>% 
             summarise(baseline_tonnage = sum(tonnage))) %>% 
  # Allocate GWP to each country
  group_by(category, ghg) %>% 
  mutate(gwp_t = gwp_t * baseline_tonnage / sum(baseline_tonnage)) %>%
  ungroup() %>% 
  # Tidy
  select(category, ag_sector, country, gwp_t, ghg, baseline_tonnage)



## Fertilisers - manufacture and urea breakdown
gwp_synthn_other <- synthn_efs %>%
  filter(!category %in% c("Soil N2O from inorganic N")) %>%
  # Convert to GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100 * frac) %>% 
  select(category, ag_sector, gwp_t, ghg) %>%
  # Cross with application rates
  crossing(synthn_rates) 



## FEED IMPORTS -------------------------
load("rdata/food/food_params.RData")

# Emissions from imported feed
gwp_feed_imported <- food_baseline$feed_imported %>% 
  # Calculate mean energy density (MJ / t) of cereals
  crossing(food_params$.feed_mj %>% 
             filter(crop %in% c("Barley", "Cereals, other", "Oats", "Wheat")) %>% 
             summarise(mj_t = mean(mj*dm))) %>%
  # Calculate baseline GWP (t CO2e - 1.54 from Lamb et al.)
  mutate(gwp_t = 1.54 * mj_imported / mj_t) %>% 
  # Tidy
  mutate(category = "Feed imports",
         ag_sector = 0,
         ghg = "CO2") %>% 
  rename(baseline_mj_imported = mj_imported) %>% 
  select(category, ag_sector, country, gwp_t, ghg, baseline_mj_imported) 



## SAVE --------------------------
ghg_pars_ag <- list(gwp_entericferm_ch4 = gwp_entericferm_ch4,
                    gwp_manureman_ch4 = gwp_manureman_ch4,
                    gwp_manureman_indirectn2o = gwp_manureman_indirectn2o,
                    gwp_manureman_directn2o = gwp_manureman_directn2o,
                    gwp_manure_agsoils_n2o = gwp_manure_agsoils_n2o,
                    gwp_urinedung_agsoils_n2o = gwp_urinedung_agsoils_n2o,
                    gwp_synthn_agsoils_n2o = gwp_synthn_agsoils_n2o,
                    gwp_sewage_agsoils_n2o = gwp_sewage_agsoils_n2o,
                    gwp_cropresidues_agsoils_n2o = gwp_cropresidues_agsoils_n2o,
                    gwp_pesticides = gwp_pesticides,
                    gwp_otherag = gwp_otherag,
                    gwp_synthn_other = gwp_synthn_other,
                    gwp_feed_imported = gwp_feed_imported)

save(ghg_pars_ag,
     pop, 
     file = "rdata/ghg/ghg_pars_ag.RData")