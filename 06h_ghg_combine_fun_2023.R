## INIT -----------------------------------------
source("setup.R")

## TIDY SCENARIO AREAS -----------------
# Mapped areas
load("rdata/scenarios/scenarios_areas_2022.RData") # Slow to load. Alternatives are quicker to load, but take up much more space!


## SCENARIO/AMBITION LEVELS -----------------------------
# Ambition combinations
ambition_combos <- read_csv("data/ambition_combinations_new.csv", skip = 0)

# Scenario params
scenario_params <- read_csv("data/scenario_parameters_new.csv", skip = 1)

# Pull out scenario names
scenario_names <- ambition_combos %>% select(scenario, name) %>% unique() 


## GET GHG PARAMS ----------------------
## Ag
# Food results
load("rdata/ghg/ghg_pars_ag.RData")
load("rdata/food/food_results_basic_2022.RData") # Raw food results
load("rdata/food/food_results_gapclosed_2022.RData") # Food results after yield growth/waste reduction/feed subs

# Manure management system info
manure_systems <- read_csv("data/ghg/ag/manure_systems.csv", skip = 1) %>% select(-notes)

# Get low carbon farming params
lowcarbonfarming <- read_csv("data/lowcarbonfarming.csv", skip = 1) %>% select(-sruc_code)


## Intertidal
load("rdata/ghg/ghg_pars_intertidal.RData")


## Peat
load("rdata/ghg/ghg_pars_peat.RData")


## Bioenergy
load("rdata/ghg/ghg_pars_bioenergy.RData")


## Hedges
load("rdata/ghg/ghg_pars_hedges.RData")


## LUC
load("rdata/ghg/ghg_pars_luc.RData")


## Woodland/agroforestry
load("rdata/ghg/ghg_pars_wood_2023.RData")

# Agroforestry footprint
ghg_pars_agroforestry <- read_csv("data/ghg/ghg_agroforestry.csv", skip = 1) %>% 
  select(-notes) %>%
  filter(type != "Silvoarable agroforestry (apples)")



## WRITE FUNCTION ------------------------
do_ghg_fun <- function(scenarios_areas, scenarios_change_areas,
                       scenario_params, ambition_combos,
                       ghg_pars_luc,
                       ghg_pars_ag, food_basic, food_gapclosed, pop, manure_systems, lowcarbonfarming,
                       ghg_pars_peat,
                       ghg_pars_intertidal,
                       ghg_pars_bioenergy,
                       ghg_pars_wood, 
                       ghg_pars_agroforestry,
                       ghg_pars_hedges){
  # NOTE: using data.table to handle initial massive data (more efficient), then tidyverse (more familiar)
  
  # Get all years 2015-2050
  years <- tibble(year = rep(seq(2015, 2100, by = 5), each = 5)[1:85],
                  year_ = seq(2015, 2099, by = 1)) 
  
  ## LUC -------------------------------------
  print("Doing LUC")
  # Get land-use change areas
  # Calculate land-use change areas
  luc_areas <- scenarios_change_areas[!lcm_to %chin% c("s.m", "b.g", "f.s", "a.h_palud", "i.g_palud")
  ][!(lcm_from == "c.w" & lcm_to %chin% c("b.w", "c.w_pinewood"))
  ][peat == 0
  ][, ':='(lcm_from = ifelse(lcm_from %chin% c("a.h_energy", "i.g_energy"), lcm_from, substr(lcm_from, 1, 3)),
           lcm_to = ifelse(lcm_to %chin% c("a.h_energy", "i.g_energy"), lcm_to, substr(lcm_to, 1, 3)))
  ][lcm_from != lcm_to
    , keyby = .(name, scenario, year, lcm_from, lcm_to, country)
    , .(area = sum(area))] %>% 
    as_tibble() %>% 
    # Annualise (divide changing area across 5 years)
    mutate(year = year - 5) %>% 
    left_join(years, by = "year") %>%
    mutate(area = area / 5) %>%
    select(-year) %>% 
    rename(year = year_) %>%
    arrange(scenario, name, year, country) 
  
  # Get Miscanthus/SRC prop
  prop_bioenergycrop <- scenario_params %>%
    select(-note) %>%
    filter(par %in% c("bioenergy_prop_miscanthus", "bioenergy_prop_src")) %>%
    rename(bioenergy_crop = par,
           prop = val) %>%
    group_by(ambition) %>%
    mutate(prop = prop / sum(prop),
           prop = ifelse(is.na(prop), 0, prop),
           bioenergy_crop = ifelse(bioenergy_crop == "bioenergy_prop_miscanthus", "a.h_energy_misc", "a.h_energy_src")) %>%
    ungroup() %>% 
    # Join with scenarios
    left_join(ambition_combos %>% 
                select(scenario, ambition_bioenergy) %>% 
                rename(ambition = ambition_bioenergy), 
              by = "ambition") %>% 
    select(-ambition)
  
  # Add Miscanthus/SRC to luc_areas
  luc_areas <- luc_areas %>% 
    filter(lcm_to == "a.h_energy") %>% 
    left_join(prop_bioenergycrop, by = "scenario") %>% 
    mutate(lcm_to = bioenergy_crop,
           area = area * prop) %>% 
    select(-bioenergy_crop, -prop) %>% 
    bind_rows(filter(luc_areas, lcm_to != "a.h_energy"), .)
  
  # Label categories
  luc_areas <- luc_areas %>% 
    mutate(category_1 = case_when(lcm_to %in% c("i.g_energy") ~ "Bioenergy crops",
                                  lcm_to %in% c("a.h_energy_misc", "a.h_energy_src") ~ "Bioenergy crops",
                                  lcm_to %in% c("b.w", "c.w", "c.w_pinewood") ~ "Woodland",
                                  lcm_to %in% c("a.g", "c.g", "n.g") ~ "Semi-natural grassland creation",
                                  lcm_to %in% c("h.r") ~ "Heathland creation",
                                  lcm_to %in% c("b.t") ~ "Urban expansion"))
  
  
  gwp_luc <- bind_rows(
    # Calculate instant change in non-forest biomass
    luc_areas %>%
      left_join(ghg_pars_luc$gwp_luc_biomass %>% filter(!lcm_from %in% c("b.w", "c.w") & !lcm_to %in% c("b.w", "c.w")) , by = c("lcm_from", "lcm_to")) %>%
      group_by(name, scenario, year, country, category_1) %>%
      summarise(gwp_t = sum(gwp_t * area), .groups = "drop") %>%
      mutate(gwp_t = ifelse(is.na(gwp_t), 0, gwp_t),
             category_2 = "Land-use change - non-forest biomass"),
    # Calculate annual change in soil C
    luc_areas %>% 
      inner_join(ghg_pars_luc$gwp_luc_soil, by = c("lcm_from", "lcm_to", "country")) %>%
      mutate(year = year.x + year.y) %>%
      select(-year.x, -year.y) %>%
      filter(year <= 2100) %>% 
      group_by(name, scenario, year, country, category_1) %>% 
      summarise(gwp_t = sum(gwp_t * area), .groups = "drop") %>% 
      mutate(category_2 = "Land-use change - soil carbon"))
  
  
  
  ## AGRICULTURE --------------------------
  print("Doing agriculture...")
  ## Pull out activity data
  # Pull out livestock data
  livestock_data <- bind_rows(food_results_basic,
                              food_results_gapclosed) %>%
    mutate(d = purrr::map(food, ~.[[4]])) %>% 
    select(-food, -i) %>% 
    unnest(cols = c(data, d)) %>% 
    mutate(yield_growth = ifelse(is.na(yield), 1, yield),
           option = ifelse(is.na(option), "None", as.character(option))) %>% 
    select(-waste, -feed, -action, -yield) %>% 
    # Combine Eggs & Poultry
    mutate(animal = ifelse(animal == "Eggs", "Poultry", animal)) %>%
    group_by(scenario, name, option, yield_growth, year, country, animal) %>%
    summarise_all(sum) %>% 
    ungroup() %>% 
    # Annualise (linear interpolate between years)
    full_join(years, by = "year") %>%
    filter(year_ <= 2050) %>% 
    group_by(scenario, name, option, country, animal) %>%
    mutate_at(vars(me, ge, cp, yield_growth), ~zoo::na.approx(ifelse(year == year_, ., NA))) %>% 
    ungroup() %>% 
    select(-year) %>% 
    rename(year = year_) %>% 
    # Add rows for deer, goats and horses
    bind_rows((.) %>% 
                select(scenario, name, option, yield_growth, year, country) %>% 
                unique() %>% 
                crossing(animal = c("Deer", "Goats", "Horses"))) 
  
  
  # Pull out ha per crop
  crop_ha <- bind_rows(food_results_basic,
                       food_results_gapclosed) %>%
    ungroup() %>% 
    mutate(d = purrr::map(food, ~.[[3]])) %>% 
    select(-food, -i) %>% 
    unnest(cols = c(data, d)) %>% 
    mutate(yield_growth = ifelse(is.na(yield), 1, yield),
           option = ifelse(is.na(option), "None", as.character(option))) %>% 
    select(-waste, -feed, -yield, -action) %>% 
    # Annualise (linear interpolate between years)
    full_join(years, by = "year") %>%
    filter(year_ <= 2050) %>% 
    group_by(scenario, name, option, country, crop_fine, organic) %>%
    mutate_at(vars(area, tonnage, yield_growth), ~zoo::na.approx(ifelse(year == year_, ., NA), na.rm = FALSE)) %>% 
    ungroup() %>% 
    select(-year) %>% 
    rename(year = year_)
  
  # Pull out crop tonnage
  crop_tonnage <- bind_rows(food_results_basic,
                            food_results_gapclosed) %>%
    ungroup() %>% 
    mutate(d = purrr::map(food, ~.[[1]])) %>% 
    select(-food, -i) %>% 
    unnest(cols = c(data, d)) %>% 
    mutate(yield_growth = ifelse(is.na(yield), 1, yield),
           option = ifelse(is.na(option), "None", as.character(option))) %>% 
    select(-waste, -feed, -yield, -action) %>%  
    filter(product %in% c("Cereal", "Fruit & vegetables", "Legumes", "Oilseeds", "Potatoes", "Sugar beet")) %>% 
    group_by(scenario, name, option, yield_growth, year, country) %>% 
    summarise(tonnage = sum(tonnage), .groups = "drop") %>% 
    # Annualise (linear interpolate between years)
    full_join(years, by = "year") %>%
    filter(year_ <= 2050) %>% 
    group_by(scenario, name, option, country) %>%
    mutate_at(vars(tonnage, yield_growth), ~zoo::na.approx(ifelse(year == year_, ., NA), na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(-year) %>% 
    rename(year = year_)
  
  # Pull out feed imports
  feed_imports <- bind_rows(food_results_basic,
                            food_results_gapclosed) %>%
    ungroup() %>% 
    mutate(d = purrr::map(food, ~.[[5]])) %>% 
    select(-food, -i) %>% 
    unnest(cols = c(data, d)) %>% 
    mutate(yield_growth = ifelse(is.na(yield), 1, yield),
           option = ifelse(is.na(option), "None", as.character(option))) %>% 
    select(-waste, -feed, -yield, -action) %>% 
    # Annualise (linear interpolate between years)
    full_join(years, by = "year") %>%
    filter(year_ <= 2050) %>% 
    group_by(scenario, name, option, country) %>%
    mutate_at(vars(mj_imported , yield_growth), ~zoo::na.approx(ifelse(year == year_, ., NA), na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(-year) %>% 
    rename(year = year_)
  
  
  # Pull out population
  pop_change <- crop_tonnage %>% 
    select(-tonnage) %>% 
    left_join(pop, by = "year")
  
  # Estimate fraction of manure emissions arising from liquid systems
  prop_liquid <- manure_systems %>% 
    filter(manure_system %in% c("Liquid", "Solid", "Other")) %>%
    group_by(animal) %>% 
    mutate(prop_direct = (kg_n * ef3) / sum(kg_n * ef3),
           prop_vol = (kg_n * frac_gasm) / sum(kg_n * frac_gasm),
           prop_ch4 = (kg_n * bo * mcf) / sum(kg_n * bo * mcf)) %>%
    ungroup() %>% 
    filter(manure_system == "Liquid") %>% 
    select(animal, prop_direct, prop_vol, prop_ch4)
  
  # Get spring crops % area 
  spring_crop_props <- crop_ha %>%
    mutate(type = case_when(crop_fine %in% c("Permanent grass", "Temporary grass") ~ crop_fine,
                            crop_fine == "Temporary grass (new ley)" ~ "Temporary grass",
                            grepl("spring", crop_fine) | crop_fine %in% c("Potatoes", "Sugar beet", "Maize, whole", "Fodder crops", "Horticultural crops") ~ "Spring crop",
                            TRUE ~ "Other crop")) %>% 
    group_by(scenario, name, option, yield_growth, year, country, type) %>% 
    summarise(area = sum(area),
              tonnage = sum(tonnage), .groups = "drop") %>% 
    filter(type %in% c("Other crop", "Spring crop")) %>% 
    group_by(scenario, name, option, year, country) %>% 
    mutate(prop_area = area / sum(area[type %in% c("Spring crop", "Other crop")]),
           prop_tonnage = tonnage / sum(tonnage[type %in% c("Spring crop", "Other crop")])) %>% 
    ungroup() %>% 
    filter(type == "Spring crop") %>% 
    select(-type, -area, -tonnage)
  
  
  ## Account for low carbon farming
  # Tidy measures
  uptake_lowcarbonfarming <- lowcarbonfarming %>% 
    select(-scope, -impact) %>% 
    gather(ambition, val, -measure, -start_year, -rollout_period) %>% 
    mutate(ambition = gsub("uptake_", "", ambition)) %>% 
    group_by(measure) %>% 
    mutate(uptake_low = unique(val[ambition == "low"])) %>%
    # Cross with all years
    crossing(year = 2015:2100) %>%
    mutate(uptake = case_when(year < start_year ~ uptake_low,
                              year >= start_year & year < start_year + rollout_period ~ uptake_low + (year - start_year + 1) * (val - uptake_low) / rollout_period,
                              TRUE ~ val),
           uptake = uptake - uptake_low) %>%
    # Join with scenarios
    left_join(ambition_combos %>% 
                select(scenario, ambition_lowcarbonfarming) %>% 
                rename(ambition = ambition_lowcarbonfarming), 
              by = "ambition") %>% 
    select(-val, -uptake_low, -start_year, -rollout_period, -ambition) %>% 
    arrange(scenario, year) 
  
  
  ## Derive final emissions
  # Emissions from synth N
  gwp_synthn_agsoils_n2o <- crop_ha %>% 
    inner_join(ghg_pars_ag$gwp_synthn_agsoils_n2o, by = c("crop_fine", "organic")) %>% 
    # NUE (crop): reduces N rate
    left_join(uptake_lowcarbonfarming %>% filter(measure == "nue_crop") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(rate = case_when((!crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)")) & yield_growth == 1 ~ rate * (1 - uptake), 
                            # (!crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)")) & yield_growth > 1 ~ rate + rate * (yield_growth - 1) * (1 - uptake),
                            (!crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)")) & yield_growth > 1 ~ rate,
                            TRUE ~ rate)) %>%
    select(-uptake) %>%
    # NUE (grass): reduces N rate
    left_join(uptake_lowcarbonfarming %>% filter(measure == "nue_grass") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(rate = case_when(crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)") & yield_growth == 1 ~ rate * (1 - uptake), 
                            # crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)") & yield_growth > 1 ~ rate + rate * (yield_growth - 1) * (1 - uptake),
                            crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)") & yield_growth > 1 ~ rate,
                            TRUE ~ rate)) %>%
    select(-uptake) %>%
    # Grass-legume mix: reduce N input to 0 on permanent grass and 50 (if > 50) on temporary grass
    left_join(uptake_lowcarbonfarming %>% filter(measure == "grass_legume_mix") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(rate = case_when(crop_fine == "Permanent grass" ~ rate * (1 - uptake),
                            crop_fine  %in% c("Temporary grass", "Temporary grass (new ley)") ~ min(50, rate) + rate * (1 - uptake),
                            TRUE ~ rate)) %>%
    select(-uptake) %>%
    # Cover crops: reduce leaching in 34% of spring crops by 45%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_crops") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(category == "Inorganic N fertiliser - indirect leach" & grepl("spring", crop_fine),
                          gwp_t * (1 - (0.34 * uptake * 0.45)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Soil loosening: reduce direct N2O emissions by 40% across 20% of land
    left_join(uptake_lowcarbonfarming %>% filter(measure == "loosen_soils") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(category == "Soil N2O from inorganic N",
                          gwp_t * (1 - (0.2 * uptake * 0.4)),
                          gwp_t)) %>%
    select(-uptake) %>%
    mutate(category_2 = "Agriculture - N2O from synthetic N") %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(rate * area * gwp_t), .groups = "drop")
  
  
  # Emissions from synth N manufacture/breakdown
  gwp_synthn_other <- crop_ha %>% 
    inner_join(ghg_pars_ag$gwp_synthn_other, by = c("crop_fine", "organic")) %>% 
    # NUE (crop): reduces N rate
    left_join(uptake_lowcarbonfarming %>% filter(measure == "nue_crop") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(rate = case_when((!crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)")) & yield_growth == 1 ~ rate * (1 - uptake), 
                            # (!crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)")) & yield_growth > 1 ~ rate + rate * (yield_growth - 1) * (1 - uptake),
                            (!crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)")) & yield_growth > 1 ~ rate,
                            TRUE ~ rate)) %>%
    select(-uptake) %>%
    # NUE (grass): reduces N rate
    left_join(uptake_lowcarbonfarming %>% filter(measure == "nue_grass") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(rate = case_when(crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)") & yield_growth == 1 ~ rate * (1 - uptake), 
                            # crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)") & yield_growth > 1 ~ rate + rate * (yield_growth - 1) * (1 - uptake),
                            crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)") & yield_growth > 1 ~ rate,
                            TRUE ~ rate)) %>%
    select(-uptake) %>%
    # Grass-legume mix: reduce N input to 0 on  permanent grass and 50 on temporary grass
    left_join(uptake_lowcarbonfarming %>% filter(measure == "grass_legume_mix") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(rate = case_when(crop_fine == "Permanent grass" ~ rate * (1 - uptake),
                            crop_fine %in% c("Temporary grass", "Temporary grass (new ley)") ~ min(50, rate) + rate * (1 - uptake),
                            TRUE ~ rate)) %>%
    # Summarise
    mutate(category_2 = ifelse(category == "CO2 from urea application", "Agriculture - urea breakdown", "Agriculture - fertiliser manufacture")) %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(rate * area * gwp_t), .groups = "drop")
  
  
  # Emissions from pesticide manufacture/breakdown
  gwp_pesticides <- crop_ha %>%
    filter(!grepl("grass", crop_fine)) %>% 
    left_join(ghg_pars_ag$gwp_pesticides, by = "organic") %>%
    # Summarise
    mutate(category_2 = "Agriculture - pesticides") %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>% 
    summarise(gwp_t = sum(gwp_t * area), .groups = "drop")
  
  
  # Emissions from liming, energy & machinery
  gwp_otherag <- crop_tonnage %>% 
    inner_join(ghg_pars_ag$gwp_otherag, by = "country") %>% 
    # Electrification: reduce emissions from farm energy use by 41%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "electrification") %>% select(-measure), by = c("scenario", "year")) %>% 
    mutate(gwp_t = ifelse(category == "Farm energy use", 
                           gwp_t * (1 - uptake * 0.41),
                           gwp_t)) %>% 
    # Summarise
    mutate(category_2 = case_when(category == "CO2 from liming" ~ "Agriculture - liming",
                                  category == "Farm energy use" ~ "Agriculture - energy use",
                                  category == "Machinery manufacture and maintenance" ~ "Agriculture - machinery")) %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>% 
    summarise(gwp_t = sum(gwp_t * tonnage / baseline_tonnage), .groups = "drop")
  
  
  # C sequestration from grass leys
  gwp_c_agsoils <- crop_ha %>% 
    filter((crop_fine == "Temporary grass") | crop_fine == "Temporary grass (new ley)") %>%
    # 0.202 t CO2 / ha / yr across 5% of temp grass (100% of new organic leys) 
    left_join(uptake_lowcarbonfarming %>% filter(measure == "grass_leys") %>% select(-measure), by = c("scenario", "year")) %>% 
    mutate(gwp_t = ifelse(organic,
                          -0.202,
                          -0.202 * uptake * 0.05),
           ghg = "CO2",
           ag_sector = 1) %>% 
    # Summarise
    mutate(category_2 = "Agriculture - soil C") %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(area * gwp_t), .groups = "drop")
  
  
  # Enteric CH4
  gwp_entericferm_ch4 <- livestock_data %>%
    mutate(animal = gsub("..grass.fed.", "", animal)) %>% 
    inner_join(ghg_pars_ag$gwp_entericferm_ch4, by = c("animal", "country")) %>% 
    # Feed additives: reduce methane emissions by 20% for beef and 30% for dairy
    left_join(uptake_lowcarbonfarming %>% filter(measure == "additive") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = case_when(animal == "Beef" ~ gwp_t * (1 - (uptake * 0.2)),
                             animal == "Milk" ~ gwp_t * (1 - (uptake * 0.3)),
                             TRUE ~ gwp_t)) %>%
    select(-uptake) %>% 
    # Breeding with genomics: reduce methane emissions by 0.15% per year 2030-2050
    left_join(uptake_lowcarbonfarming %>% filter(measure == "breeding_genomics") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = case_when(animal %in% c("Beef", "Milk") ~ gwp_t * (1 - (uptake * 0.0015 * min(max(year - 2030 + 1, 0), 20))),
                             TRUE ~ gwp_t)) %>%
    select(-uptake) %>% 
    # Breeding with GE: reduce methane emissions by 0.4% per year 2040-2050
    left_join(uptake_lowcarbonfarming %>% filter(measure == "breeding_genomics") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = case_when(animal %in% c("Beef", "Milk") ~ gwp_t * (1 - (uptake * 0.004 * min(max(year - 2040 + 1, 0), 10))),
                             TRUE ~ gwp_t)) %>%
    select(-uptake) %>% 
    # Summarise
    mutate(category_2 = "Agriculture - CH4 from enteric fermentation") %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"), 
                                 gwp_t, 
                                 gwp_t * ge / baseline_ge)), .groups = "drop")
  
  
  # Manure CH4
  gwp_manureman_ch4 <- livestock_data %>% 
    inner_join(ghg_pars_ag$gwp_manureman_ch4, by = c("animal", "country")) %>%
    # Precision feeding: reduces dairy & pig VS excretion by 2%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "precision_feeding") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(animal %in% c("Milk", "Pig"), 
                          gwp_t * (1 - (uptake * 0.02)),
                          gwp_t)) %>% 
    select(-uptake) %>% 
    # Cover slurry tanks: Reduce CH4 emissions by 47% for liquid manure
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_slurry") %>% select(-measure), by = c("scenario", "year")) %>%
    left_join(prop_liquid %>% select(animal, prop_ch4) %>% rename(prop_liquid = prop_ch4), by = "animal") %>% 
    mutate(prop_liquid = ifelse(is.na(prop_liquid), 0, prop_liquid),
           gwp_t = gwp_t * (1 - (uptake * prop_liquid * 0.47))) %>% 
    # Summarise
    mutate(category_2 = "Agriculture - CH4 from livestock manure") %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"), 
                                 gwp_t, 
                                 gwp_t * me / baseline_me * grassfed_multiplier)), .groups = "drop")
  
  # Manure N2O
  gwp_manureman_directn2o <- livestock_data %>%
    inner_join(ghg_pars_ag$gwp_manureman_directn2o, by = c("animal", "country")) %>%
    # HSG: reduces dairy N excretion by 9%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "hsg_dairy") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(animal == "Milk", 
                          gwp_t * (1 - (uptake * 0.09)),
                          gwp_t)) %>% 
    select(-uptake) %>%   
    # Precision feeding: reduces dairy & pig N excretion by 2%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "precision_feeding") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(animal %in% c("Milk", "Pig"), 
                          gwp_t * (1 - (uptake * 0.02)),
                          gwp_t)) %>% 
    select(-uptake) %>% 
    # Cover slurry tanks: Reduce N2O emissions by 100% for liquid manure
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_slurry") %>% select(-measure), by = c("scenario", "year")) %>%
    left_join(prop_liquid %>% select(animal, prop_direct) %>% rename(prop_liquid = prop_direct), by = "animal") %>% 
    mutate(prop_liquid = ifelse(is.na(prop_liquid), 0, prop_liquid),
           gwp_t = gwp_t * (1 - (uptake * prop_liquid))) %>% 
    # Summarise
    mutate(category_2 = "Agriculture - N2O from livestock manure") %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"), 
                                 gwp_t, 
                                 gwp_t * cp / baseline_cp)), .groups = "drop")
  
  
  # Manure N2O (indirect)
  gwp_manureman_indirectn2o <- livestock_data %>% 
    inner_join(ghg_pars_ag$gwp_manureman_indirectn2o, by = c("animal", "country")) %>%
    # HSG: reduces dairy N excretion by 9%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "hsg_dairy") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(animal == "Milk", 
                          gwp_t * (1 - (uptake * 0.09)),
                          gwp_t)) %>% 
    select(-uptake) %>%   
    # Precision feeding: reduces dairy & pig N excretion by 2%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "precision_feeding") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(animal %in% c("Milk", "Pig"), 
                          gwp_t * (1 - (uptake * 0.02)),
                          gwp_t)) %>% 
    select(-uptake) %>% 
    # Cover slurry tanks: Reduce NH3 emissions by 80% for liquid manure
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_slurry") %>% select(-measure), by = c("scenario", "year")) %>%
    left_join(prop_liquid %>% select(animal, prop_vol) %>% rename(prop_liquid = prop_vol), by = "animal") %>% 
    mutate(prop_liquid = ifelse(is.na(prop_liquid), 0, prop_liquid),
           gwp_t = gwp_t * (1 - (uptake * prop_liquid * 0.8))) %>% 
    # Summarise
    mutate(category_2 = "Agriculture - N2O from livestock manure") %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"), 
                                 gwp_t, 
                                 gwp_t * cp / baseline_cp)), .groups = "drop") 
  
  
  # Manure applied to soil
  gwp_manure_agsoils_n2o <- livestock_data %>% 
    inner_join(ghg_pars_ag$gwp_manure_agsoils_n2o, by = c("animal", "country")) %>%
    # HSG: reduces dairy N excretion by 9%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "hsg_dairy") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(animal == "Milk", 
                          gwp_t * (1 - (uptake * 0.09)),
                          gwp_t)) %>% 
    select(-uptake) %>%   
    # Precision feeding: reduces dairy & pig N excretion by 2%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "precision_feeding") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(animal %in% c("Milk", "Pig"), 
                          gwp_t * (1 - (uptake * 0.02)),
                          gwp_t)) %>%
    select(-uptake) %>%   
    # Loosen soils: reduces direct N2O emissions by 40% on 20% on grassland
    left_join(uptake_lowcarbonfarming %>% filter(measure == "loosen_soils") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(category == "Animal manure applied",
                          gwp_t * (1 - (0.2 * uptake * 0.4)),
                          gwp_t)) %>%  
    select(-uptake) %>%   
    # Cover crops: reduce leaching in 34% of spring crops by 45%
    # Assume crops (vs grass) recieve 31.2% of manure
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_crops") %>% select(-measure), by = c("scenario", "year")) %>%
    left_join(spring_crop_props, by = c("scenario", "name", "option", "yield_growth", "year", "country")) %>% 
    mutate(gwp_t = ifelse(category == "Animal manure applied - indirect leach",
                          gwp_t * (1 - (0.34 * uptake * 0.45 * prop_area * 0.312)), # prop_area = proportional area of spring crops
                          gwp_t)) %>%
    # Summarise
    mutate(category_2 = "Agriculture - soil N2O from livestock manure") %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"), 
                                 gwp_t, 
                                 gwp_t * cp / baseline_cp)), .groups = "drop")  
  
  # Urine and dung deposited
  gwp_urinedung_agsoils_n2o <- livestock_data %>% 
    inner_join(ghg_pars_ag$gwp_urinedung_agsoils_n2o, by = c("animal", "country")) %>%
    # HSG: reduces dairy N excretion by 9%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "hsg_dairy") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(animal == "Milk", 
                          gwp_t * (1 - (uptake * 0.09)),
                          gwp_t)) %>% 
    select(-uptake) %>%   
    # Precision feeding: reduces dairy & pig N excretion by 2%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "precision_feeding") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(animal %in% c("Milk", "Pig"), 
                          gwp_t * (1 - (uptake * 0.02)),
                          gwp_t)) %>% 
    select(-uptake) %>%   
    # Loosen soils: reduces direct N2O emissions by 40% on 20% on grassland
    left_join(uptake_lowcarbonfarming %>% filter(measure == "loosen_soils") %>% select(-measure), by = c("scenario", "year")) %>%
    mutate(gwp_t = ifelse(category == "Urine and dung deposited",
                          gwp_t * (1 - (0.2 * uptake * 0.4)),
                          gwp_t)) %>%  
    # Summarise
    mutate(category_2 = "Agriculture - soil N2O from livestock manure") %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"), 
                                 gwp_t, 
                                 gwp_t * cp / baseline_cp * grassfed_multiplier)), .groups = "drop") 
  
  
  # Sewage sludge applied
  gwp_sewage_agsoils_n2o <- pop_change %>% 
    inner_join(ghg_pars_ag$gwp_sewage_agsoils_n2o, by = "country") %>% 
    # Cover crops: reduce leaching in 34% of spring crops by 45%
    # Assume crops (vs grass) recieve 95% of biosolids
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_crops") %>% select(-measure), by = c("scenario", "year")) %>%
    left_join(spring_crop_props, by = c("scenario", "name", "option", "yield_growth", "year", "country")) %>% 
    mutate(gwp_t = ifelse(category == "Sewage sludge applied - indirect leach",
                          gwp_t * (1 - (0.34 * uptake * 0.45 * prop_area * 0.95)), # prop_area = proportional area of spring crops
                          gwp_t)) %>%
    # Summarise
    mutate(gwp_t = gwp_t * pop_rel) %>% 
    mutate(category_2 = "Agriculture - N2O from other N") %>% 
    select(scenario, name, option, year, country, ag_sector, ghg, gwp_t, category_2)
  
  
  # Crop residues
  gwp_cropresidues_agsoils_n2o <- crop_tonnage %>%
    inner_join(ghg_pars_ag$gwp_cropresidues_agsoils_n2o, by = "country") %>%
    # Cover crops: reduce leaching in 34% of spring crops by 45%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_crops") %>% select(-measure), by = c("scenario", "year")) %>%
    left_join(spring_crop_props, by = c("scenario", "name", "option", "yield_growth", "year", "country")) %>% 
    mutate(gwp_t = ifelse(category == "Crop residues - indirect leach",
                          gwp_t * (1 - (0.34 * uptake * 0.45 * prop_tonnage)), # prop_tonnage = proportional *tonnage* of spring crops
                          gwp_t)) %>%
    # Summarise
    mutate(gwp_t = gwp_t * tonnage / baseline_tonnage) %>% 
    mutate(category_2 = "Agriculture - N2O from other N") %>%
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(gwp_t), .groups = "drop")   
  
  
  # Feed imports
  gwp_feed_imported <- feed_imports %>% 
    inner_join(ghg_pars_ag$gwp_feed_imported, by = "country") %>% 
    # Summarise
    mutate(category_2 = "Agriculture - imported feed") %>% 
    group_by(scenario, name, option, year, country, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(gwp_t * mj_imported / baseline_mj_imported), .groups = "drop")   
  
  ## Combine all
  gwp_ag <- bind_rows(gwp_synthn_agsoils_n2o,
                      gwp_synthn_other,
                      gwp_pesticides,
                      gwp_otherag,
                      gwp_c_agsoils,
                      gwp_entericferm_ch4,
                      gwp_manureman_directn2o ,
                      gwp_manureman_indirectn2o,
                      gwp_manure_agsoils_n2o,
                      gwp_urinedung_agsoils_n2o,
                      gwp_manureman_ch4,
                      gwp_sewage_agsoils_n2o,
                      gwp_cropresidues_agsoils_n2o,
                      gwp_feed_imported) %>% 
    group_by(scenario, name, option, year, country, category_2, ag_sector, ghg) %>%
    summarise(gwp_t = sum(gwp_t), .groups = "drop") %>% 
    mutate(category_1 = "Agriculture") %>% 
    # Duplicate 2050 across all years
    bind_rows((.) %>% 
                filter(year == 2050) %>% 
                select(-year) %>% 
                crossing(year = 2051:2100))
 
  
  ## PEAT ----------------------------------
  print("Doing peat...")
  # Calculate peat x LCM areas
  peat_areas <- scenarios_areas[peat == 1 & lcm != "f.w"
                                , keyby = .(scenario, name, year, country, lcm)
                                , .(area = sum(area))] %>% 
    as_tibble() %>% 
    # Complete missing years (fill area with 0)
    complete(year, nesting(scenario, name, country, lcm), fill = list(area = 0)) %>%
    # Annualise (interpolate between 5 years)
    left_join(years, by = "year") %>%
    filter(year_ <= 2050) %>% 
    group_by(scenario, name, country, lcm) %>% 
    mutate(area = ifelse(year == year_, area, NA),
           area = zoo::na.approx(area)) %>% 
    ungroup() %>% 
    select(-year) %>% 
    rename(year = year_) 
  
  
  # Identify bog / fen as near-natural (existing) or rewetted (new)
  peat_areas <- peat_areas %>%
    # Get baseline area 
    filter(lcm %in% c("f.s", "b.g") & year == 2015) %>%
    mutate(area_existing = area) %>%
    select(-area, -year) %>%
    crossing(year = unique(peat_areas$year)) %>%
    # Get area of new bog/fen 
    left_join(peat_areas %>%
                filter(lcm %in% c("f.s", "b.g")),
              by = c("scenario", "name", "country", "lcm", "year")) %>%
    mutate(area_new = area - area_existing) %>%
    select(-area) %>%
    # Tidy
    gather(condition, area, -scenario, -name, -country, -lcm, -year) %>%
    mutate(condition = ifelse(condition == "area_existing", "Existing", "New (rewetted)")) %>%
    # Join with non-bog/fen peat (age = Existing)
    bind_rows(peat_areas %>%
                filter(!lcm %in% c("f.s", "b.g")) %>%
                mutate(lcm = ifelse(lcm %in% c("a.h_organic", "i.g_organic", "c.w_pinewood"), substr(lcm, 1, 3), lcm),
                       condition = ifelse(lcm %in% c("a.h_palud", "i.g_palud"), "Paludiculture", "Existing")) %>% 
                group_by(scenario, name, country, lcm, year, condition) %>% 
                summarise(area = sum(area), .groups = "drop")) %>%
    select(scenario, name, year, country, lcm, condition, area) 
  
  # Adjust bog & a.h EF according to scenario-specific within-land-cover restoration
  gwp_peat <- peat_areas %>%
    # Get all combinations of scenario/year/country, with ambition level
    select(scenario, name, year, country) %>% 
    unique() %>% 
    left_join(ambition_combos %>% 
                select(scenario, ambition_peat) %>% 
                rename(ambition = ambition_peat), 
              by = "scenario") %>%
    # Get emissions factors for b.g/a.h
    left_join(ghg_pars_peat$gwp_peat %>% 
                filter(lcm %in% c("b.g", "a.h")), 
              by = "country") %>%
    # Fix initial prop of rewetted, paludiculture & sustman to 0
    mutate(prop = ifelse(condition %in% c("Cropland (sustainable management)", "New (rewetted)"), 0, prop)) %>%
    # Get scenario params for each ambition level (crop and bog only)         
    left_join(scenario_params %>% 
                filter(par %in% c("peat_lowlandcrop_sustman", "peat_upland", "peat_extraction")) %>% 
                select(ambition, par, val) %>% 
                spread(par, val) %>% 
                set_names(c("ambition", "extraction", "sustman", "upland")), 
              by = "ambition") %>%
    # Make changes to cropland - swap exising for palud/sustman
    group_by(scenario, name, country, ghg, year, lcm) %>%
    mutate(prop = case_when(condition == "Cropland (sustainable management)" & year >= 2020  ~ sustman * (year - 2020) / 5,
                            TRUE ~ prop),
           prop = case_when(prop < 0 ~ 0,
                            TRUE ~ prop),
           prop = ifelse(condition == "Existing" & lcm == "a.h", 1 - prop[condition == "Cropland (sustainable management)"], 
                         prop)) %>%
    # Make changes to bog - swap extraction/modified for rewetted
    mutate(prop = case_when(condition == "Extraction" & year >= 2020 ~ prop - prop * extraction * (year - 2020) / 5,
                            condition == "Modified" & year >= 2020  ~ prop - prop * upland * (year - 2020) / 5,
                            TRUE ~ prop),
           prop = case_when(prop < 0 ~ 0,
                            TRUE ~ prop),
           prop = case_when(condition == "New (rewetted)" ~ 1 - sum(prop),
                            TRUE ~ prop)) %>%
    ungroup() %>%
    filter(prop != 0) %>% 
    # Calculated weighted mean across categories within LCM
    group_by(scenario, name, year, country, ghg, lcm) %>% 
    summarise(gwp_t = sum(gwp_t * prop), .groups = "drop") %>% 
    # All 'Existing' (i.e don't apply to land converted to bog)
    mutate(condition = "Existing") %>% 
    # Combine with other EFs  
    bind_rows(peat_areas %>% 
                select(scenario, name, year, country) %>% 
                unique() %>%
                # Get emissions factors for everything else
                left_join(ghg_pars_peat$gwp_peat %>% 
                            filter(lcm != "a.h" & !(lcm == "b.g" & condition != "New (rewetted)")), 
                          by = "country") %>% 
                select(-prop, -category)) 
  
  # Join with peat areas
  gwp_peat <- gwp_peat %>% 
    inner_join(peat_areas, by = c("scenario", "name", "year", "country", "condition", "lcm")) %>%
    # Duplicate 2050 for all years up to 2100 & 2020 for year 2015
    bind_rows((.) %>% 
                filter(year == 2050) %>% 
                select(-year) %>% 
                crossing(year = 2051:2100)) 
  
  # Add categories & summarise
  gwp_peat <- gwp_peat %>% 
    mutate(category_1 = "Peatland",
           category_2 = case_when(lcm %in% c("a.h", "i.g", "f.s", "a.h_organic", "a.h_palud", "i.g_organic", "i.g_palud") ~ "Lowland peat",
                                  lcm %in% c("b.g", "n.g", "c.g", "a.g", "h.r", "b.g") ~ "Upland peat",
                                  lcm %in% c("c.w", "b.w", "c.w_pinewood") ~ "Forested peat",
                                  TRUE ~ NA_character_)) %>% 
    # Summarise across land covers
    group_by(scenario, name, country, year, category_1, category_2, ghg) %>% 
    summarise(gwp_t = sum(gwp_t * area), .groups = "drop")
  
  
  
  ## INTERTIDAL ----------------------------------
  print("Doing intertidal...")
  # Calculate area changing to saltmarsh from anything else
  intertidal_new_ha <- scenarios_change_areas[lcm_to == "s.m" & lcm_from != "s.m"
                                              , keyby = .(scenario, name, year, country)
                                              , .(intertidal_new_ha = sum(area))] %>% 
    # To tbl
    as_tibble() %>% 
    # Annualise (divide changing area across 5 years) 
    mutate(year = year - 5) %>% 
    left_join(years, by = "year") %>%
    mutate(intertidal_new_ha = intertidal_new_ha / 5) %>%
    select(-year) %>% 
    rename(year = year_) %>%
    arrange(scenario, name, year, country) %>% 
    # Calculate cumulative area
    group_by(scenario, name, country) %>%
    mutate(cumul_intertidal_new_ha = cumsum(intertidal_new_ha)) %>%
    ungroup() %>% 
    select(-intertidal_new_ha)
  
  # Add years beyond 2050 (no further increase in cumulative area)
  intertidal_new_ha <- intertidal_new_ha %>% 
    filter(year == 2049) %>% # Final year
    select(-year) %>% 
    crossing(year = 2050:2100) %>% 
    bind_rows(intertidal_new_ha, .)
  
  # Combine existing and new areas with EFs
  gwp_intertidal <- bind_rows(ghg_pars_intertidal$intertidal_existing_ha %>%
                                # Cross with all scenarios (= existing intertidal habitat)
                                crossing(scenario_names) %>% 
                                filter(!(country != "England" & grepl("networks", name))) %>% 
                                # Add EF
                                crossing(filter(ghg_pars_intertidal$gwp_intertidal, category == "intertidal_existing")) %>%
                                # Calculate total net GWP for existing saltmarsh
                                group_by(year, scenario, name, country, category, ghg) %>% 
                                summarise(gwp_t = gwp * intertidal_existing_ha, .groups = "drop"),
                              intertidal_new_ha %>% 
                                # Add EF
                                crossing(ghg_pars_intertidal$gwp_intertidal %>% filter(category == "intertidal_new")) %>% 
                                # Calculate total net GWP for new saltmarsh
                                group_by(year, scenario, name, country, category, ghg) %>% 
                                summarise(gwp_t = gwp * cumul_intertidal_new_ha, .groups = "drop")) %>% 
    # Add categories - 2 levels
    mutate(category_2 = ifelse(category == "intertidal_existing", "Intertidal habitat (existing)", "Intertidal habitat (new)"),
           category_1 = "Intertidal habitat") %>% 
    select(scenario, country, name, year, category_1, category_2, ghg, gwp_t) 
  
  
  ## BIOENERGY INPUTS -----------------------------------------
  print("Doing bioenergy...")
  # Calculate bioenergy crop areas (on arable)
  bioenergy_areas <- scenarios_change_areas[lcm_to == "a.h_energy" & lcm_from != "a.h_energy"
                                            , keyby = .(scenario, name, year, country, lcm_to)
                                            , .(area = sum(area))] %>%
    as_tibble()
  
  if(nrow(bioenergy_areas) > 0){
    bioenergy_areas <- bioenergy_areas %>%
      # Add rows for 2015 & 2020 (area == 0)
      bind_rows((.) %>% 
                  select(-year, -area) %>%
                  unique() %>% 
                  crossing(year = c(2015, 2020)) %>% 
                  mutate(area = 0)) %>% 
      arrange(year) %>% 
      # Annualise (divide changing area across 5 years) 
      mutate(year = year - 5) %>% 
      left_join(years, by = "year") %>%
      mutate(area = area / 5) %>%
      select(-year) %>% 
      rename(year = year_) %>%
      # Divide between Miscanthus and SRC
      left_join(prop_bioenergycrop, by = "scenario") %>%
      mutate(bioenergy_crop = ifelse(bioenergy_crop == "a.h_energy_src", "SRC", "Miscanthus")) %>% 
      mutate(area = area * prop) %>%
      select(-prop) %>% 
      filter(area != 0) %>% 
      # Cross with all ages
      crossing(age = 1:100) %>%
      filter(age + year <= 2100) %>%
      mutate(year = year + age)
    
    # Join with EFs (fertiliser use only - soil and biomass dealt with under LUC)
    gwp_bioenergy <- bioenergy_areas %>%
      left_join(filter(ghg_pars_bioenergy$gwp_bioenergy, category_2 == "Bioenergy crops - fertiliser"), 
                by = c("bioenergy_crop", "age")) %>%
      group_by(scenario, name, year, country, ghg, category_1, category_2) %>%
      summarise(gwp_t = sum(gwp_t * area), .groups = "drop")
  } else{
    gwp_bioenergy <- NULL
  }
  
  
  
  ## WOODLAND -----------------------
  print("Doing woodland...")
  # Get area of existing, unchanged woodland (1 value per scenario, constant across all years)
  extant_wood_areas <- scenarios_areas[year == 2015 & lcm %in% c("b.w", "c.w", "c.w_pinewood")
                                       , keyby = .(name, scenario, country, lcm, yc_sbi, yc_sok, yc_sp, yc_ss)
                                       , .(area = sum(area))] %>% 
    as_tibble() %>% 
    # Cross with all years to 2050
    crossing(year = 2015:2050) %>% 
    # Join with area converted to other land-use
    left_join(scenarios_change_areas[lcm_from %in% c("b.w", "c.w", "c.w_pinewood") & lcm_from != lcm_to
                                     , keyby = .(name, scenario, year, country, lcm_from, yc_sbi, yc_sok, yc_sp, yc_ss)
                                     , .(area_lost = sum(area))
    ][, ':='(lcm = lcm_from,
             lcm_from = NULL)] %>% 
      as_tibble() %>% 
      # Annualise (divide changing area_lost across 5 years)
      mutate(year = year - 5) %>% 
      left_join(years, by = "year") %>%
      mutate(area_lost = area_lost / 5) %>%
      select(-year) %>% 
      rename(year = year_) %>%
      arrange(scenario, name, year, country),
    by = c("name", "scenario", "country", "year", "lcm", "yc_sbi", "yc_sok", "yc_sp", "yc_ss")) %>% 
    # Calculate (cumulative) lost area
    mutate(area_lost = ifelse(is.na(area_lost), 0, area_lost)) %>% 
    group_by_at(vars(-year, -area_lost)) %>% 
    arrange(year) %>% 
    mutate(area_lost = cumsum(area_lost)) %>% 
    ungroup() %>% 
    # Calculate remaining area of existing woodland
    mutate(area = area - area_lost) %>% 
    select(-area_lost) %>% 
    # Restrict to woodland remaining in 2050
    filter(year == 2050) %>% 
    select(-year) %>% 
    # Cross with all years to 2100
    crossing(year = 2015:2100) %>% 
    # Add woodland type
    left_join(tibble(lcm = c("b.w", "b.w", "c.w_pinewood", "c.w"),
                     type = c("Semi-natural broadleaved woodland (unmanaged)", "Managed broadleaved woodland", "Semi-natural coniferous woodland (unmanaged)", "Managed coniferous woodland"),
                     prop = c(0.8, 0.2, 1, 1)), 
              by = "lcm") %>% 
    mutate(area = area * prop) %>% 
    select(-prop) %>% 
    # Get matching yield class
    mutate(yc = case_when(type == "Semi-natural broadleaved woodland (unmanaged)" ~ yc_sok,
                          type == "Managed broadleaved woodland" ~ yc_sok,
                          type == "Semi-natural coniferous woodland (unmanaged)" ~ yc_sp,
                          type == "Managed coniferous woodland" ~ yc_ss),
           # Round down to nearest even number
           yc = floor(yc / 2) * 2,
           # Minimum allowed = lowest in WCC tables
           yc = case_when(type != "Managed coniferous woodland" & yc <= 4 ~ 4,
                          type == "Managed coniferous woodland" & yc <= 6 ~ 6,
                          TRUE ~ yc)) %>%
    # Sum area per woodland type/YC
    group_by(scenario, name, year, country, lcm, type, yc) %>% 
    summarise(area = sum(area), .groups = "drop") %>% 
    # Cross with age cohort distribution 
    left_join(ghg_pars_wood$existing_wood_age %>% filter(country == "England") %>% bind_rows((.) %>% filter(lcm == "c.w") %>% mutate(lcm = "c.w_pinewood")) %>% select(-country, -age),
              by = c("lcm")) %>%
    mutate(area = area * prop) %>% 
    mutate(age = year - cohort) %>% 
    select(-prop, -cohort)
  
  
  # Get area of newly created woodland
  new_wood_areas <- scenarios_change_areas[lcm_to %in% c("b.w", "c.w", "c.w_pinewood", "i.g_energy") & lcm_from != lcm_to
                                           , keyby = .(name, scenario, year, country, lcm_to, yc_sbi, yc_sok, yc_sp, yc_ss, natregen)
                                           , .(area = sum(area))
  ][, ':='(lcm = lcm_to,
           lcm_to = NULL)] %>% 
    as_tibble() %>% 
    # Annualise (divide changing area_lost across 5 years)
    mutate(year = year - 5) %>% 
    left_join(years, by = "year") %>%
    mutate(area = area / 5) %>%
    select(-year) %>% 
    rename(year = year_) %>%
    arrange(scenario, name, year, country) %>% 
    # Account for open ground - woodland establishment only on 85% of area
    left_join(ambition_combos %>% 
                select(scenario, ambition_wood) %>% 
                left_join(scenario_params %>% 
                            filter(par == "wood_openground"),
                          by = c("ambition_wood" = "ambition")) %>% 
                select(scenario, val),
              by = "scenario") %>% 
    mutate(area = ifelse(lcm == "i.g_energy", area, area * (1-val))) %>% 
    select(-val) %>% 
    # Add woodland type
    left_join(tibble(lcm = c("b.w", "b.w", "c.w_pinewood", "c.w", "i.g_energy"),
                     type = c("Semi-natural broadleaved woodland (unmanaged)", "Managed broadleaved woodland", "Semi-natural coniferous woodland (unmanaged)", "Managed coniferous woodland", "SRF"),
                     prop = c(0.8, 0.2, 1, 1, 1)), 
              by = "lcm") %>% 
    mutate(area = area * prop) %>% 
    select(-prop) %>% 
    # Get matching yield class
    mutate(yc = case_when(type == "Semi-natural broadleaved woodland (unmanaged)" ~ yc_sok,
                          type == "Managed broadleaved woodland" ~ yc_sok,
                          type == "Semi-natural coniferous woodland (unmanaged)" ~ yc_sp,
                          type == "Managed coniferous woodland" ~ yc_ss,
                          type == "SRF" ~ 12),
           # Round down to nearest even number
           yc = ifelse(type == "SRF", yc, floor(yc / 2) * 2)) %>%
    # Establishment - reduce yc for natregen (semi-natural woodland only)
    mutate(natregen = ifelse(type %in% c("Semi-natural broadleaved woodland (unmanaged)", "Semi-natural coniferous woodland (unmanaged)") & natregen == 1,
                             1, 
                             0),
           yc = ifelse(natregen == 1,
                       yc - 2, 
                       yc),
           # Minimum allowed = lowest in WCC tables
           yc = case_when(type != "Managed coniferous woodland" & yc <= 4 ~ 4,
                          type == "Managed coniferous woodland" & yc <= 6 ~ 6,
                          TRUE ~ yc)) %>% 
    # Sum area per woodland type/YC
    group_by(scenario, name, year, country, lcm, type, natregen, yc) %>% 
    summarise(area = sum(area), .groups = "drop") %>% 
    # Cross with age cohort distribution 
    crossing(age = seq(0, 200, by = 1)) %>% 
    mutate(year = year + age) %>% 
    filter(year >= 2015 & year <= 2100)
  
  
  # Get area of woodland converted to something else
  extinct_wood_areas <- scenarios_change_areas[lcm_from %in% c("b.w", "c.w") & lcm_from != lcm_to
                                               , keyby = .(name, scenario, year, country, lcm_from, yc_sbi, yc_sok, yc_sp, yc_ss)
                                               , .(area = sum(area))
  ][, ':='(lcm = lcm_from,
           lcm_from = NULL)] %>% 
    as_tibble() %>%
    # Annualise (divide changing area_lost across 5 years)
    mutate(year = year - 5) %>% 
    left_join(years, by = "year") %>%
    mutate(area = area / 5) %>%
    select(-year) %>% 
    rename(year = year_) %>%
    arrange(scenario, name, year, country) %>% 
    # Add woodland type
    left_join(tibble(lcm = c("b.w", "b.w", "c.w"),
                     type = c("Semi-natural broadleaved woodland (unmanaged)", "Managed broadleaved woodland", "Managed coniferous woodland"),
                     prop = c(0.8, 0.2, 1)), 
              by = "lcm") %>% 
    mutate(area = area * prop) %>% 
    select(-prop) %>% 
    # Get matching yield class
    mutate(yc = case_when(type == "Semi-natural broadleaved woodland (unmanaged)" ~ yc_sok,
                          type == "Managed broadleaved woodland" ~ yc_sok,
                          type == "Semi-natural coniferous woodland (unmanaged)" ~ yc_sp,
                          type == "Managed coniferous woodland" ~ yc_ss),
           # Round down to nearest even number
           yc = floor(yc / 2) * 2,
           # Minimum allowed = lowest in WCC tables
           yc = case_when(type != "Managed coniferous woodland" & yc <= 4 ~ 4,
                          type == "Managed coniferous woodland" & yc <= 6 ~ 6,
                          TRUE ~ yc)) %>%
    # Sum area per woodland type/YC
    group_by(scenario, name, year, country, lcm, type, yc) %>% 
    summarise(area = sum(area), .groups = "drop") %>% 
    # Get year felled
    rename(year_felled = year) %>% 
    crossing(year = 2015:2100) %>% 
    # Cross with age cohort distribution 
    left_join(ghg_pars_wood$existing_wood_age %>% filter(country == "England") %>% bind_rows((.) %>% filter(lcm == "c.w") %>% mutate(lcm = "c.w_pinewood")) %>% select(-country, -age),
              by = c("lcm")) %>%
    mutate(area = area * prop) %>% 
    mutate(age = year - cohort) %>%
    select(-prop) 
  
  
  # Combined areas with flux (note year_felled)
  gwp_woodland <- bind_rows(extant_wood_areas %>%
                              mutate(category_1 = "Woodland",
                                     category_2 = "Existing woodland",
                                     natregen = FALSE,
                                     year_felled = NA),
                            new_wood_areas %>%
                              mutate(category_1 = ifelse(type == "SRF", "Bioenergy crops", "Woodland"),
                                     category_2 = ifelse(type == "SRF", "SRF", "New woodland"),
                                     year_felled = NA),
                            extinct_wood_areas %>%
                              mutate(category_1 = "Woodland",
                                     category_2 = "Cleared woodland",
                                     natregen = FALSE,
                                     year_felled = year_felled - cohort)) %>%
    # Join with WCC seq data
    left_join(ghg_pars_wood$ghg_wood_ann, by = c("type", "yc", "natregen", "age" = "year", "year_felled")) %>% 
    # Summarise
    group_by(scenario, name, country, year, category_1, category_2) %>%
    summarise(gwp_t = -sum(area * annual_flux), .groups = "drop") %>%
    mutate(ghg = "CO2")
  
  
  
  ## AGROFORESTRY ----------------
  print("Doing agroforestry...")
  # Calculate areas to agroforestry
  agroforestry_areas <- scenarios_change_areas[lcm_to %chin% c("a.h_silvoa", "i.g_silvop", "a.h_organic_silvoa", "i.g_organic_silvop") & lcm_from %chin% c("a.h", "i.g", "a.h_organic", "i.g_organic")
                                               , keyby = .(name, country, scenario, year, lcm_to)
                                               , .(area = sum(area))
  ][, lcm := lcm_to
  ][, lcm_to := NULL] %>%
    as_tibble() %>% 
    # Annualise (divide changing area across 5 years)
    mutate(year = year - 5) %>% 
    left_join(years, by = "year") %>%
    mutate(area = area / 5) %>%
    select(-year) %>% 
    rename(year = year_) %>%
    arrange(scenario, name, year, country) 
  
  
  # Calculate GWP
  gwp_agroforestry <- bind_rows(
    # Trees
    agroforestry_areas %>% 
      left_join(ghg_pars_agroforestry %>% select(-contains("scrub")), by = "lcm") %>% 
      # Duplicate 2050-2100
      bind_rows((.) %>%
                  filter(year == 2050) %>%
                  select(-year) %>%
                  crossing(year = 2051:2100)) %>% 
      # Cross with all possible ages
      crossing(age = seq(0, 80, by = 1)) %>%
      mutate(year = year + age) %>%
      filter(year >= 2015 & year <= 2100) %>%
      # Join with WCC
      left_join(filter(ghg_pars_wood$ghg_wood_ann, is.na(year_felled)), by = c("type", "age" = "year")) %>%
      # Sum annual flux across woodland types
      group_by(scenario, name, country, year, type) %>%
      summarise(gwp_t = -sum(area * annual_flux * area_multiplier), .groups = "drop") %>% 
      mutate(category_1 = "Agroforestry") %>%
      rename(category_2 = type) %>% 
      mutate(ghg = "CO2"),
    # Soil
    agroforestry_areas %>% 
      # Assume arable -> silvoarable is halfway between arable -> grass and arable -> wood (country-specific)
      left_join(ghg_pars_luc$gwp_luc_soil %>% 
                  filter(lcm_from == "a.h" & lcm_to %in% c("i.g", "b.w")) %>% 
                  group_by(country, ghg, year) %>% 
                  summarise(gwp_t = mean(gwp_t), .groups = "drop") %>%
                  rename(age = year) %>% 
                  mutate(age = age - 1) %>% 
                  filter(age >= 0),
                by = "country") %>% 
      mutate(year = year + age) %>%
      filter(year <= 2100) %>% 
      left_join(select(ghg_pars_agroforestry, lcm, type, area_multiplier), by = "lcm") %>% 
      # Summarise and tidy
      group_by(name, scenario, country, year, ghg, type) %>% 
      summarise(gwp_t = sum(gwp_t * area * area_multiplier), .groups = "drop") %>% 
      mutate(category_1 = "Agroforestry") %>% 
      rename(category_2 = type)
  )
  
  
  
  ## WOOD PASTURE ----------------
  print("Doing wood pasture...")
  # Calculate areas to woodpasture
  woodpasture_areas <- scenarios_change_areas[lcm_to %chin% c("n.g_woodpa", "c.g_woodpa", "a.g_woodpa", "h.r_woodpa") & lcm_from != lcm_to
  ][, yc := ifelse(floor(yc_sok / 2) * 2 <= 4, 4, floor(yc_sok / 2) * 2)
  ][, keyby = .(name, country, scenario, year, lcm_to, yc)
    , .(area = sum(area))
  ][, lcm := lcm_to
  ][, lcm_to := NULL] %>%
    as_tibble() %>% 
    # Annualise (divide changing area across 5 years)
    mutate(year = year - 5) %>% 
    left_join(years, by = "year") %>%
    mutate(area = area / 5) %>%
    select(-year) %>% 
    rename(year = year_) %>%
    arrange(scenario, name, year, country) 
  
  # Calculate GWP
  gwp_woodpasture <- woodpasture_areas %>% 
    left_join(ghg_pars_agroforestry, by = "lcm") %>% 
    # Duplicate 2050-2100
    bind_rows((.) %>%
                filter(year == 2050) %>%
                select(-year) %>%
                crossing(year = 2051:2100)) %>% 
    # Cross with all possible ages
    crossing(age = seq(0, 80, by = 1)) %>%
    mutate(year = year + age) %>%
    filter(year >= 2015 & year <= 2100) %>% 
    # Join with WCC
    left_join(filter(ghg_pars_wood$ghg_wood_ann, is.na(year_felled)), by = c("type", "yc", "age" = "year")) %>%
    # Calculate scrub flux
    mutate(annual_flux_scrub = ifelse(age < scrub_years, scrub_c_density/scrub_years * scrub_footprint * 44/12, 0)) %>% 
    select(-scrub_footprint, -scrub_c_density, -scrub_years) %>% 
    # Sum annual flux across woodland types
    group_by(scenario, name, country, year) %>% 
    summarise(gwp_t = -sum(area * annual_flux * area_multiplier + area * annual_flux_scrub), .groups = "drop") %>% 
    mutate(category_1 = "Wood pasture",
           category_2 = "Wood pasture",
           ghg = "CO2") 
  
  
  ## HEDGES ------------------------------
  print("Doing hedges...")
  # Calculate length of new hedge p/a
  hedge_add <- ambition_combos %>%
    # Get ambition level
    select(name, scenario, ambition_hedge) %>%
    rename(ambition = ambition_hedge) %>%
    left_join(scenario_params %>%
                filter(grepl("hedge", par) & par != "hedge_stopyear") %>%
                rename('frac' = val) %>%
                select(frac, ambition) %>%
                left_join(scenario_params %>%
                            filter(par == "hedge_stopyear") %>%
                            select(ambition, val) %>%
                            rename('stopyear' = val),
                          by = "ambition"),
              by = "ambition") %>%
    # Cross with baseline hedge length
    crossing(ghg_pars_hedges$hedge_length) %>%
    filter(!(country != "England" & grepl("networks", scenario))) %>% 
    # Cross with all years 2015-2100
    crossing(year = seq(2015, 2100, by = 1)) %>%
    # Calculate additional km per year
    mutate(add = ifelse(year >= 2020 & year < stopyear, length * frac / 5, 0))
  
  gwp_hedges <- bind_rows(
    # Calculate annual biomass stock change 
    hedge_add %>%
      # Cross with 5-yarly GHG flux
      crossing(ghg_pars_hedges$gwp_hedges) %>%
      mutate(year = year + age) %>%
      filter(year <= 2100) %>%
      # Summarise and tidy
      group_by(name, scenario, country, year, ghg) %>%
      summarise(gwp_t = sum(gwp_t * add), .groups = "drop") %>%
      mutate(category_1 = "Hedgerows",
             category_2 = "Hedgerows - biomass"),
    # Calculate annual soil carbon change
    hedge_add %>% 
      # Assume arable -> hedge is halfway between arable -> grass and arable -> wood (country-specific)
      left_join(ghg_pars_luc$gwp_luc_soil %>%
                  filter(lcm_from == "a.h" & lcm_to %in% c("i.g", "b.w")) %>%
                  group_by(country, ghg, year) %>%
                  summarise(gwp_t = mean(gwp_t), .groups = "drop") %>%
                  rename(age = year) %>%
                  mutate(age = age - 1) %>%
                  filter(age >= 0),
                by = "country") %>%
      mutate(year = year + age) %>%
      filter(year <= 2100) %>%
      # Convert km to ha (1.5m wide)
      mutate(add = add * 0.0015 * 100) %>%
      # Summarise and tidy
      group_by(name, scenario, country, year, ghg) %>%
      summarise(gwp_t = sum(gwp_t * add * 0.5), .groups = "drop") %>% # Apply to 50% of area (arable only)
      mutate(category_1 = "Hedgerows",
             category_2 = "Hedgerows - soils"))
  
  
  
  
  ## COMBINE ----------------------------------
  print("Combining all...")
  # Combined all
  gwp_combined <- bind_rows(gwp_luc,
                            gwp_peat,
                            gwp_intertidal,
                            gwp_bioenergy,
                            gwp_woodland,
                            gwp_agroforestry,
                            gwp_woodpasture,
                            gwp_hedges) %>% 
    # Cross with all food gap closure options (no impact on non-ag 'sectors')
    left_join(unique(select(gwp_ag, scenario, option)),
              by = "scenario") %>% 
    bind_rows(gwp_ag)
  
  ## HWP ------------------------------
  # Bioenergy crops
  hwp_combined <- bioenergy_areas %>% 
    mutate(hwp = 12, # yield (odt per ha/year)
           hwp_type = "fuel (dedicated crops)") %>% 
    group_by(scenario, name, year, country, hwp_type) %>% 
    summarise(hwp = sum(area * hwp), .groups = "drop") 
  
  # Add SRF
  hwp_combined <- new_wood_areas %>% 
    filter(lcm == "i.g_energy") %>% 
    left_join(filter(ghg_pars_wood$ghg_wood_ann, is.na(year_felled)), by = c("type", "yc", "natregen", "age" = "year")) %>% 
    mutate(hwp_type = "fuel (dedicated crops)") %>% 
    group_by(scenario, name, year, country, hwp_type) %>% 
    summarise(hwp = sum(area * clearfell_stock) * 12/44 * 2,
              .groups = "drop") %>% 
    bind_rows(hwp_combined) 
  
  # Add Agroforestry
  hwp_combined <- agroforestry_areas %>% 
    left_join(ghg_pars_agroforestry %>% select(-contains("scrub")), by = "lcm") %>% 
    # Duplicate 2050-2100
    bind_rows((.) %>%
                filter(year == 2050) %>%
                select(-year) %>%
                crossing(year = 2051:2100)) %>% 
    # Cross with all possible ages
    crossing(age = seq(0, 80, by = 1)) %>%
    mutate(year = year + age) %>%
    filter(year >= 2015 & year <= 2100) %>% 
    left_join(filter(ghg_pars_wood$ghg_wood_ann, is.na(year_felled)), by = c("type", "age" = "year")) %>% 
    mutate(hwp_type = "fuel (agroforestry)") %>% 
    group_by(scenario, name, year, country, hwp_type) %>% 
    summarise(hwp = sum(area * clearfell_stock * area_multiplier) * 12/44 * 2,
              .groups = "drop") %>% 
    bind_rows(hwp_combined)
  
  # Add woodland (new/extant)
  hwp_combined <- bind_rows(extant_wood_areas %>%
                              mutate(natregen = 0,
                                     year_felled = NA),
                            new_wood_areas %>%
                              filter(lcm != "i.g_energy") %>% 
                              mutate(year_felled = NA),
                            extinct_wood_areas %>% 
                              mutate(natregen = 0,
                                     year_felled = year_felled - cohort)) %>%
    left_join(ghg_pars_wood$ghg_wood_ann, by = c("type", "yc", "natregen", "age" = "year", "year_felled")) %>%
    group_by(scenario, name, lcm, country, year) %>%
    summarise(hwp_clearfell = sum(area * clearfell_stock) * 12/44 * 2,
              hwp_thin = sum(area * removed_thin) * 12/44 * 2,
              .groups = "drop") %>%
    # Divide into HWP pools
    gather(activity, hwp, -year, -lcm, -country, -scenario, -name) %>%
    mutate(species = case_when(lcm == "b.w" ~ "SBI",
                               lcm == "c.w" ~ "SS",
                               lcm == "wind_c.w" ~ "SS",
                               lcm == "c.w_pinewood" ~ "SP"),
           activity = gsub("hwp_", "", activity)) %>%
    left_join(ghg_pars_wood$hwp_fracs, by = c("activity", "species")) %>%
    mutate(hwp = hwp * prop,
           hwp_type = paste0(hwp_type, " (forestry)")) %>%
    group_by(scenario, name, year, country, hwp_type) %>%
    summarise(hwp = sum(hwp), .groups = "drop") %>%
    bind_rows(hwp_combined)
  
  # Summarise
  hwp_combined <- hwp_combined %>% 
    group_by(scenario, name, year, country, hwp_type) %>% 
    summarise(hwp = sum(hwp), .groups = "drop")
  
  # Return
  return(list(gwp_combined = gwp_combined,
              hwp_combined = hwp_combined))
}

# RUN & SAVE ------------------------
gwp_combined <- do_ghg_fun(scenarios_areas, scenarios_change_areas,
                                scenario_params, ambition_combos,
                                ghg_pars_luc,
                                ghg_pars_ag, food_basic, food_gapclosed, pop, manure_systems, lowcarbonfarming,
                                ghg_pars_peat,
                                ghg_pars_intertidal,
                                ghg_pars_bioenergy,
                                ghg_pars_wood, 
                                ghg_pars_agroforestry,
                                ghg_pars_hedges)

save(gwp_combined, file = "rdata/ghg/gwp_combined_2023.RData")