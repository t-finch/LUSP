## INITS -------------------------------
source("setup.R")
load("rdata/food/food_params_2022.RData")
load("rdata/food/food_fun_2022.RData")


# TIDY SCENARIO AREAS --------------------------
load("rdata/scenarios/scenarios_areas_2022.RData") # Slow to load. Alternatives are quicker to load, but take up much more space!

# Filter out non-ag LCM
scenarios_areas <- scenarios_areas[!lcm %chin% c("b.t", "b.w", "c.w", "c.w_pinewood", "c.l", "f.w", "a.h_palud", "i.g_palud", "r.k", "a.h_energy", "i.g_energy")]

# Merge London and SE
scenarios_areas <- scenarios_areas[, nuts1 := ifelse(nuts1 %chin% c("South East (England)", "London"), "South East & London (England)", nuts1)]

# Sum across variables
scenarios_areas <- scenarios_areas[, keyby = .(scenario, name, year, country, lcm, nuts1, alc)
                                   , .(area = sum(area))]



# GET SCENARIO PROPERTIES --------------------------
# Ambition combinations
ambition_combos <- read_csv("data/ambition_combinations_new.csv", skip = 0)

# Scenario params
scenario_params <- read_csv("data/scenario_parameters_new.csv", skip = 1)



## ORGANIC ADJUSTMENTS --------------------------
# Add identifier for organic land
scenarios_areas <- scenarios_areas[, organic := lcm %chin% c("a.h_organic", "i.g_organic", "a.h_organic_silvoa", "i.g_organic_silvop")] 



## AGROFORESTRY ADJUSTMENTS --------------------------
# Identify silvoarable type - fruit or poplar
food_params$.silvoarable_type <- "poplar"
# food_params$.silvoarable_type <- "apples"


# Agroforestry footprint
agroforestry_footprint <- read_csv("data/food/agroforestry_footprint.csv", skip = 1) %>% select(-notes) %>% 
  filter(type != paste0("Silvoarable agroforestry (", food_params$.silvoarable_type, ")")) %>% 
  select(-type)

# Yield penality (shading from silvoarable) and area penality (equivalent to area removed from production)
# Join scenario areas with agroforestry footprint
scenarios_areas <- merge(scenarios_areas, as.data.table(agroforestry_footprint), all.x = TRUE, by = "lcm")
# Multiplier to 1 if NA
scenarios_areas <- scenarios_areas[, ':='(yield_multiplier = ifelse(is.na(yield_multiplier), 1, yield_multiplier),
                                          area_multiplier = ifelse(is.na(area_multiplier), 1, area_multiplier))] 




## HEDGE ADJUSTMENTS --------------------------
# Baseline hedge length
hedge_length <- read_csv("data/hedges/cs_hedge_length.csv", skip = 1) %>% select(-notes) %>% 
  summarise(length = sum(length))

# Calculate cumulative area of new hedge
hedge_areas <- ambition_combos %>%
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
  crossing(hedge_length) %>% 
  # Cross with all years 2020-2100
  crossing(year = 2015:2050) %>% 
  # Calculate additional km per 5-year period
  mutate(add = ifelse(year < stopyear & year >= 2020, length * frac / 5, 0)) %>%
  # Calculate cumulative ha of new hedge
  group_by(name, scenario) %>% 
  mutate(ha_removed = cumsum(add * 0.0015 * 100)) %>% # 0.0015 km wide (1.5m), 100 ha per km2 
  ungroup() %>% 
  select(-ambition, -frac, -stopyear, -length) 


# Reduce area of i.g and a.h
# Join scenario areas with hedge areas 
scenarios_areas <- merge(scenarios_areas, as.data.table(hedge_areas), all.x = TRUE, by = c("year", "name", "scenario"))
# Calculate proportion of a.h and i.g removed in each scenario year
scenarios_areas <- scenarios_areas[, c("prop_a.h_removed", "prop_i.g_removed") := .(ha_removed/2/sum(area[lcm %in% c("a.h", "a.h_organic")]), 
                                                                                    ha_removed/2/sum(area[lcm %in% c("i.g", "i.g_organic")]))
                                   , by = .(year, scenario)
                                   # Update areas
][, area := case_when(lcm %chin% c("a.h", "a.h_organic") ~ area * (1 - prop_a.h_removed),
                      lcm %chin% c("i.g", "i.g_organic") ~ area * (1 - prop_i.g_removed),
                      TRUE ~ area)
  # Removed unnecessary columns
][, c("add", "ha_removed", "prop_a.h_removed", "prop_i.g_removed") := NULL]



## IMPROVED PRACTICES --------------------------
# Read low carbon farming measures
lowcarbonfarming <- read_csv("data/lowcarbonfarming.csv", skip = 1)

# Tidy legume crop params
min_legumes <- lowcarbonfarming %>% 
  filter(measure == "legume_crops") %>% 
  select(-sruc_code, -scope, -impact) %>% 
  gather(ambition, prop, -measure, -start_year, -rollout_period) %>% 
  mutate(ambition = gsub("uptake_", "", ambition)) %>% 
  crossing(year = 2015:2050) %>% 
  # Applied to 50% of farmland
  mutate(min_legumes = 0.5 * case_when(year < start_year ~ 0, 
                                       year >= start_year & year < start_year + rollout_period ~ (year - start_year + 1) * prop / rollout_period,
                                       TRUE ~ prop)) %>% 
  left_join(ambition_combos %>% 
              select(scenario, ambition_lowcarbonfarming) %>% 
              rename(ambition = ambition_lowcarbonfarming),
            by = "ambition") %>% 
  select(-start_year, -rollout_period, -measure, -prop, -ambition) 

# Add to food_params
food_params$.min_legumes <- min_legumes



# Calculate n (0.0625-ha pixels) from area (ha)
scenarios_areas <- scenarios_areas[, n := area * 16]



# RUN - BASIC FOOD PRODUCTION (WITHOUT DEMAND/YIELD MEASURES) --------------------------
# Grab all combinations of scenario * year
food_results_basic <- scenarios_areas[, .N, keyby = c("scenario", "name", "year")
][, N := NULL] %>% 
  as_tibble() %>%
  mutate(i = 1:nrow(.)) %>%
  group_by(i) %>%
  nest() %>% 
  # Map food_production_fun across unique scenario-years
  mutate(food = purrr::map(data, ~food_production_fun(x = scenarios_areas[scenario == .$scenario & year == .$year], 
                                                      params = food_params,
                                                      waste = FALSE))) %>% 
  ungroup()


# Save
save(food_results_basic, file = "rdata/food/food_results_basic_2022.RData")



# RUN - AGROFORESTRY SENSITIVITY TEST (WITHOUT DEMAND/YIELD MEASURES) --------------------------
# Re-run for subset of scenarios with no agroforestry shading
scenarios_areas <- scenarios_areas[, yield_multiplier := 1]
food_results_silvoa_test1 <- scenarios_areas[, .N, keyby = c("scenario", "name", "year")
][, N := NULL] %>% 
  as_tibble() %>%
  filter(year == 2050 & nchar(scenario) == 1) %>% 
  mutate(i = 1:nrow(.)) %>%
  group_by(i) %>%
  nest() %>% 
  # Map food_production_fun across unique scenario-years
  mutate(food = purrr::map(data, ~food_production_fun(x = scenarios_areas[scenario == .$scenario & year == .$year], 
                                                      params = food_params,
                                                      waste = FALSE))) %>% 
  ungroup()

# Re-run for subset of scenarios with apple trees (rather than poplar)
food_params$.silvoarable_type <- "apples"
food_results_silvoa_test2 <- scenarios_areas[, .N, keyby = c("scenario", "name", "year")
][, N := NULL] %>% 
  as_tibble() %>%
  filter(year == 2050 & nchar(scenario) == 1) %>% 
  mutate(i = 1:nrow(.)) %>%
  group_by(i) %>%
  nest() %>% 
  # Map food_production_fun across unique scenario-years
  mutate(food = purrr::map(data, ~food_production_fun(x = scenarios_areas[scenario == .$scenario & year == .$year], 
                                                      params = food_params,
                                                      waste = FALSE))) %>% 
  ungroup()

# Compare
inner_join(food_results_basic %>%
             mutate(food_type_sum = purrr::map(food, ~.[[1]])) %>%
             select(-food) %>%
             unnest(cols = c(data, food_type_sum)) %>% 
             group_by(name, scenario, year) %>%
             summarise(energy = sum(energy), .groups = "drop") %>%
             ungroup(),
           food_results_silvoa_test2 %>%
             mutate(food_type_sum = purrr::map(food, ~.[[1]])) %>%
             select(-food) %>%
             unnest(cols = c(data, food_type_sum)) %>% 
             group_by(name, scenario, year) %>%
             summarise(energy = sum(energy), .groups = "drop") %>%
             ungroup(),
           by = c("name", "scenario", "year")) %>% 
  mutate((energy.y - energy.x)/energy.x*100)

inner_join(food_results_silvoa_test1 %>%
             mutate(food_type_sum = purrr::map(food, ~.[[1]])) %>%
             select(-food) %>%
             unnest(cols = c(data, food_type_sum)) %>% 
             # filter(product == "Fruit & vegetables") %>%
             group_by(name, scenario, year) %>%
             summarise(energy = sum(energy), .groups = "drop") %>%
             ungroup(),
           food_results_silvoa_test2 %>%
             mutate(food_type_sum = purrr::map(food, ~.[[1]])) %>%
             select(-food) %>%
             unnest(cols = c(data, food_type_sum)) %>% 
             # filter(product == "Fruit & vegetables") %>%
             group_by(name, scenario, year) %>%
             summarise(energy = sum(energy), .groups = "drop") %>%
             ungroup(),
           by = c("name", "scenario", "year")) %>% 
  mutate((energy.y - energy.x)/energy.x*100)




## BASELINE HUMAN POPULATION --------------------------
# Projections
# Use UN medium, which comes close (73.0m) to ONS projection of 72.4m by 2043 https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/nationalpopulationprojections/2018based
pop <- read_csv("data/population.csv", skip = 1) %>% select(-source, -notes) %>%
  filter(est %in% c("estimate", "medium variant")) %>%
  filter(!(year == 2020 & est == "estimate")) %>%
  select(year, pop) %>%
  mutate(pop_rel = pop/pop[year == 2015]) %>%
  filter(year %in% seq(2015, 2100, by = 5))


# RUN - 2050 FOOD PRODUCTION - NO YIELD GROWTH, FIXED WASTED, VARIABLE FEED CROP SUBSTITUTION --------------------
# For 2050 only
scenarios_areas_2050 <- copy(scenarios_areas)
scenarios_areas_2050 <- scenarios_areas_2050[year == 2050 & nchar(scenario) == 1]


# Run for each permutation
food_results_gapclosedsimp <- crossing(yield = 1,
                                       waste = 0.5,
                                       feed = 1 - seq(0, 0.5, length.out = 100),
                                       scenario = unique(scenarios_areas_2050$scenario),
                                       year = 2050) %>%
  # Add baseline row (waste = TRUE, so different to above)
  add_row(yield = 1,
          waste = 1,
          feed = 1, 
          scenario = "0",
          year = 2015) %>% 
  add_row(yield = 1,
          waste = 0.65,
          feed = 1, 
          scenario = "0",
          year = 2050) %>% 
  arrange(yield, -waste, -feed, scenario, year) %>% 
  mutate(i = 1:nrow(.)) %>%
  group_by(i) %>%
  nest() %>% 
  mutate(food = purrr::map(data, ~food_production_fun(x = scenarios_areas_2050[scenario == .$scenario
  ][, yield_multiplier := ifelse(organic, yield_multiplier, yield_multiplier * .$yield)],
  params = food_params,
  .waste_multiplier = .$waste,
  .feed_multiplier = .$feed,
  waste = TRUE))) %>% 
  ungroup()

save(food_results_gapclosedsimp, file = "rdata/food/food_gapclosed_simple.RData")



# RUN - 2050 FOOD PRODUCTION WITH PERMUTATIONS OF COMBINED DEMAND/YIELD MEASURES --------------------
# For 2050 only
scenarios_areas_2050 <- copy(scenarios_areas)
scenarios_areas_2050 <- scenarios_areas_2050[year == 2050]

# For extreme scenarios only: BAU and NbSOrganic
scenarios_areas_2050 <- scenarios_areas_2050[scenario %in% c("0", "5")]


# Run for finite combos of yield/waste/feed
food_results_allperms <- crossing(yield = 1 + seq(0, 0.34, length.out = 8),
                                  waste = 1 - seq(0, 0.7, length.out = 15),
                                  feed = 1 - seq(0, 0.5, length.out = 11),
                                  scenario = unique(scenarios_areas_2050$scenario),
                                  year = 2050) %>%
  # Add baseline row (waste = TRUE)
  add_row(yield = 1,
          waste = 1,
          feed = 1, 
          scenario = "0",
          year = 2015) %>% 
  arrange(yield, -waste, -feed, scenario, year) %>% 
  mutate(i = 1:nrow(.)) %>%
  group_by(i) %>%
  nest() %>% 
  mutate(food = purrr::map(data, ~food_production_fun(x = scenarios_areas_2050[scenario == .$scenario
  ][, yield_multiplier := ifelse(organic, yield_multiplier, yield_multiplier * .$yield)],
  params = food_params,
  .waste_multiplier = .$waste,
  .feed_multiplier = .$feed,
  waste = TRUE))) %>% 
  ungroup()



# Calculate food gap
food_results_allperms_gap <- food_results_allperms %>%
  # Grab first item 
  mutate(food_type_sum = purrr::map(food, ~.[[1]])) %>% 
  select(-food) %>% 
  unnest(cols = c(data, food_type_sum)) %>% 
  group_by(scenario, yield, waste, feed, year) %>% 
  summarise(energy = sum(energy)) %>% 
  ungroup() %>%
  mutate(base_energy = energy[year == 2015 & scenario == "0"]) %>% 
  filter(year != 2015) %>%
  mutate(rel_energy = (energy / pop$pop_rel[pop$year == 2050]) / base_energy) 



# Save
save(food_results_allperms, food_results_allperms_gap, file = "rdata/food/food_results_allperms_2022.RData")
load("rdata/food/food_results_allperms_2022.RData")




# IDENTIFY FEASIBLE SOLUTIONS TO FOOD GAP CLOSURE --------------------
# Function
foodgap_closure_fun <- function(data){
  mod <- gam(rel_energy ~ s(feed, waste, yield), data = data)
  
  newdata <- crossing(yield = 1 + seq(0, 0.34, length.out = 100),
                      waste = 1 - seq(0, 0.7, length.out = 100),
                      feed = 1 - seq(0, 0.5, length.out = 100))
  
  newdata$fit <- predict(mod, newdata)
  
  newdata <- newdata %>% 
    filter(fit > 0.99 & fit < 1.01) %>% 
    select(-fit)
  
  foodgap_options <- bind_rows(newdata %>%
                                 arrange(-yield, -(feed == 1 & waste == 1), abs((1-feed) - (1-waste))) %>% 
                                 slice(1) %>% 
                                 mutate(option = "Focus on yield growth"),
                               newdata %>%
                                 arrange(waste, -(feed == 1 & yield == 1), abs((1-feed) - (yield-1))) %>% 
                                 slice(1) %>% 
                                 mutate(option = "Focus on food waste reduction"),
                               newdata %>%
                                 arrange(feed, -(waste == 1 & yield == 1), abs((1-waste) - (yield-1))) %>% 
                                 slice(1) %>% 
                                 mutate(option = "Focus on feed crop substitution"),
                               newdata %>%
                                 arrange(yield, abs((1-feed) - (1-waste))) %>% 
                                 slice(1) %>% 
                                 mutate(option = "Focus on waste and feed crops"),
                               newdata %>%
                                 arrange(-(feed == 1 & waste == 1 & yield == 1), abs((1-feed) - (1-waste)) + abs((1-feed) - (yield-1)) + abs((1-waste) - (yield-1))) %>% 
                                 slice(1) %>% 
                                 mutate(option = "Focus on all three measures"))
  
  return(foodgap_options)
  
}

# Run
foodgap_closure_options <- food_results_allperms_gap %>%
  group_by(scenario) %>%
  nest() %>% 
  mutate(foodgap_options = purrr::map(data, ~foodgap_closure_fun(.))) %>% 
  select(-data) %>% 
  unnest(cols = c(foodgap_options)) %>% 
  ungroup() %>% 
  mutate(waste = 1-waste,
         feed = 1-feed,
         yield = yield-1) %>% 
  gather(var, val, -option, -scenario) %>% 
  mutate(option = fct_relevel(option, "Focus on food waste reduction", "Focus on feed crop substitution", "Focus on yield growth", "Focus on waste and feed crops"),
         var = fct_recode(var, "% food waste reduction" = 'waste', "% feed crop substitution" = 'feed', "% yield growth" = 'yield'),
         var = fct_relevel(var, "% food waste reduction", "% feed crop substitution", "% yield growth")) 

# Save
save(foodgap_closure_options, file = "rdata/food/foodgap_closure_options_2022.RData")
load("rdata/food/foodgap_closure_options_2022.RData")




# RUN - FOOD SUPPLY 2020-2050 FOR EACH FOOD GAP CLOSURE OPTION --------------------
food_results_gapclosed <- foodgap_closure_options %>%
  left_join(ambition_combos %>% select(scenario, name), by = "scenario") %>% 
  # # Just focus on 2/3 lever strategies
  # filter(option %in% c("Focus on waste and feed crops", "Focus on all three measures")) %>% 
  spread(var, val) %>% 
  rename('waste' = "% food waste reduction", 'feed' = "% feed crop substitution", 'yield' = "% yield growth") %>%
  # Calculate % action across levers
  mutate(action = case_when(option == "Focus on food waste reduction" ~ waste,
                            option == "Focus on feed crop substitution" ~ feed,
                            option == "Focus on yield growth" ~ yield,
                            option == "Focus on waste and feed crops" ~ (waste + feed) / 2,
                            option == "Focus on all three measures" ~ (waste + feed + yield) / 3),
         action = round(action*100, 0)) %>%
  # Calculate action per year
  crossing(year = seq(2015, 2050, by = 5)) %>% 
  mutate(waste = case_when(year == 2050 ~ 1 - waste,
                           year <= 2020 ~ 1,
                           TRUE ~ NA_real_),
         feed = case_when(year == 2050 ~ 1 - feed,
                          year <= 2020 ~ 1,
                          TRUE ~ NA_real_),
         yield = case_when(year == 2050 ~ yield + 1,
                           year <= 2020 ~ 1,
                           TRUE ~ NA_real_)) %>%
  group_by(option, scenario, name) %>% 
  mutate_at(vars(waste, feed, yield), list(~zoo::na.approx(.))) %>% # Linear interpolate
  ungroup() %>% 
  # Run food_production_fun
  mutate(i = 1:nrow(.)) %>% 
  group_by(i) %>%
  nest() %>% 
  mutate(food = purrr::map(data, ~food_production_fun(x = mutate(filter(scenarios_areas, scenario == .$scenario & year == .$year), 
                                                                 yield_multiplier = ifelse(organic, yield_multiplier, yield_multiplier * .$yield)),
                                                      params = food_params,
                                                      .waste_multiplier = .$waste,
                                                      .feed_multiplier = .$feed,
                                                      waste = FALSE))) 



# Save
save(food_results_gapclosed, file = "rdata/food/food_results_gapclosed_2022.RData")
