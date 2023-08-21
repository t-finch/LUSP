## INITS -------------------------------
source("setup.R")
load("rdata/food/food_params_2022.RData")
load("rdata/scenarios/scenarios_areas_2022.RData")


# Agroforestry footprint
agroforestry_footprint <- read_csv("data/food/agroforestry_footprint.csv", skip = 1) %>% select(-notes)

# Ambition combinations
ambition_combos <- read_csv("data/ambition_combinations_new.csv", skip = 0)

# Add unique spatial scenario code (letters)
ambition_combos <- ambition_combos %>%
  mutate(scenario_2 = letters[as.integer(as.factor(scenario))]) %>%
  select(name, scenario, scenario_2, everything())

# Read low carbon farming measures
lowcarbonfarming <- read_csv("data/lowcarbonfarming.csv", skip = 1)

# Tidy legume crop params
min_legumes <- lowcarbonfarming %>% 
  filter(measure == "legume_crops") %>% 
  select(-sruc_code, -scope, -impact) %>% 
  gather(ambition, prop, -measure, -start_year, -rollout_period) %>% 
  mutate(ambition = gsub("uptake_", "", ambition)) %>% 
  crossing(year = 2020:2050) %>% 
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

food_params$.silvoarable_type <- "poplar"


areas <- scenarios_areas %>%
  filter(year == 2050 & scenario == 8) %>%
  filter(lcm %in% c("a.h", "i.g", "n.g", "c.g", "a.g", "f.s", "h.r", "b.g", "s.m",
                    "a.h_silvoa", "i.g_silvop", "n.g_woodpa", "c.g_woodpa", "a.g_woodpa", "h.r_woodpa",
                    "a.h_organic", "i.g_organic", "a.h_organic_silvoa", "i.g_organic_silvop")) %>%
  group_by(scenario, year, country, nuts1, alc, lcm) %>%
  summarise(area = sum(area),
            n = area * 16) %>%
  ungroup() %>%
  # Add identifier for organic land
  mutate(organic = lcm %in% c("a.h_organic", "i.g_organic", "a.h_organic_silvoa", "i.g_organic_silvop")) %>%
  # Agroforestry
  left_join(agroforestry_footprint, by = "lcm") %>%
  mutate(yield_multiplier = ifelse(is.na(yield_multiplier), 1, yield_multiplier),
         area_multiplier = ifelse(is.na(area_multiplier), 1, area_multiplier)) %>% 
  select(-type)


# SPECIFY PRODUCTION FUNCTION ---------------------
food_production_fun <- function(x, params, .waste_multiplier = 1, .feed_multiplier = 1, .name = NA, waste = FALSE){
  options(dplyr.summarise.inform = FALSE)
  
  ## Read parameters ----------------
  # Food production per pixel (tonnes, area, energy, protein) 
  .food <- params$.food %>% filter(area_per_n > 0)
  # Feed production per pixel (MJ)
  .feed <- params$.feed
  # Fodder production per pixel (tonnes, area)
  .fodder <- params$.fodder
  # Nutritional value of feed crops to each animal type
  .feed_nutrition <- params$.feed_nutrition
  # Average nutritional value of each feed crop
  .feed_mj <- params$.feed_mj
  # Estimated trade balance of feed
  .frac_imported <- params$.frac_imported
  # Fraction used for feed/food
  .utilization <- params$.utilization
  # Fraction of a.h and i.g as temp grass
  .tempgrass_area <- params$.tempgrass_area
  # Grass production per pixel (area, unitless relative yield)
  .grass_prod <- params$.grass_prod
  # t grass per t meat
  .grass_req <- params$.grass_req
  # t fodder per t meat
  .fodder_req <- params$.fodder_req
  # mj feed per t meat
  .feed_req <- params$.feed_req
  # Distribution of grazing stock across census units
  .ruminant_props <- params$.ruminant_props
  # Conversion factor for grazing stock
  .meat_factor <- params$.meat_factor
  # Non-grazing stock feed allocation
  .nonruminant_feed_ratios <- params$.nonruminant_feed_ratios
  # Nutritional value of livetock products
  .livestock_food <- params$.livestock_food
  # Fraction wasted
  .waste <- params$.waste
  
  # Food from silvoarable
  if(params$.silvoarable_type == "apples"){
    .agroforestry_food <- params$.agroforestry_food
  } else{
    .agroforestry_food <- params$.agroforestry_food %>% 
      mutate(prod_per_n = 0,
             area_per_n = 0)
  }
  
  # Minumum legume prop
  if(length(params$.min_legumes) <= 1){
    .min_legumes <- 0
  } else{
    .min_legumes <- (params$.min_legumes %>% 
      filter(year == unique(x$year) & scenario == unique(x$scenario)))$min_legumes
  }
  
  # Add grass-fed params
  .livestock_food <- .livestock_food %>% 
    bind_rows((.) %>% 
                filter(product %in% c("Beef", "Sheep", "Milk")) %>% 
                mutate(product = paste0(product, " (grass-fed)")))
  
  .waste <- .waste %>% 
    bind_rows((.) %>% 
                filter(product %in% c("Beef", "Sheep", "Milk")) %>% 
                mutate(product = paste0(product, " (grass-fed)")))
  
  .feed_nutrition <- .feed_nutrition %>%
    bind_rows((.) %>% 
                filter(animal %in% c("Beef", "Sheep", "Milk") & crop == "Grass") %>% 
                mutate(animal = paste0(animal, " (grass-fed)")))
  
  
  ## Adjustments ---------------
  # Adjust waste
  .waste <- .waste %>%
    mutate(wasted = wasted * .waste_multiplier)
  
  # Adjust utilization
  .utilization <- .utilization %>%
    group_by(crop, crop_fine, grade) %>% 
    mutate(sum = sum(prop_ut)) %>% 
    mutate(prop_ut = ifelse(fraction == "Feed" & grade == "high", prop_ut * .feed_multiplier, prop_ut),
           prop_ut = ifelse(fraction == "Food" & grade == "high", sum - prop_ut[fraction == "Feed" & grade == "high"], prop_ut)) %>%
    ungroup() %>% 
    select(-sum) 
  
  
  ## Grassland ---------------
  # Calculate grassland areas from land use areas
  grass_areas <- x %>%
    filter(lcm %in% c("a.h", "i.g", "n.g", "c.g", "a.g", "f.s", "h.r", "b.g", "s.m",
                      "n.g_new", "c.g_new", "a.g_new", "h.r_new",
                      "a.h_silvoa", "i.g_silvop", "n.g_woodpa", "c.g_woodpa", "a.g_woodpa", "h.r_woodpa", "h.r_woodpa",
                      "a.h_organic_silvoa", "i.g_organic_silvop", "a.h_organic", "i.g_organic")) %>%
    mutate(lcm_simple = substr(lcm, 1, 3)) %>% 
    left_join(.tempgrass_area, by = c("nuts1", "lcm_simple" = "lcm", "organic")) %>% 
    mutate(grass_type = ifelse(is.na(prop), "rough", grass_type),
           prop = ifelse(is.na(prop), 1, prop)) %>% 
    group_by(country, nuts1, lcm, lcm_simple, grass_type, organic, yield_multiplier) %>% 
    summarise(area = sum(area * prop * area_multiplier),
              n = sum(n * area_multiplier),
              .groups = "drop") %>% 
    filter(area != 0)
  
  # Calculate grass production - per nuts1 region
  grass_per_nuts1 <- grass_areas %>%
    # Deal with new SNG/woodpa separately (grass-fed)
    filter(!lcm %in% c("a.g_woodpa", "n.g_woodpa", "c.g_woodpa", "n.g_new", "c.g_new", "a.g_new", "h.r_new")) %>% 
    filter(grass_type != "temp_new") %>% # Because increased temp grass in a.h already acounted for in .grass_prod
    left_join(.grass_prod, by = c("country", "nuts1", "lcm_simple" = "lcm", "grass_type", "organic")) %>%
    mutate(grass_type = ifelse(grass_type == "rough", "rough", "improved")) %>% 
    group_by(country, nuts1, grass_type) %>%
    # Account for yield and area
    summarise(prod = sum(yield_per_n * n * yield_multiplier),
              .groups = "drop")
  
  ## Ruminant production ---------------
  # Ruminant production from available grass
  ruminant_production_per_country <- grass_per_nuts1 %>% 
    inner_join(.ruminant_props, by = c("nuts1", "grass_type")) %>% 
    left_join(.meat_factor, by = "product") %>%
    group_by(country, product) %>% 
    summarise(production = sum(prod * prop * meat_factor),
              .groups = "drop")
  
  
  # Ruminant production from grass-fed systems
  if(sum(grass_areas$lcm %in% c("a.g_woodpa", "n.g_woodpa", "c.g_woodpa", "n.g_new", "c.g_new", "a.g_new", "h.r_new")) > 0){
    ruminant_production_per_country <- grass_areas %>%
      # Grass produced from new SNG/woodpa (grass-fed)
      filter(lcm %in% c("a.g_woodpa", "n.g_woodpa", "c.g_woodpa", "n.g_new", "c.g_new", "a.g_new", "h.r_new")) %>% 
      left_join(.grass_prod, by = c("country", "nuts1", "lcm_simple" = "lcm", "grass_type", "organic")) %>%
      mutate(grass_type = ifelse(grass_type == "rough", "rough", "improved")) %>% 
      group_by(country, nuts1, grass_type) %>%
      # Account for yield and area
      summarise(prod = sum(yield_per_n * n * yield_multiplier),
                .groups = "drop") %>% 
      # Convert to meat
      inner_join(.ruminant_props, by = c("nuts1", "grass_type")) %>% 
      left_join(.meat_factor, by = "product") %>%
      left_join(.grass_req %>% 
                  mutate(grass_fed = ifelse(grepl("grass-fed", product), "grass_fed", "default"),
                         product = sub("..grass.fed.", "", product)) %>% 
                  spread(grass_fed, grass_req) %>% 
                  mutate(grass_fed_ratio = default/grass_fed) %>% 
                  select(product, grass_fed_ratio),
                by = "product") %>%
      mutate(product = paste0(product, " (grass-fed)")) %>% 
      # Sum across countries
      group_by(country, product) %>% 
      summarise(production = sum(prod * prop * meat_factor * grass_fed_ratio),
                .groups = "drop") %>% 
      bind_rows(ruminant_production_per_country, .)  
  }
  
  # Sum across countries
  ruminant_production <- ruminant_production_per_country %>%
    group_by(product) %>% 
    summarise(production = sum(production),
              .groups = "drop") 
  
  
  
  ## Fodder crops ---------------
  # Calculate arable areas from land use map
  arable_areas <- x %>%
    filter(lcm %in% c("a.h", "a.h_silvoa", "a.h_organic", "a.h_organic_silvoa")) %>%
    mutate(lcm = "a.h") %>% 
    group_by(country, nuts1, alc, lcm, organic, yield_multiplier) %>% 
    summarise(area = sum(area * area_multiplier),
              n = sum(n * area_multiplier),
              .groups = "drop")
  
  # Calculate fodder production - per country
  fodder_per_country <- arable_areas %>%
    left_join(.fodder, by = c("country", "nuts1", "alc", "organic")) %>% 
    group_by(crop_group, country) %>% 
    # Account for yield and area
    summarise(prod = sum(prod_per_n * n * yield_multiplier),
              .groups = "drop") 
  
  # Calculate fodder production - total
  fodder_prod <- fodder_per_country %>% 
    group_by(crop_group) %>% 
    summarise(prod = sum(prod),
              .groups = "drop")
  
  # Fodder requirements of ruminants
  ruminant_fodder <- .fodder_req %>%
    inner_join(ruminant_production, by = "product") %>% 
    mutate(consumption = production * fodder_req) %>% 
    group_by(crop_group) %>% 
    summarise(consumption = sum(consumption),
              .groups = "drop")
  
  # Fodder surplus
  fodder_surplus <- fodder_prod %>% 
    left_join(ruminant_fodder, by = "crop_group") %>% 
    mutate(surplus = round(prod - consumption)) %>% 
    filter(surplus != 0)
  
  # If fodder surplus, replace with other arable crops
  if(nrow(fodder_surplus) > 0){
    # Calculate area spared from fodder production
    spare_fodder <- arable_areas %>%
      # Area and production of fodder crops
      left_join(.fodder, by = c("country", "nuts1", "alc", "organic")) %>% 
      group_by(country, nuts1, alc, crop_group, organic) %>% 
      summarise(prod = sum(n * prod_per_n * yield_multiplier),
                area = sum(n * area_per_n),
                .groups = "drop") %>% 
      # Calculate spare area per land type
      left_join(fodder_surplus %>% select(crop_group, surplus), by = c("crop_group")) %>% 
      group_by(crop_group) %>% 
      mutate(surplus_prod = surplus * prod / sum(prod),
             spare_area = surplus_prod / (prod / area)) %>%
      ungroup() %>% 
      mutate(frac_spare = spare_area / area)
    
    # Update .fodder
    .fodder <- .fodder %>%
      left_join(spare_fodder %>% select(country, nuts1, alc, crop_group, frac_spare), by = c("country", "nuts1", "alc", "crop_group")) %>% 
      # Reduce fodder area/prod
      mutate(area_per_n = area_per_n * (1 - frac_spare),
             prod_per_n = prod_per_n * (1 - frac_spare)) %>% 
      select(-frac_spare) 
    
    # Calculate fraction of arable land freed for other crops (but NOT temp grass)
    spare_fodder <- spare_fodder %>%
      group_by(country, nuts1, alc, organic) %>% 
      summarise(spare_area = sum(spare_area),
                .groups = "drop") %>% 
      # Calculate fraction of all arable land spared
      left_join(arable_areas %>% 
                  left_join(.food, by = c("country", "nuts1", "alc", "organic")) %>% 
                  group_by(country, nuts1, alc, organic) %>% 
                  summarise(area = sum(n * area_per_n),
                            .groups = "drop"), 
                by = c("country", "nuts1", "alc", "organic")) %>%
      mutate(frac_arable_spared = spare_area / area) %>%
      select(-spare_area, -area)
    
    # Update .food
    .food <- .food %>%
      left_join(spare_fodder, by = c("country", "nuts1", "alc", "organic")) %>%
      # Increase food area/prod
      mutate(area_per_n = area_per_n * (1 + frac_arable_spared),
             prod_per_n = prod_per_n * (1 + frac_arable_spared)) %>% 
      select(-frac_arable_spared)
    
    # Update .feed
    .feed <- .feed %>%
      left_join(spare_fodder, by = c("country", "nuts1", "alc", "organic")) %>%
      # Increase food area/prod
      mutate(mj_per_n = mj_per_n * (1 + frac_arable_spared)) %>% 
      select(-frac_arable_spared)
    
    # Recalculate fodder production - per country
    fodder_per_country <- arable_areas %>%
      left_join(.fodder, by = c("country", "nuts1", "alc", "organic")) %>% 
      group_by(crop_group, country) %>% 
      # Account for yield and area
      summarise(prod = sum(prod_per_n * n * yield_multiplier),
                .groups = "drop")
    
    # Recalculate fodder production - total
    fodder_prod <- fodder_per_country %>% 
      group_by(crop_group) %>% 
      summarise(prod = sum(prod),
                .groups = "drop") 
  }
  
  
  ## Increase legume area (low carbon farming) -------------------
  # Calculate ha legumes per pixel to add to meet minimum req. - non-organic land only
  add_legumes <- .food %>%
    filter(crop_group == "Legumes" & !organic) %>%
    group_by(country, nuts1, alc) %>%
    summarise(add_legumes = sum(0.0625 * (.min_legumes - sum(area_per_n) / 0.0625)),
              .groups = "drop") %>%
    mutate(add_legumes = ifelse(add_legumes < 0, 0, add_legumes))
  
  
  # Scale up area_per_n for Legumes
  food_legumes <- full_join(add_legumes,
                            .food %>%
                              filter(crop_group == "Legumes" & !organic) %>%
                              group_by(country, nuts1, alc, crop_fine) %>%
                              mutate(prop = sum(area_per_n)) %>%
                              group_by(country, nuts1, alc) %>%
                              mutate(prop = prop / sum(prop)) %>%
                              ungroup(),
                            by = c("country", "nuts1", "alc")) %>%
    mutate(multiplier = (area_per_n + add_legumes * prop) / area_per_n,
           prod_per_n = prod_per_n * multiplier,
           area_per_n = area_per_n * multiplier) %>%
    select(-add_legumes, -prop)
  
  # Scale down area_per_n for Cereal & Oilseeds
  food_nonlegumes <- full_join(add_legumes,
                               .food %>%
                                 filter(crop_group %in% c("Cereal", "Oilseeds") & !organic) %>%
                                 group_by(country, nuts1, alc, crop_fine) %>%
                                 mutate(prop = sum(area_per_n)) %>%
                                 group_by(country, nuts1, alc) %>%
                                 mutate(prop = prop / sum(prop)) %>%
                                 ungroup(),
                               by = c("country", "nuts1", "alc")) %>%
    mutate(multiplier = (area_per_n - add_legumes * prop) / area_per_n,
           prod_per_n = prod_per_n * multiplier,
           area_per_n = area_per_n * multiplier) %>%
    select(-add_legumes, -prop) %>%
    # No change to organic / other crops
    bind_rows(.food %>%
                filter((!crop_group %in% c("Cereal", "Oilseeds", "Legumes")) | organic) %>%
                mutate(multiplier = 1))
  
  # Update .food
  .food <- bind_rows(food_legumes,
                     food_nonlegumes) %>%
    select(-multiplier)
  
  # Update .feed
  .feed <- .feed %>%
    left_join(bind_rows(food_legumes,
                        food_nonlegumes) %>%
                select(-prod_per_n, -area_per_n, -energy, -protein),
              by = c("country", "nuts1", "alc", "grade", "organic", "crop_fine", "crop", "crop_group")) %>%
    mutate(mj_per_n = ifelse(is.na(multiplier), mj_per_n, mj_per_n * multiplier)) %>%
    select(-multiplier)
  
  
  
  ## Food from arable land -------------------
  # Calculate total food production - per country
  food_per_country <- arable_areas %>%
    left_join(.food, by = c("country", "nuts1", "alc", "organic")) %>% 
    left_join(.utilization %>% filter(fraction == "Food") %>% select(-fraction), by = c("crop_fine", "crop", "grade")) %>% 
    group_by(crop_group, country) %>% 
    # Account for nutrition, yield, area & utilisation
    summarise(tonnage = sum(prod_per_n * n * yield_multiplier, na.rm = TRUE),
              energy = sum(energy * prod_per_n * n * yield_multiplier * prop_ut, na.rm = TRUE),
              protein = sum(protein * prod_per_n * n * yield_multiplier * prop_ut, na.rm = TRUE),
              .groups = "drop") %>% 
    rename('food' = crop_group) %>% 
    filter(energy != 0)
  
  # Calculate production from silvoarable - assume none for feed
  food_per_country <- x %>%
    filter(lcm %in% c("a.h_silvoa", "a.h_organic_silvoa")) %>%
    group_by(country, nuts1, lcm) %>% 
    summarise(n = sum(n)) %>%
    ungroup() %>%  
    left_join(.agroforestry_food, by = "lcm") %>% 
    group_by(crop_group, country) %>% 
    # Account for nutrition, yield, area & utilisation
    summarise(tonnage = sum(prod_per_n * n, na.rm = TRUE),
              energy = sum(energy * prod_per_n * n, na.rm = TRUE),
              protein = sum(protein * prod_per_n * n, na.rm = TRUE),
              .groups = "drop") %>% 
    rename('food' = crop_group) %>% 
    # Bind with field production 
    bind_rows(food_per_country, .) %>% 
    # Re-summarise
    group_by(food, country) %>% 
    summarise_all(sum) %>% 
    ungroup()
  
  # Calculate food production - total
  food_prod <- food_per_country %>%
    select(-country) %>% 
    group_by(food) %>% 
    summarise_all(sum) %>% 
    ungroup()
  
  
  ## Feed from arable land -------------------
  # Calculate total feed production - per country
  feed_per_country <- arable_areas %>%
    left_join(.feed, by = c("country", "nuts1", "alc", "organic")) %>% 
    filter(!is.na(mj_per_n)) %>% 
    left_join(.utilization %>% filter(fraction == "Feed") %>% select(-fraction), by = c("crop_fine", "crop", "grade")) %>% 
    group_by(crop_group, country) %>%
    # Account for yield, area and utilisation
    summarise(mj = sum(mj_per_n * n * yield_multiplier * prop_ut, na.rm = TRUE),
              .groups = "drop") %>% 
    filter(mj != 0)
  
  # Calculate feed production - total
  feed_prod <- feed_per_country %>% 
    summarise(mj = sum(mj)) 
  
  
  # Feed requirements of ruminants raised on grass
  ruminant_feed <- .feed_req %>%
    inner_join(ruminant_production, by = "product") %>% 
    mutate(consumption = production * feed_req_mj) %>% 
    select(product, consumption)
  
  # Feed produced, imported & surplus
  feed_surplus <- feed_prod %>%
    rename('mj_produced' = mj) %>% 
    mutate(mj_imported = mj_produced * .frac_imported$frac_imported,
           # Ensure feed imports do not exceed absolute baseline imports 
           mj_imported = ifelse(mj_imported > .frac_imported$mj_imported, .frac_imported$mj_imported, mj_imported),
           mj_surplus = mj_produced + mj_imported - sum(ruminant_feed$consumption))
  
  # Non-ruminant production from surplus feed
  if(feed_surplus$mj_surplus > 0){
    nonruminant_production <- feed_surplus %>% 
      crossing(.nonruminant_feed_ratios) %>%
      left_join(.feed_req, by = "product") %>% 
      mutate(production = (mj_surplus * prop) / feed_req_mj) %>% 
      select(product, production)
  } else{
    nonruminant_production <- tibble(product = c("Poultry", "Pig", "Eggs"),
                                     production = 0)  
  }
  
  
  
  ## Total livestock -------------------
  # Total livestock production - ruminants and nonruminants
  livestock_production_per_country <- bind_rows(ruminant_production_per_country %>%
                                                  # mutate(product = gsub("..grass.fed.", "", product)) %>%
                                                  group_by(country, product) %>%
                                                  summarise(production = sum(production),
                                                            .groups = "drop"),
                                                # Allocate Pigs, Poultry & Eggs according to cereal feed production
                                                feed_per_country %>% 
                                                  filter(crop_group == "Cereal") %>% 
                                                  mutate(prop = mj / sum(mj)) %>% 
                                                  crossing(nonruminant_production %>% filter(product %in% c("Poultry", "Pig", "Eggs"))) %>% 
                                                  mutate(production = production * prop) %>%
                                                  select(product, country, production)) %>% 
    left_join(.livestock_food, by = 'product') %>% 
    # Account for nutrition
    mutate(energy = (production * 1000) * (energy / 1000000), # kcal/kg to million kcal/t
           protein = (production * 1000) * (protein / 1000000)) %>% # g/kg to tonnes/t 
    rename('tonnage' = production)
  
  
  
  # Sum across countries
  livestock_production <- livestock_production_per_country %>% 
    group_by(product) %>% 
    summarise(tonnage = sum(tonnage),
              energy = sum(energy),
              protein = sum(protein),
              .groups = "drop")
  
  
  ## Summarise total food output ---------------
  # Sum production of each product, accounting for waste
  food_type_sum <- bind_rows(food_per_country %>%
                               rename('product' = food),
                             livestock_production_per_country) %>%
    arrange(product, country) 
  
  # Account for waste
  if(waste){  
    food_type_sum <- food_type_sum %>% 
      left_join(.waste, by = 'product') %>% 
      mutate(energy = energy * (1 - wasted),
             protein = protein * (1 - wasted)) %>%
      select(-wasted) 
  }
  
  
  ## Output for GHG calcs ---------------
  # Crop areas - for N rate
  crop_areas <- bind_rows(arable_areas %>%
                            left_join(.food, by = c("country", "nuts1", "alc", "organic")) %>%
                            mutate(crop_fine = ifelse(crop == "Horticultural crops", crop, crop_fine)) %>%
                            group_by(country, crop_fine, organic) %>%
                            summarise(area = sum(n * area_per_n),
                                      tonnage = sum(n * prod_per_n),
                                      .groups = "drop"),
                          # Fodder crops
                          arable_areas %>% 
                            left_join(.fodder, by = c("country", "nuts1", "alc", "organic")) %>% 
                            group_by(country, crop_group, organic) %>%
                            summarise(area = sum(n * area_per_n),
                                      tonnage = sum(n * prod_per_n),
                                      .groups = "drop") %>%
                            rename('crop_fine' = crop_group),
                          # Also improved (temp/perm) grass
                          grass_areas %>% 
                            filter(lcm %in% c("i.g", "a.h")) %>% 
                            mutate(crop_fine = case_when(grass_type == "perm" ~ "Permanent grass",
                                                         grass_type == "temp" ~ "Temporary grass",
                                                         grass_type == "temp_new" ~ "Temporary grass (new ley)")) %>%
                            # mutate(crop_fine = ifelse(grass_type == "perm", "Permanent grass", "Temporary grass")) %>%
                            group_by(country, crop_fine, organic) %>% 
                            summarise(area = sum(area),
                                      .groups = "drop"))
  
  # Cropped area
  cropped_area <- arable_areas %>%
    group_by(country, organic) %>%
    summarise(cropped_area = sum(area),
              .groups = "drop")  
  
  
  # GE, ME & CP content of feed consumed by each livestock type
  # Grass consumed
  grass_consumed <- .grass_req %>%
    inner_join(ruminant_production_per_country, by = "product") %>% 
    mutate(crop = "Grass") %>% 
    # product = gsub("..grass.fed.", "", product)) %>%
    group_by(country, crop, product) %>% 
    summarise(consumption = sum(production * grass_req),
              .groups = "drop")
  
  # Fodder consumed
  fodder_consumed <- .fodder_req %>%
    inner_join(ruminant_production_per_country, by = "product") %>% 
    rename('crop' = crop_group) %>% 
    group_by(country, crop, product) %>% 
    summarise(consumption = sum(production * fodder_req),
              .groups = "drop")
  
  # Feed produced
  feed_produced <- arable_areas %>%
    left_join(.feed, by = c("country", "nuts1", "alc", "organic")) %>% 
    left_join(.utilization %>% filter(fraction == "Feed") %>% select(-fraction), by = c("crop_fine", "crop", "grade")) %>% 
    group_by(crop) %>% 
    summarise(mj_produced = sum(mj_per_n * n * yield_multiplier * prop_ut, na.rm = TRUE),
              .groups = "drop") %>% 
    filter(mj_produced != 0) %>% 
    group_by(crop) %>% 
    summarise(mj_produced = sum(mj_produced),
              .groups = "drop")
  
  # Feed consumed (re-do `livestock_production_per_country`, keeping default and grass-fed separate)
  feed_consumed <- bind_rows(ruminant_production_per_country,
                             feed_per_country %>% 
                               filter(crop_group == "Cereal") %>% 
                               mutate(prop = mj / sum(mj)) %>% 
                               crossing(nonruminant_production %>% filter(product %in% c("Poultry", "Pig", "Eggs"))) %>% 
                               mutate(production = production * prop) %>%
                               select(product, country, production)) %>%
    rename('tonnage' = production) %>% 
    inner_join(.feed_req, by = "product") %>% 
    mutate(mj_consumption = tonnage * feed_req_mj) %>% 
    select(country, product, mj_consumption) %>% 
    mutate(mj_consumption = mj_consumption / sum(mj_consumption)) %>% 
    crossing(feed_produced) %>% 
    left_join(.feed_mj, by = "crop") %>% 
    mutate(consumption = (mj_consumption * mj_produced) / mj / dm) %>% 
    select(country, product, crop, consumption) 
  
  # Combine
  feed_nutrient_consumption <- bind_rows(grass_consumed,
                                         fodder_consumed,
                                         feed_consumed) %>%
    rename('animal' = product) %>% 
    left_join(.feed_nutrition, by = c("animal", "crop")) %>%
    rename('me' = mj) %>% 
    group_by(country, animal) %>% 
    summarise_at(vars(me, ge, cp), list(~sum(. * dm * consumption))) %>% #MJ energy, tonnes protein
    ungroup()
  
  # Feed imported - allocate in proportion to cereal production
  feed_imported <- feed_per_country %>% 
    filter(crop_group == "Cereal") %>%
    mutate(prop = mj / sum(mj)) %>% 
    mutate(mj_imported = prop * feed_surplus$mj_imported) %>% 
    select(country, mj_imported) 
  
  
  
  ## Return this ---------------
  out <- list(food_type_sum = food_type_sum,
              country_sums = cropped_area,
              crop_areas = crop_areas,
              feed_nutrient_consumption = feed_nutrient_consumption,
              feed_imported = feed_imported)
  
  return(out)
}


# SAVE ---------------------
save(food_production_fun, file = "rdata/food/food_fun_2022.RData")
