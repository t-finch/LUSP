## INITS -------------------------------
source("setup.R")

## TIDY BASELINE AREAS ------------------------------------
load("rdata/scenarios/scenarios_areas_2022.RData")


# Save raw areas
areas_baseline <- scenarios_areas %>%
  filter(year == 2015 & scenario == 0) %>% 
  # Combine London with SE
  mutate(nuts1 = ifelse(nuts1 %in% c("South East (England)", "London"), "South East & London (England)", nuts1)) %>%
  # Summarise (collapse peat categories)
  group_by(country, nuts1, alc, lcm) %>% 
  summarise(area = sum(area),
            n = area * 16,
            .groups = "drop")



## DISTRIBUTE TEMPORARY GRASS ACROSS a.h & i.g ------------------------------------
# NUTS1 arable crop & grass areas
arable_area <- read_csv("data/food/arable_area_nuts1.csv", skip = 1) %>% select(-year, -notes, -source, -lcm) 

# Calculate temporary grass areas on a.h / i.g
tempgrass_redist <- inner_join(arable_area %>% 
                                 mutate(type = ifelse(crop_group == "Grass", crop_fine, "Arable")) %>% 
                                 group_by(type, nuts1) %>% 
                                 summarise(area_ha = sum(area_ha),
                                           .groups = "drop"),
                               areas_baseline %>% 
                                 filter(lcm %in% c("a.h", "i.g")) %>% 
                                 group_by(nuts1, lcm) %>% 
                                 summarise(area = sum(area),
                                           .groups = "drop"), 
                               by = "nuts1") %>% 
  filter(!(type == "Arable" & lcm == "i.g") & !(type == "Permanent grass" & lcm == "a.h")) %>% 
  # How much temp grass is a.h (ha), how much i.g (%)?
  group_by(nuts1) %>% 
  summarise(temp_grass_a.h = min(area[type == "Arable"] - area_ha[type == "Arable"],
                                 unique(area_ha[type == "Temporary grass"])),
            temp_grass_i.g = (unique(area_ha[type == "Temporary grass"]) - temp_grass_a.h) / unique(area[lcm == "i.g"]))

# Update arable_area with new Temp grass area
arable_area <- arable_area %>% 
  filter(crop_fine == "Temporary grass") %>% 
  left_join(tempgrass_redist %>% select(-temp_grass_i.g), by = "nuts1") %>% 
  mutate(area_ha = temp_grass_a.h) %>% 
  select(-temp_grass_a.h) %>% 
  bind_rows(arable_area %>% filter(crop_group != "Grass"), .) %>% 
  filter(area_ha != 0)


# Temp grass props
tempgrass_area <- bind_rows(arable_area %>% 
                              group_by(nuts1) %>% 
                              mutate(prop = area_ha/sum(area_ha)) %>% 
                              ungroup() %>% 
                              filter(crop_fine == "Temporary grass") %>% 
                              select(nuts1, prop) %>% 
                              mutate(lcm = "a.h",
                                     grass_type = "temp"),
                            tempgrass_redist %>% 
                              select(-temp_grass_a.h) %>% 
                              rename('temp' = temp_grass_i.g) %>% 
                              mutate(perm = 1-temp)%>% 
                              gather(grass_type, prop, -nuts1) %>% 
                              mutate(lcm = "i.g"))



## BASELINE CROP PRODUCTION ------------------------------------
# UK arable crop yield
arable_yield <- read_csv("data/food/arable_yield.csv", skip = 1) %>% select(-region, -year, -source, -notes)

# UK arable crop production (for selected crops)
arable_prod <- read_csv("data/food/arable_production.csv", skip = 1) %>% select(-region, -year, -source)

# Yield correction (so yield * area = prod)
arable_yield_correction <- arable_area %>% 
  inner_join(arable_yield, by = c('crop_fine', 'crop')) %>% 
  mutate(prod = area_ha * yield) %>% 
  group_by(crop) %>% 
  summarise(prod = sum(prod),
            area = sum(area_ha)) %>%
  ungroup() %>% 
  left_join(arable_prod, by = 'crop') %>%
  mutate(correction = production / prod) %>% 
  select(crop, correction)

# UK arable crop yield - corrected
arable_yield <- arable_yield %>% 
  left_join(arable_yield_correction, by = 'crop') %>% 
  mutate(yield = ifelse(is.na(correction), yield, yield * correction)) %>% 
  select(-correction) 

# Baseline total horticultural production
hort_prod <- read_csv("data/food/hort_production.csv", skip = 1) %>% 
  select(hort_crop, crop, prod_t) %>% 
  rename('crop_fine' = hort_crop, 'prod' = prod_t)

# Baseline horticultural area
hort_area <- arable_area %>%
  filter(crop == "Horticultural crops") %>%
  # Exclude Hardy nursery stock from area
  filter(crop_fine != "Hardy nursery stock") %>% 
  group_by(crop, crop_group, nuts1) %>% 
  summarise(area_ha = sum(area_ha),
            .groups = "drop")

# Baseline average yield of horticulture
hort_yield <- hort_area %>%
  group_by(crop) %>% 
  summarise(area_ha = sum(area_ha),
            .groups = "drop") %>% 
  left_join(hort_prod, by = c("crop")) %>%
  # Average yield of each fruit/veg per ha horticulture
  mutate(yield = prod/area_ha) %>% 
  select(-area_ha, -prod)


## ORGANIC CROPPING -------------------------------
# Update arable areas, so minimum 10% legumes, 15% temporary grass
arable_area <- arable_area %>% 
  group_by(nuts1) %>% 
  mutate(area_legumes = sum(area_ha[crop_group == "Legumes"]),
         area_tempgrass = sum(area_ha[crop == "Temporary grass"]),
         area_total = sum(area_ha),
         add_legumes = area_total * (0.1 - area_legumes / area_total),
         add_tempgrass = area_total * (0.15 - area_tempgrass / area_total),
         add_legumes = ifelse(add_legumes < 0, 0, add_legumes),
         add_tempgrass = ifelse(add_tempgrass < 0, 0, add_tempgrass),
         add_total = add_legumes + add_tempgrass, 
         area_ha_organic = case_when(crop_group == "Legumes" ~ area_ha + add_legumes * (area_ha / area_legumes),
                                     crop == "Temporary grass" ~ area_ha + add_tempgrass,
                                     TRUE ~ area_ha - add_total * (area_ha / (area_total - area_legumes - area_tempgrass)))) %>% 
  ungroup() %>% 
  select(crop_fine, crop, crop_group, nuts1, area_ha_organic) %>% 
  rename('area_ha' = area_ha_organic) %>% 
  mutate(crop_fine = ifelse(crop_fine == "Temporary grass", "Temporary grass (new ley)", crop_fine)) %>% 
  mutate(organic = TRUE) %>% 
  bind_rows(arable_area %>% mutate(organic = FALSE)) %>% 
  group_by(nuts1) %>% 
  mutate(area_ha = ifelse(crop == "Temporary grass" & organic, area_ha - area_ha[crop == "Temporary grass" & !organic], area_ha)) %>%
  ungroup() %>% 
  bind_rows(arable_area %>% filter(crop == "Temporary grass") %>% mutate(organic = TRUE)) 

# Hort area
hort_area <- arable_area %>%
  filter(crop == "Horticultural crops" & organic == TRUE) %>%
  # Exclude Hardy nursery stock from area
  filter(crop_fine != "Hardy nursery stock") %>% 
  group_by(crop, crop_group, nuts1) %>% 
  summarise(area_ha = sum(area_ha),
            .groups = "drop") %>% 
  mutate(organic = TRUE) %>% 
  bind_rows(hort_area %>% mutate(organic = FALSE))

# Yield penalities
organic_yields <- read_csv("data/food/organic_yields.csv", skip = 1) %>% select(-source, -notes)

# Adjust arable yield
arable_yield <- arable_yield %>% 
  left_join(organic_yields, by = c("crop")) %>% 
  mutate(yield = yield * organic_yield) %>% 
  select(-organic_yield) %>% 
  mutate(organic = TRUE) %>% 
  bind_rows(arable_yield %>% mutate(organic = FALSE))

# Adjust horticultural yield
hort_yield <- hort_yield %>% 
  left_join(organic_yields, by = c("crop")) %>% 
  mutate(yield = yield * organic_yield) %>% 
  select(-organic_yield) %>% 
  mutate(organic = TRUE) %>% 
  bind_rows(hort_yield %>% mutate(organic = FALSE))




## ARABLE/HORT PRODUCTION PER PIXEL ----------------------------------------
# ALC yield weighting
alc_weighting <- read_csv("data/food/alc_weighting2.csv", skip = 1) %>% select(-notes)

# Get area of a.h * ALC * NUTS1 from raster
a.h_alc_area <- areas_baseline %>%
  filter(lcm == "a.h") %>%
  group_by(country, nuts1, alc) %>% 
  summarise(area = sum(area),
            n = sum(n),
            .groups = "drop") %>% 
  # Cross by organic
  crossing(organic = c(TRUE, FALSE))


# Arable production per pixel
arable_nuts1_alc_prod <- a.h_alc_area %>%
  # Proportion of NUTS1 arable area
  group_by(nuts1, organic) %>%
  mutate(prop_nuts1 = area/sum(area)) %>%
  ungroup() %>%
  # Get ALC weighting
  left_join(alc_weighting, by = c("alc", "country")) %>%
  # Get NUTS1 crop area
  left_join(arable_area, by = c("nuts1", "organic")) %>%
  # Get UK crop yield
  left_join(arable_yield, by = c("crop_fine", "crop", "organic")) %>%
  # Calculate regional production
  mutate(prod = area_ha * prop_nuts1 * yield * rel_yield,
         area = area_ha * prop_nuts1,
         prod_per_n = prod / n,
         area_per_n = area / n) %>% 
  select(-area_ha, -prop_nuts1, -rel_yield, -yield) %>% 
  filter(!crop_group %in% c("Fruit & vegetables", "Fallow", "Grass"))


# Horticulture production per pixel
hort_nuts1_alc_prod <- a.h_alc_area %>%
  # Proportion of NUTS1 arable area
  group_by(nuts1, organic) %>%
  mutate(prop_nuts1 = area/sum(area)) %>%
  ungroup() %>%
  # Get ALC weighting
  left_join(alc_weighting, by = c("alc", "country")) %>%
  # Get hort area 
  left_join(hort_area, by = c("nuts1", "organic")) %>% 
  # Get hort yield
  left_join(hort_yield, by = c("crop", "organic")) %>% 
  # Calculate prod
  mutate(prod = area_ha * prop_nuts1 * yield * rel_yield,
         prod_per_n = prod / n) %>%
  select(-area, -prop_nuts1, -rel_yield, -yield) %>% 
  # Calculate area of each crop type
  rename('area' = area_ha) %>% 
  group_by(crop_fine, nuts1, organic) %>% 
  mutate(area = (area * n / sum(n)) / n_distinct(hort_yield$crop_fine), # Divide eqaully across 33 horticultural crops 
         area_per_n = area / n) %>% 
  ungroup() 


# Combine arable & horticulture production
a.h_prod_per_n <- bind_rows(arable_nuts1_alc_prod,
                            hort_nuts1_alc_prod)

# Adjust areas on low grade land
a.h_prod_per_n <- a.h_prod_per_n %>% 
  mutate(grade = ifelse(alc %in% c("Grade 1", "Grade 2", "Grade 3A", "Grade 3B", "3A", "3B", "3.1", "3.2", "3a", "3b", "1", "2"), "high", "low")) %>%
  group_by(nuts1, crop_fine, organic) %>%
  mutate(area2 = case_when(grade == "low" & (crop_fine %in% c("Potatoes", "Sugar beet") | crop == "Horticultural crops") ~ 0,
                           grade == "high" & (crop_fine %in% c("Potatoes", "Sugar beet") | crop == "Horticultural crops") ~ area + sum(area[grade == "low"]) * (area / sum(area[grade == "high"])),
                           TRUE ~ area)) %>%
  group_by(nuts1, alc, organic) %>%
  mutate(area2 = case_when(!(crop_fine %in% c("Potatoes", "Sugar beet") | crop == "Horticultural crops") ~ area + (area / sum(area[!(crop_fine %in% c("Potatoes", "Sugar beet") | crop == "Horticultural crops")])) * sum(area - area2),
                           TRUE ~ area2)) %>%
  ungroup() %>%
  mutate(prod_per_n = prod_per_n * area2/area,
         area_per_n = area_per_n * area2/area,
         area = area2,
         prod = prod_per_n * n) %>%
  select(-area2)


# Calculate error
a.h_prod_error <- bind_rows(
  # Arable
  inner_join(arable_area %>% 
               filter(!organic) %>% 
               left_join(arable_yield, by = c("crop_fine", "crop", "organic")) %>% 
               group_by(crop_fine) %>% 
               # Actual production based on area*yield
               summarise(prod_exp = sum(area_ha * yield),
                         .groups = "drop"),
             a.h_prod_per_n %>% 
               filter(!organic) %>% 
               group_by(crop_fine) %>%
               # Estimated production from summing pixel-level prod 
               summarise(prod_obs = sum(prod),
                         .groups = "drop"),
             by = 'crop_fine'),
  # Horticulture
  inner_join(hort_area %>% 
               filter(!organic) %>% 
               left_join(hort_yield, by = c("crop", "organic")) %>% 
               group_by(crop_fine) %>% 
               # Actual production based on area*yield
               summarise(prod_exp = sum(area_ha * yield),
                         .groups = "drop"),
             a.h_prod_per_n %>% 
               filter(!organic) %>% 
               group_by(crop_fine) %>%
               # Estimated production from summing pixel-level prod 
               summarise(prod_obs = sum(prod),
                         .groups = "drop"),
             by = 'crop_fine')) %>% 
  mutate(rel_prod = prod_exp/ prod_obs)%>% 
  select(crop_fine, rel_prod)

# Recalculate corrected production 
a.h_prod_per_n <- a.h_prod_per_n %>% 
  left_join(a.h_prod_error, by = "crop_fine") %>% 
  mutate(prod = prod * rel_prod,
         prod_per_n = prod_per_n * rel_prod) %>% 
  select(-rel_prod) 



## FOOD VALUE OF ARABLE & HORTICULTURE ----------------------------------------
# Arable nutritional info (FAO)
arable_food <- read_csv("data/food/arable_food_alt.csv", skip = 1) %>% select(-source)

# Arable food value per tonne
arable_food_val <- arable_food %>%
  mutate(energy = energy / 1000, # Million kcal per tonne
         protein = protein / 1000) %>% # kg per kg i.e. tonnes per tonne i.e %
  group_by(crop) %>%
  inner_join(unique(select(a.h_prod_per_n, crop, crop_fine)), ., by = "crop") 


# Horticulture nutritional info
hort_food <- read_csv("data/food/hort_food.csv", skip = 1) %>% 
  select(-source, -USDA_name, -edible_name)

# Horticulture food value per tonne
hort_food_val <- hort_food %>%
  mutate(energy = edible_frac * (kcal_100g / 0.0001) / 1000000, # Million kcal per tonne
         protein = edible_frac * (gp_100g / 100)) %>% # tonnes per tonne, or %
  select(crop_fine, energy, protein) %>% 
  inner_join(unique(select(a.h_prod_per_n, crop, crop_fine)), ., by = "crop_fine") 

# Combine arable & horticulture food value per tonne
a.h_food_val <- bind_rows(arable_food_val,
                          hort_food_val)

# Calculate energy & protein per pixel
food_per_n <- a.h_prod_per_n %>% 
  filter(!crop_group %in% c("Fodder crops", "Maize, grain", "Maize, whole")) %>%
  left_join(a.h_food_val, by = c("crop_fine", "crop"))

# Arable utilisation (FAO)
arable_utilisation <- read_csv("data/food/arable_utilization_alt.csv", skip = 1) %>% 
  select(-notes) %>% 
  inner_join(unique(select(a.h_prod_per_n, crop, crop_fine)), ., by = "crop") 

# Horticulture utilisation (FAO)
hort_utilisation <- read_csv("data/food/hort_utilization.csv", skip = 1) %>% 
  select(-notes, -hort_category) %>% 
  inner_join(unique(select(a.h_prod_per_n, crop, crop_fine)), ., by = "crop_fine") %>% 
  filter(fraction != "Losses")

# Combine 
a.h_utilisation <- bind_rows(arable_utilisation,
                             hort_utilisation) %>%
  # Account for losses later (as waste)
  group_by(crop, crop_fine) %>% 
  mutate(prop_ut = prop_ut / sum(prop_ut)) %>% 
  filter(fraction %in% c("Feed", "Food")) %>% 
  arrange(crop, crop_fine, fraction) %>% 
  ungroup()

# Separate utilisation for high/low grade farmland
a.h_utilisation <- a.h_prod_per_n %>%
  group_by(crop_fine, grade) %>% 
  summarise(prod = sum(prod),
            .groups = "drop") %>%
  group_by(crop_fine) %>% 
  mutate(prop_grade = prod / sum(prod)) %>%
  ungroup() %>% 
  full_join(a.h_utilisation, by = "crop_fine") %>% 
  group_by(crop_fine) %>% 
  mutate(prop_ut = case_when(fraction %in% c("Seed", "Processing losses") | crop == "Oilseed rape" | sum(prop_ut[fraction == "Feed"]) == 0  | sum(prod[grade == "low"]) == 0 ~ prop_ut,
                             grade == "low" & fraction == "Feed" ~ sum(prop_ut[fraction %in% c("Feed", "Food") & grade == "low"]),
                             grade == "low" & fraction == "Food" ~ 0,
                             grade == "high" & fraction == "Food" ~ prop_ut / prop_grade,
                             grade == "high" & fraction == "Feed" ~ (prop_ut - prop_grade[grade == "low" & fraction == "Feed"] * sum(prop_ut[fraction %in% c("Feed", "Food") & grade == "low"])) / prop_grade)) %>%
  ungroup() %>%
  filter(prop_grade != 0) %>%
  select(-prod, -prop_grade) %>%
  arrange(grade, crop_fine, fraction)


## RUMINANT DISTRIBUTION ------------------
# Livestock props NUTS1
ruminant_props_eng <- bind_rows(read_csv("data/food/sheepcattle_bynuts1.csv") %>% select(-notes, -source, -year)) %>% 
  mutate(country = "England")

# Livestock head per nuts1 region, 2015
ruminant_props <- read_csv("data/food/sheepcattle_bycountry.csv") %>% select(-notes, -source) %>%
  filter(year == 2015) %>% 
  mutate(livestock = ifelse(livestock == "Unspecified cattle", "Beef cattle", livestock)) %>% 
  group_by(country, livestock) %>% 
  summarise(head = sum(head),
            .groups = "drop") %>% 
  left_join(ruminant_props_eng, 
            by = c("country", "livestock")) %>% 
  mutate(nuts1 = ifelse(is.na(nuts1), country, nuts1),
         head = ifelse(is.na(prop), head, prop * head)) %>% 
  select(nuts1, livestock, head) %>% 
  rename(product = livestock) %>% 
  mutate(product = case_when(product == "Beef cattle" ~ "Beef",
                             product == "Dairy cattle" ~ "Milk",
                             product == "Sheep" ~ product)) %>% 
  group_by(product) %>% 
  mutate(prop = head/sum(head)) %>% 
  select(-head)

# Get baseline grass requirements
grass_req <- read_csv("data/food/grass_req.csv", skip = 1) %>% select(-notes)

# Get baseline livestock production
livestock_prod <- read_csv("data/food/livestock_production.csv", skip = 1) %>% select(-notes)

# Calculate fraction of grass (in each NUTS1) to sheep/beef/dairy
ruminant_props <- ruminant_props %>% 
  left_join(livestock_prod, by = "product") %>% 
  left_join(grass_req %>% select(product, grass_req), by = "product") %>% 
  mutate(prod = prop * production, # Production, proportional to headage
         grass = prod * grass_req) %>% # Consumption, proportional to production
  group_by(nuts1) %>% 
  mutate(prop = grass / sum(grass)) %>% 
  ungroup() %>% 
  select(nuts1, product, prop) 


# BASELINE GRASS PRODUCTION ----------------------------------
# Get area of grass types * grassclass * NUTS1 from raster
grass_class_areas <- areas_baseline %>%
  filter(lcm %in% c("a.h", "i.g", "n.g", "c.g", "a.g", "f.s", "h.r", "b.g", "s.m")) %>%
  left_join(tempgrass_area, by = c("nuts1", "lcm")) %>% 
  mutate(grass_type = ifelse(is.na(prop), "rough", grass_type),
         prop = ifelse(is.na(prop), 1, prop)) %>% 
  mutate(area = area * prop) %>% 
  group_by(country, nuts1, lcm, grass_type) %>% 
  summarise(area = sum(area),
            n = sum(n),
            .groups = "drop")

# Add rows for missing LCM*grassclass*NUTS1 which may appear in future scenarios 
grass_class_areas <- crossing(unique(select(areas_baseline, country, nuts1)),
                              unique(select(grass_class_areas, lcm, grass_type))) %>% 
  anti_join(grass_class_areas, by = c("country", "nuts1", "lcm", "grass_type")) %>% 
  mutate(area = 0,
         n = 0) %>%
  mutate(area_per_n = ifelse(lcm %in% c("a.h", "i.g"), NA, 0.0625)) %>% 
  bind_rows(mutate(grass_class_areas, area_per_n = area/n), .) %>% 
  group_by(nuts1, lcm, grass_type) %>% 
  mutate(area_per_n = ifelse(lcm %in% c("a.h", "i.g") & is.na(area_per_n), unique(area_per_n[!is.na(area_per_n)]), area_per_n)) %>% 
  ungroup()

# Get grass type relative yield
grasstype_weighting <- read_csv("data/food/grass_yields.csv", skip = 1) %>% select(-notes) %>%
  mutate(yield = yield/max(yield)) %>% 
  rename('yield_type' = yield)

# Calculate relative yield of each grass type/class grassland
grass_yields <- grass_class_areas %>%
  left_join(grasstype_weighting, by = c("lcm", "grass_type")) %>% 
  rename(yield = yield_type) 

# Add grass_type to ruminant_props, with dairy avoiding rough grass where possible
ruminant_props <- ruminant_props %>%
  # Calculate grass production
  inner_join(grass_yields, by = "nuts1") %>%
  mutate(grass_type = ifelse(grass_type == "rough", "rough", "improved")) %>% 
  group_by(nuts1, grass_type, product, prop) %>% 
  summarise(grass_prod_rel = sum(yield * area),
            .groups = "drop") %>%
  # Redistribute rough grass away from dairy, where possible
  group_by(nuts1) %>% 
  mutate(improved = unique(grass_prod_rel[grass_type == "improved"]),
         rough = unique(grass_prod_rel[grass_type == "rough"]),
         milk = sum(grass_prod_rel[product == "Milk"] * prop[product == "Milk"]),
         beef = sum(grass_prod_rel[product == "Beef"] * prop[product == "Beef"]),
         sheep = sum(grass_prod_rel[product == "Sheep"] * prop[product == "Sheep"]),
         milk_rough = ifelse(milk < improved, 0, milk - improved),
         milk_improved = milk - milk_rough,
         beef_improved = (improved - milk_improved) * (beef / (beef + sheep)),
         beef_rough = (rough - milk_rough) * (beef / (beef + sheep)),
         sheep_improved = (improved - milk_improved) * (sheep / (beef + sheep)),
         sheep_rough = (rough - milk_rough) * (sheep / (beef + sheep))) %>%
  ungroup() %>% 
  select(-improved, -rough, -milk, -beef, -sheep) %>% 
  gather(var, prop2, -nuts1, -grass_type, -product, -prop, -grass_prod_rel) %>% 
  separate(var, c("product2", "grass_type2"), "_") %>% 
  filter(grass_type == grass_type2 & tolower(product) == product2) %>% 
  group_by(nuts1, grass_type) %>% 
  mutate(prop = prop2/sum(prop2),
         prop = ifelse(grass_prod_rel == 0, 0, prop)) %>% 
  ungroup() %>% 
  select(nuts1, product, grass_type, prop) %>% 
  # Merge with census units for which no current grass production
  bind_rows(ruminant_props %>%
              anti_join(grass_yields, by = "nuts1") %>% 
              crossing(grass_type = c("rough", "improved"))) %>% 
  mutate(prop = ifelse(prop < 0, 0, prop))


# Calculate meat factor
meat_factor <- ruminant_props %>% 
  # Grass req
  left_join(grass_req %>% select(product, grass_req), by = "product") %>%
  # Grass yield
  inner_join(grass_yields %>% mutate(grass_type = ifelse(grass_type == "rough", "rough", "improved")), by = c("nuts1", "grass_type")) %>%
  # Calculate meat production
  mutate(meat_prod = ((yield * area * prop) / grass_req)) %>% 
  left_join(livestock_prod, by = "product") %>% 
  group_by(product, production) %>%
  mutate(meat_prod = meat_prod * production / sum(meat_prod, na.rm = TRUE)) %>%
  ungroup() %>% 
  # Calculate 'meat factor'
  group_by(product) %>% 
  summarise(meat_factor = mean((meat_prod / (yield * area)) / prop, na.rm = TRUE)) %>% 
  ungroup()

# Calculate absolute grassland yields
grass_per_n <- grass_yields %>%
  group_by(nuts1, lcm) %>%
  # Sum n across temp and perm
  mutate(n = sum(n)) %>%
  # Calculate area_per_n for missing LCM*grassclass*NUTS1 combos
  group_by(country, nuts1, lcm, grass_type) %>% 
  mutate(area_per_n = ifelse(sum(!is.na(area_per_n)) == 0, NA, unique(area_per_n[!is.na(area_per_n)]))) %>% 
  ungroup() %>% 
  mutate(area_per_n = ifelse(is.na(area_per_n), 0.0625, area_per_n)) %>% 
  mutate(#area_per_n = round(area / n, 10),
    yield_per_n = yield * area_per_n) %>% 
  # No need for temporary grass on i.g in regions where there is none
  filter(!(grass_type == "temp" & area == 0 & lcm == "i.g"))



# ORGANIC GRASSLAND ---------------------------
# Adjust temp grass area & yield
grass_per_n <- grass_per_n %>% 
  # Increase temp grass area up to 15% of a.h
  mutate(area_per_n = ifelse(lcm == "a.h" & area_per_n < 0.15 * 0.0625, 0.15 * 0.0625, area_per_n),
         area = area_per_n * n) %>% 
  # Add yield penalty
  left_join(organic_yields %>% 
              filter(crop %in% c("Temporary grass", "Permanent grass")) %>% 
              mutate(grass_type = ifelse(crop == "Temporary grass", "temp", "perm")) %>% 
              select(-crop),
            by = "grass_type") %>% 
  mutate(yield = yield * ifelse(is.na(organic_yield), 1, organic_yield),
         yield_per_n = yield * area_per_n) %>% 
  select(-organic_yield) %>% 
  mutate(organic = TRUE) %>% 
  bind_rows(grass_per_n %>% 
              mutate(organic = FALSE)) %>% 
  select(-n, -yield, -area)

# Temp grass props
tempgrass_area <- bind_rows(arable_area %>%
                              filter(organic) %>% 
                              group_by(nuts1) %>% 
                              mutate(prop = area_ha/sum(area_ha)) %>% 
                              ungroup() %>% 
                              filter(crop_fine == "Temporary grass") %>% 
                              select(nuts1, prop) %>% 
                              mutate(lcm = "a.h",
                                     grass_type = "temp"),
                            arable_area %>%
                              filter(organic) %>% 
                              group_by(nuts1) %>% 
                              mutate(prop = area_ha/sum(area_ha)) %>% 
                              ungroup() %>% 
                              filter(crop_fine == "Temporary grass (new ley)") %>% 
                              select(nuts1, prop) %>% 
                              mutate(lcm = "a.h",
                                     grass_type = "temp_new"),
                            tempgrass_redist %>% 
                              select(-temp_grass_a.h) %>% 
                              rename('temp' = temp_grass_i.g) %>% 
                              mutate(perm = 1-temp)%>% 
                              gather(grass_type, prop, -nuts1) %>% 
                              mutate(lcm = "i.g")) %>% 
  mutate(organic = TRUE) %>% 
  bind_rows(tempgrass_area %>% mutate(organic = FALSE))



# BASELINE FODDER CROP PRODUCTION ----------------------------------
# Get fodder requirements
fodder_req <- read_csv("data/food/fodder_req.csv", skip = 1) %>% select(-notes)

# Calculate fodder requirements
fodder_consumption <- livestock_prod %>% 
  inner_join(fodder_req, by = "product") %>% 
  mutate(fodder_req = production * fodder_req) %>% 
  select(-production)

# Calculate fodder production per pixel
fodder_per_n <- a.h_alc_area %>%
  # Proportion of NUTS1 arable area
  group_by(nuts1, organic) %>%
  mutate(prop_nuts1 = area/sum(area)) %>%
  ungroup() %>%
  # Get ALC weighting
  left_join(alc_weighting, by = c("alc", "country")) %>%
  # Get NUTS1 crop area
  left_join(arable_area %>% filter(crop %in% c("Fodder crops", "Maize, whole")), by = c("nuts1", "organic")) %>% 
  mutate(prod = area_ha * prop_nuts1 * rel_yield,
         area = area_ha * prop_nuts1,
         area_per_n = area / n) %>%
  select(-crop_fine, -crop, -area_ha, -rel_yield) %>% 
  # Join with actual requirements
  left_join(fodder_consumption %>% 
              group_by(crop_group) %>% 
              summarise(fodder_req = sum(fodder_req),
                        .groups = "drop"), 
            by = "crop_group") %>% 
  group_by(crop_group) %>% 
  mutate(yield = fodder_req / sum(prod[!organic])) %>% 
  ungroup() %>% 
  # Adjust organic yield
  left_join(organic_yields, by = c("crop_group" = "crop")) %>% 
  mutate(yield = ifelse(organic, organic_yield * yield, yield)) %>% 
  mutate(prod = prod * yield,
         prod_per_n = prod / n) %>% 
  select(-fodder_req, -organic_yield) 

# Calculate absolute fodder yield
fodder_yield <- fodder_per_n %>% 
  select(crop_group, yield, organic) %>% 
  unique()




## BASELINE FEED PRODUCTION OF ARABLE & HORTICULTURE ----------------------------------------
# Get feed nutrition
feed_nutrition <- read_csv("data/food/feed.csv", skip = 1) %>% select(-notes, -source) %>%
  gather(product, mj, -crop, -dm, -cp, -ge) %>% 
  mutate(mj = mj * 1000,  # ME MJ per kg to MJ per tonne
         ge = ge * 1000,  # GE MJ per kg to MJ per tonne
         cp = cp * 1) %>% # CP tonnes per tonne
  mutate(animal = case_when(product == "beef_me" ~ "Beef",
                            product == "milk_me" ~ "Milk",
                            product == "sheep_me" ~ "Sheep",
                            product == "pig_me" ~ "Pig",
                            product == "poultry_me" ~ "Poultry",
                            product == "eggs_me" ~ "Eggs"))

# Average MJ across ruminants, pigs and poultry
feed_mj <- feed_nutrition %>%
  filter(animal %in% c("Beef", "Poultry", "Pig")) %>% 
  group_by(crop, dm) %>% 
  summarise(mj = mean(mj)) %>% 
  ungroup()

# Calculate feed production per n
feed_per_n <- a.h_prod_per_n %>% 
  select(country, nuts1, alc, grade, organic, crop_fine, crop, crop_group, n, prod_per_n) %>% 
  inner_join(feed_mj, by = 'crop') %>% 
  mutate(mj_per_n = prod_per_n * dm * mj) %>% 
  select(-prod_per_n, -dm, -mj)

# Get feed requirements
feed_req <- read_csv("data/food/feed_req.csv", skip = 1) %>% select(-notes)

# Calculate feed requirements
feed_req <- feed_req %>% 
  mutate(energy = energy * 1000) %>% # from MJ/kg to MJ/tonne
  group_by(product) %>% 
  summarise(feed_req_mj = sum(feed_req * energy)) # MJ feed per tonne meat/milk


# LIVESTOCK PRODUCTION --------------------------------------
# Ratio of feed use for nonruminants
nonruminant_feed_ratios <- livestock_prod %>%
  filter(product %in% c("Poultry", "Pig", "Eggs")) %>% 
  left_join(feed_req, by = 'product') %>% 
  mutate(prop = (production * feed_req_mj) / sum(production * feed_req_mj)) %>% 
  select(-production, -feed_req_mj)

# Livestock food value
livestock_food <- read_csv("data/food/livestock_food_alt.csv", skip = 1) %>% select(-notes)


# BASELINE FEED IMPORTS ---------------------------------
frac_imported <- ruminant_props %>%
  # Calculate baseline ruminant production from available grass
  inner_join(grass_yields %>% mutate(grass_type = ifelse(grass_type == "rough", "rough", "improved")), by = c("nuts1", "grass_type")) %>%
  left_join(meat_factor, by = "product") %>% 
  group_by(product) %>% 
  summarise(production = sum(prop * meat_factor * area * yield),
            .groups = "drop") %>% 
  # Join with feed requirements to calculate baseline feed consumed
  inner_join(feed_req, by = "product") %>% 
  summarise(mj_consumed = sum(production * feed_req_mj),
            .groups = "drop") %>%   
  # Join with baseline feed produced
  bind_cols(a.h_prod_per_n %>%
              filter(!organic) %>% 
              group_by(crop_fine, crop, crop_group, grade) %>% 
              summarise(prod = sum(prod),
                        .groups = "drop") %>% 
              inner_join(a.h_utilisation %>% filter(fraction == "Feed"), by = c("crop_fine", "crop", "grade")) %>% 
              inner_join(feed_mj, by = 'crop') %>% 
              summarise(mj_produced = sum(prod * dm * mj * prop_ut),
                        .groups = "drop")) %>% 
  # Calculate feed surplus (MJ)
  mutate(mj_surplus = mj_produced - mj_consumed) %>% 
  select(mj_produced, mj_surplus) %>% 
  # Calculate feed requirements of baseline non-ruminant production
  crossing(livestock_prod %>% filter(product %in% nonruminant_feed_ratios$product)) %>% 
  inner_join(feed_req, by = "product") %>% 
  group_by(mj_produced, mj_surplus) %>% 
  summarise(mj_consumed = sum(production * feed_req_mj),
            .groups = "drop") %>% 
  # Calculate surplus imported
  mutate(mj_imported = mj_consumed - mj_surplus) %>% 
  select(-mj_consumed, -mj_surplus) %>% 
  mutate(frac_imported = mj_imported / mj_produced)


# FOOD WASTE --------------------------------------
waste <- read_csv("data/food/waste.csv", skip = 1) %>% select(-notes)


# FOOD FROM SILVOARABLE --------------------------------------
# Read in fruit yields
agroforestry_food <- read_csv("data/food/agroforestry_food.csv", skip = 1) %>% select(-notes)

# Tidy
agroforestry_food <- agroforestry_food %>% 
  mutate(crop_group = "Fruit & vegetables") %>%
  # Calculate prod per n
  mutate(prod_per_n = yield * 0.0625,
         area_per_n = 0.0625) %>% 
  # Join with nutritional value
  left_join(a.h_food_val, by = c("crop", "crop_fine"))




## COLLATE PARAMETERS ----------------------------------
# Collate paramaters
food_params <- list(
  # Baseline food of arable/horticulture
  .food = food_per_n %>% select(-n, -prod, -area),
  # Baseline feed from arable/horticulture
  .feed = feed_per_n %>% select(-n),
  # Feed nutrition
  .feed_nutrition = feed_nutrition,
  # Feed average MJ values
  .feed_mj = feed_mj,
  # Fraction of feed imported
  .frac_imported = frac_imported,
  # Baseline fodder from arable/horticulture
  .fodder = fodder_per_n %>% select(-n, -prod, -yield, -area, -prop_nuts1),
  # Crop utilisation
  .utilization = a.h_utilisation,
  # Temporary grass areas
  .tempgrass_area = tempgrass_area,
  # Baseline grass production
  .grass_prod = grass_per_n,
  # Ruminant livestock props
  .ruminant_props = ruminant_props,
  # # Livestock grass requirements
  .grass_req = grass_req %>% select(-meat_out, -grass_in),
  # Ruminant meat factor
  .meat_factor = meat_factor,
  # Livestock food requirements
  .fodder_req = fodder_req,
  # Livestock feed requirements
  .feed_req = feed_req,
  # Non-ruminant feed ratios
  .nonruminant_feed_ratios = nonruminant_feed_ratios,
  # Food value of animal products
  .livestock_food = livestock_food,
  # Wasted fraction
  .waste = waste,
  # Food from agroforestry
  .agroforestry_food = agroforestry_food)

## SAVE ---------------------------------
save(food_params, file = "rdata/food/food_params_2022.RData")
