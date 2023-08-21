source("setup.R")



## GET WCC SEQUESTRATION CURVES ---------------------------
# Selected species from WCC
wcc_data_raw <- read_csv("data/ghg/wood/ghg_wood_wcc_full.csv", skip = 1) %>%
  # Tidy year
  separate(year, c("start", "end"), sep = "-") %>% 
  mutate(year = as.numeric(end)) %>% 
  select(-start, -end) 


# Initial filter & tidy - core species & standard spacing
wcc_data_5y <- wcc_data_raw %>%
  # Don't need these species:
  filter(!species %in% c("BE", "DF")) %>% 
  filter(yc > 2) %>% 
  # Standard spacing:
  filter((species %in% c("SAB", "OK") & spacing == 2.5) | (species %in% c("SS") & spacing == 1.7) | (species == "SP" & spacing == 2) | species == "EL") %>% 
  # Thinned or not:
  filter((species %in% c("SS") & thin == "Thinned") | species %in% c("SP", "OK", "SAB", "EL")) %>% 
  # Derive in-period management & total emissions (from cumulative)
  group_by(species, spacing, yc, thin) %>% 
  mutate(mgmt = ifelse(year == 5, cumulative_mgmt, cumulative_mgmt - lag(cumulative_mgmt)),
         annual_total = ifelse(year == 5, cumulative_total, cumulative_total - lag(cumulative_total))) %>% 
  ungroup() %>% 
  # Multiple /yr values by 5 
  mutate(removed = removed * 5,
         standing = standing * 5,
         debris = debris * 5) %>% 
  # Tidy columns
  select(year, species, spacing, yc, thin, standing, debris, mgmt, removed, annual_total) 

# Get thin params from Oak (for SAB)
thin_params_ok <- inner_join(wcc_data_5y %>% 
                               filter(thin == "Thinned") %>% 
                               select(-thin),
                             wcc_data_5y %>% 
                               filter(thin == "NO_thin") %>% 
                               select(-thin), 
                             by = c("year", "species", "spacing", "yc"), suffix = c("_thin", "_nothin")) %>% 
  filter(species == "OK" & yc == 8) %>%
  mutate(d_annual = annual_total_nothin - annual_total_thin,
         d_standing = standing_nothin - standing_thin,
         d_debris = debris_nothin - debris_thin,
         standing_prop = d_standing/d_annual,
         debris_prop = d_debris/d_annual,
         removed_prop = removed_thin/d_annual,
         mgmt = mgmt_thin) %>% 
  select(year, standing_prop, debris_prop, removed_prop, mgmt)


# Apply Oak thin params to SAB to estimate debris, standing and removed
wcc_data_5y <- inner_join(wcc_data_5y %>% 
                            filter(thin == "Thinned") %>% 
                            select(-thin),
                          wcc_data_5y %>% 
                            filter(thin == "NO_thin") %>% 
                            select(-thin), 
                          by = c("year", "species", "spacing", "yc"), suffix = c("_thin", "_nothin")) %>% 
  filter(species == "SAB" & yc > 2 & yc < 12) %>%
  # Get difference in annual flux between thin/no thin 
  mutate(d_annual = annual_total_nothin - annual_total_thin) %>% 
  # Join with Oak thin params
  left_join(thin_params_ok, by = "year") %>%
  # Estimate standing, debris and removed 
  mutate(standing = ifelse(is.na(standing_prop), standing_nothin, standing_nothin - (standing_prop * d_annual)),
         debris = ifelse(is.na(debris_prop), debris_nothin, debris_nothin - (debris_prop * d_annual)),
         removed = ifelse(is.na(removed_prop), 0, removed_prop * d_annual),
         thin = "Thinned") %>% 
  select(year, species, spacing, yc, thin, standing, debris, mgmt, removed) %>% 
  # Join with rest of species
  bind_rows(filter(wcc_data_5y, !(species == "SAB" & thin == "Thinned") & !species %in% c("OK", "EL"))) %>%
  # Divide /yr variables by 5 again
  mutate(standing = standing / 5,
         debris = debris / 5,
         removed = removed / 5) %>%
  # Tidy
  rename(removed_thin = removed) %>% 
  select(-annual_total, -mgmt)

# Add year 0 
wcc_data_5y <- wcc_data_5y %>%
  select(species, spacing, yc, thin) %>% 
  unique() %>% 
  mutate(year = 0) %>% 
  bind_rows(wcc_data_5y) %>%
  arrange(species, spacing, yc, thin, year) %>% 
  # Everything = 0 in year 0
  mutate_at(vars(standing, debris, removed_thin), list(~ifelse(year == 0, 0, .)))


# Interpolate all years
wcc_data_ann <- wcc_data_5y %>%
  # Cumulative debris & standing (*5)
  group_by(species, spacing, yc, thin) %>% 
  mutate(cumulative_standing = cumsum(standing * 5),
         cumulative_debris = cumsum(debris * 5)) %>% 
  ungroup() %>% 
  # Add all years 0:200
  full_join(wcc_data_5y %>%
              select(species, spacing, yc, thin) %>%
              unique() %>%
              crossing(year = 0:200),
            by = c("species", "spacing", "yc", "thin", "year")) %>%
  arrange(species, spacing, yc, thin, year) %>%
  # Interpolate
  group_by(species, spacing, yc, thin) %>%
  mutate_at(vars(cumulative_standing, cumulative_debris), list(~zoo::na.approx(.))) %>% 
  # Thinning occurs in intervals - don't interpolate (but * 5) 
  mutate(removed_thin = ifelse(is.na(removed_thin), 0, removed_thin * 1)) %>% 
  # Annual standing & debris - derive from cumulative
  mutate(standing = cumulative_standing - lag(cumulative_standing, default = 0),
         debris = cumulative_debris - lag(cumulative_debris, default = 0)) %>% 
  ungroup() 




## CLEARFELL ROTATIONS ---------------------------------
# Read in rotation length - year of maximum MAI (nearest decade); varies according to species, YC and thinning regime
rotation_lengths <- read_csv("data/ghg/wood/rotation_lengths.csv", skip = 1) %>% select(-mai, -notes) 

# Drop un-used combinations
rotation_lengths <- rotation_lengths %>%  
  filter(!is.na(type)) %>% 
  rename(rotation_length = age) %>% 
  select(-thin_details, -spacing)

# Add rotation cycles
wcc_data_ann <- wcc_data_ann %>%
  select(species, spacing, yc, thin, year) %>%
  # Add rotation lengths
  inner_join(rotation_lengths, by = c("species", "yc", "thin")) %>% 
  group_by(species, yc, thin, rotation_length) %>%
  # Add running stand age, assuming 3-year fallow after clearfell
  mutate(age = rep(c(seq(0, unique(rotation_length), by = 1), rep(0, 2)), 9)[1:201]) %>% 
  ungroup() %>%  
  select(-rotation_length) %>% 
  # Join with sequestration rate - with 'year' resetting with each rotation 
  left_join(wcc_data_ann, by = c("species", "spacing", "yc", "thin", "age" = "year")) %>%
  # Extract cumulative standing carbon in clearfell year
  group_by(species, type, yc, thin) %>% 
  mutate(clearfell_stock = ifelse(age == max(age), cumulative_standing, 0),
         clearfelled = TRUE,
         cumulative_standing = ifelse(clearfell_stock == 0, cumulative_standing, 0),
         removed_thin = ifelse(clearfell_stock == 0, removed_thin, 0)) %>% 
  ungroup() %>% 
  # Combine with semi-natural (no clearfell rotation) 
  bind_rows(wcc_data_ann %>% 
              filter(species %in% c("SP", "SAB") & thin == "Thinned") %>% 
              mutate(age = year,
                     clearfell_stock = 0,
                     clearfelled = FALSE,
                     type = case_when(species == "SP" ~ "Semi-natural coniferous woodland", 
                                      species == "SAB" ~ "Semi-natural broadleaved woodland"))) %>% 
  bind_rows(wcc_data_ann %>%
              filter(species %in% c("SP", "SAB") & thin == "NO_thin") %>%
              mutate(age = year,
                     clearfell_stock = 0,
                     clearfelled = FALSE,
                     type = case_when(species == "SP" ~ "Semi-natural coniferous woodland (unmanaged)",
                                      species == "SAB" ~ "Semi-natural broadleaved woodland (unmanaged)")))



## ADD EARLY FELLING EVENTS --------------
wcc_data_ann <- wcc_data_ann %>%
  crossing(year_felled = 0:200) %>% 
  mutate(clearfell_stock = case_when(year == year_felled ~ cumulative_standing,
                                     year > year_felled ~ 0,
                                     TRUE ~ clearfell_stock),
         cumulative_standing = ifelse(year < year_felled, cumulative_standing, 0),
         cumulative_debris = ifelse(year < year_felled, cumulative_debris, 0),
         removed_thin = ifelse(year < year_felled, removed_thin, 0)) %>% 
  bind_rows(wcc_data_ann %>% mutate(year_felled = NA), 
            .)


## HWP STOCKS ------------------------
# Read in HWP parameters
hwp_params <- read_csv("data/ghg/wood/hwp.csv", skip = 1) %>% select(-notes)

# Calculate stock change each year post-felling
hwp_stock <- hwp_params %>%
  # Sum proportion across products
  group_by(species, activity, product, hwp_residence, hwp_decay) %>% 
  summarise(frac = sum(prop_biomass * prop_removed * prop_harvest * prop_product), .groups = "drop") %>%
  group_by(species, activity) %>% 
  mutate(frac = ifelse(activity == "thin", frac / sum(frac), frac),
         frac = ifelse(is.na(frac), 0, frac)) %>%
  ungroup() %>% 
  # Get all years 0-200 (max length assuming felling in 1890)
  crossing(year = 0:200) %>% 
  # Calculate stock in each year since felling
  group_by(species, activity, product) %>% 
  mutate(stock = case_when(year <= hwp_residence ~ frac,
                           year > hwp_residence & hwp_decay == 1 ~ 0,
                           year > hwp_residence & hwp_decay > 1 ~ frac * exp((year - hwp_residence) / (hwp_decay / log(0.05))))) %>%
  # Sum across products
  group_by(year, species, activity) %>% 
  summarise(stock = sum(stock), .groups = "drop") %>% 
  # Join with species types
  left_join(tibble(type = unique(wcc_data_ann$type),
                   species = c("AF", "SBI", "AF", "AF", "SRF", "SS", "SBI", "SP", "SBI", "SP")),
            by = "species") %>% 
  select(-species)


# Function for stacking HWP stock across multiple rotations
stock_decay_fun <- function(d, type, activity, stock){
  # s <- filter(stock, type == gsub(" (over-mature)", "", !!type, fixed = TRUE) & activity == !!activity)$stock
  if(sum(d) == 0){
    return(rep(0, length(d)))
  } else{
    s <- filter(stock, type == unique(!!type) & activity == unique(!!activity))$stock
    
    n <- sum(d != 0)
    inds <- which(d != 0)
    x <- rep(0, length(s))
    
    for(i in 1:n){
      x <- x + d[inds[i]] * c(rep(0, inds[i] - 1), s)[1:length(x)] 
    }
    return(x[1:length(d)])
    
  }
}



# LITTER & DEBRIS STCOCKS ---------------------------
# Read in debris parameters
debris_params <- read_csv("data/ghg/wood/debris.csv", skip = 1) %>% select(-notes)

# Calculation stock decay following clearfell
debris_stock <- debris_params %>% 
  # Calculate fraction of standing biomass going to coarse, fine and non-woody debris
  group_by(species, activity, debris_component, debris_residence, debris_decay) %>% 
  summarise(frac = sum(prop_biomass * prop_harvest * (1 - prop_removed) * prop_debris)) %>% 
  ungroup() %>%   
  # Get all years 0-200 (max length assuming felling in 1890)
  crossing(year = 0:200) %>% 
  # Calculate stock in each year since felling
  group_by(species, activity, debris_component) %>% 
  mutate(stock = case_when(year <= debris_residence ~ frac,
                           year > debris_residence & debris_decay == 1 ~ 0,
                           # year > debris_residence & debris_decay > 1 & year <= debris_residence + debris_decay ~ frac - frac * ((year - debris_residence) / debris_decay),
                           # TRUE ~ 0)) %>%
                           year > debris_residence & debris_decay > 1 ~ frac * exp((year - debris_residence) / (debris_decay / log(0.05))))) %>%
  # Sum across products
  group_by(year, species, activity) %>% 
  summarise(stock = sum(stock)) %>% 
  ungroup() %>% 
  # Join with species types
  left_join(tibble(type = unique(wcc_data_ann$type),
                   species = c("AF", "SBI", "AF", "AF", "SRF", "SS", "SBI", "SP", "SBI", "SP")),
            by = "species") %>% 
  select(-species)



## NET ANNUAL AND CUMULATIVE EMISSIONS ---------------------------------
# Calculate remaining HWP stock (from subsequent clearfell)
ghg_wood_ann <- wcc_data_ann %>% 
  mutate(thin = ifelse(type == "Silvoarable agroforestry (apples)", "NO_thin", thin)) %>% # Clearfell occurs prior to first thin
  group_by(species, yc, type, clearfelled, thin, year_felled) %>%
  arrange(species, yc, type, clearfelled, thin, year_felled, year, age) %>% 
  mutate(hwp_stock_clearfell = ifelse(clearfelled,
                                      stock_decay_fun(clearfell_stock, type, "clearfell", hwp_stock),
                                      0),
         hwp_flux_clearfell = hwp_stock_clearfell - lag(hwp_stock_clearfell),
         hwp_flux_clearfell = ifelse(is.na(hwp_flux_clearfell), 0, hwp_flux_clearfell)) %>%
  # Calculate remaining HWP stock (from thinning)
  mutate(hwp_stock_thin = ifelse(thin == "Thinned",
                                 stock_decay_fun(removed_thin, type, "thin", hwp_stock),
                                 0),
         hwp_flux_thin = hwp_stock_thin - lag(hwp_stock_thin),
         hwp_flux_thin = ifelse(is.na(hwp_flux_thin), 0, hwp_flux_thin)) %>% 
  # Calculate debris stock (from subsequent clearfell)
  mutate(debris_stock_clearfell = ifelse(clearfelled, 
                                         stock_decay_fun(clearfell_stock, type, "clearfell", debris_stock),
                                         0),
         debris_flux_clearfell = debris_stock_clearfell - lag(debris_stock_clearfell),
         debris_flux_clearfell = ifelse(is.na(debris_flux_clearfell), 0, debris_flux_clearfell)) %>%
  # Calculate net annual flux
  mutate(cumulative_bm = cumulative_standing + cumulative_debris,
         annual_flux_bm = cumulative_bm - lag(cumulative_bm),
         annual_flux_bm = ifelse(is.na(annual_flux_bm), 0, annual_flux_bm),
         annual_flux = annual_flux_bm + hwp_flux_clearfell + hwp_flux_thin + debris_flux_clearfell) %>% 
  ungroup() %>%
  select(-species, -spacing, -age, -standing, -debris)


# Add natregen - delay by 5 years (change yc later)
ghg_wood_ann <- bind_rows(ghg_wood_ann %>% 
                            filter(grepl("Semi-natural", type)) %>% 
                            filter(year == 0) %>%
                            select(-year) %>% 
                            crossing(year = 0:4),
                          ghg_wood_ann %>% 
                            filter(grepl("Semi-natural", type)) %>% 
                            mutate(year = year + 5) %>% 
                            filter(year <= 200)) %>% 
  mutate(natregen = 1) %>% 
  bind_rows(ghg_wood_ann %>% 
              mutate(natregen = 0))


## EXISTING WOODLAND AGE ------------------------
# Get woodland age distribution 
existing_wood_age <- read_csv("data/ghg/wood/wood_age_distribution.csv", skip = 1) %>% 
  select(-source, -units, -year) 

# Assume NI age distribution is equivalent to GB average
existing_wood_age <- existing_wood_age %>% 
  group_by(var, age_class, age) %>% 
  summarise(country = "Northern Ireland",
            val = sum(val)) %>% 
  ungroup() %>% 
  bind_rows(existing_wood_age)

# Calculate age distribution 
existing_wood_age <- existing_wood_age %>%
  # Add rows for all years
  full_join(tibble(age = seq(0, 110, by = 1),
                   age_class = c(rep(c("0-20", "21-40", "41-60", "61-80", "81-100"), each = 20), rep("100+", 11))) %>% 
              crossing(country = unique(existing_wood_age$country),
                       var = unique(existing_wood_age$var)), 
            by = c("var", "age_class", "age", "country")) %>%
  arrange(age) %>%
  group_by(country, var, age_class) %>%
  # Replicate age_class area across each year within that class, and divide across years in period
  mutate(val = unique(val[!is.na(val)]) / length(val)) %>%
  # Calculate proportion
  group_by(country, var) %>% 
  mutate(prop = val/sum(val)) %>% 
  ungroup() %>% 
  # Calculate establishment year (assume 2015 baseline)
  mutate(cohort = 2015 - age) %>%
  # Tidy
  mutate(lcm = ifelse(var == "Broadleaved woodland", 1, 2)) %>% 
  # rename('woodland_type' = var) %>% 
  select(-age_class, -val, -var) %>% 
  arrange(country, lcm, cohort) 

# Duplicate for pinewood, change to LCM codes
existing_wood_age <- existing_wood_age %>% 
  mutate(lcm = ifelse(lcm == 1, "b.w", "c.w")) %>% 
  bind_rows(existing_wood_age %>% 
              filter(lcm == 2 & country == "Scotland") %>% 
              mutate(lcm = "c.w_pinewood"))


## HWP fracs -----------
# For estimating HWP supply
# Calculate frac as board/timber/fuel
hwp_fracs <- hwp_params %>%
  select(species, activity, biomass_component, prop_biomass, prop_removed, prop_harvest, harvest_component) %>% 
  unique() %>% 
  group_by(species, activity) %>% 
  mutate(prop_biomass = prop_biomass / sum(unique(prop_biomass)),
         prop = prop_biomass * prop_removed,
         prop = ifelse(biomass_component == "stem", prop * prop_harvest, prop),
         prop = prop / sum(prop)) %>% 
  ungroup() %>%
  select(-prop_biomass, -prop_removed, -prop_harvest) %>% 
  left_join(hwp_params %>% select(-prop_biomass, -prop_removed, -prop_harvest, -hwp_residence, -hwp_decay), by = c("species", "activity", "biomass_component", "harvest_component")) %>% 
  mutate(prop = prop * prop_product) %>% 
  select(-prop_product) %>% 
  group_by(species, activity, product) %>% 
  summarise(prop = sum(prop), .groups = "drop") %>% 
  rename(hwp_type = product)


## SAVE ------------------
ghg_pars_wood <- list(ghg_wood_ann = ghg_wood_ann, 
                      existing_wood_age = existing_wood_age, 
                      stock_decay_fun = stock_decay_fun, 
                      debris_stock = debris_stock, 
                      hwp_stock = hwp_stock,
                      hwp_fracs = hwp_fracs)

# save(ghg_pars_wood, file = "rdata/ghg/ghg_pars_wood.RData")
save(ghg_pars_wood, file = "rdata/ghg/ghg_pars_wood_2023.RData")