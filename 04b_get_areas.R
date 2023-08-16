# INIT ----------------
source("setup.R")


## READ IN BASELINE RASTERS --------------------------

##
## Crop grassclass and agcensid
##

# LCM - 2015
r_lcm_nir <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_nir.tif")
r_lcm_eng <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_eng.tif")
r_lcm_wal <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_wal.tif")
r_lcm_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_sct.tif")

# Native pinewood
r_pinewood_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_pinewood_sct.tif")
r_lcm_sct[r_pinewood_sct[] == 1] <- 2.1

# LCM - baseline scenario
r_lcm_a_nir <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_nir.tif")
r_lcm_a_eng <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_eng.tif")
r_lcm_a_wal <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_wal.tif")
r_lcm_a_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_sct.tif")

# Lookups
load("rdata/raster_lookup_lcm.RData")

# Add novel land covers
lookup_lcm <- lookup_lcm %>% 
  bind_rows(tibble(lcm_layer = c(2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 9.1, 10.1, 3.2, 4.2, 3.3, 4.3, 3.31, 4.31, 3.4, 4.4, 5.2, 6.2, 7.2),
                   lcm = c("c.w_pinewood", "a.h_silvoa", "i.g_silvop", "n.g_woodpa", "c.g_woodpa", "a.g_woodpa", "h.r_woodpa", "h.r_woodpa", "a.h_energy", "i.g_energy", "a.h_organic", "i.g_organic", "a.h_organic_silvoa", "i.g_organic_silvop", "a.h_palud", "i.g_palud", "n.g_new", "c.g_new", "a.g_new")))

# ALC
r_alc_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc_nir.tif")
r_alc_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc2_eng.tif") # New version
r_alc_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc_wal.tif")
r_alc_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc_sct.tif")
load("rdata/raster_lookup_alc2.RData") # New version

# NUTS1
r_nuts1_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_nuts1_nir.tif")
r_nuts1_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_nuts1_eng.tif")
r_nuts1_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_nuts1_wal.tif")
r_nuts1_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_nuts1_sct.tif")
load("rdata/raster_lookup_nuts1.RData")

# Peat
r_peat_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_nir.tif")
r_peat_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_eng.tif")
r_peat_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_wal.tif")
r_peat_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_sct.tif")

# Woodop
r_woodop_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_woodop_eng.tif")
r_woodop_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_woodop_sct.tif")
r_woodop_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_woodop_wal.tif")
r_woodop_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_woodop_nir.tif")

# Waders
r_waders_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_eng.tif")
r_waders_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_sct.tif")
r_waders_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_wal.tif")
r_waders_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_nir.tif")

# Yield class
r_cyc_eng <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_cyc_eng.tif")[[c(6:9)]]
r_cyc_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_cyc_sct.tif")[[c(6:9)]]
r_cyc_wal <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_cyc_wal.tif")[[c(6:9)]]
r_cyc_nir <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_cyc_nir.tif")[[c(6:9)]]

names(r_cyc_eng) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")
names(r_cyc_sct) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")
names(r_cyc_wal) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")
names(r_cyc_nir) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")

# Nat regen
r_natregen_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_natregen_eng.tif")
r_natregen_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_natregen_sct.tif")
r_natregen_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_natregen_wal.tif")
r_natregen_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_natregen_nir.tif")

# Soil
r_soil_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_soiltype_eng.tif")
r_soil_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_soiltype_sct.tif")
r_soil_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_soiltype_wal.tif")
r_soil_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_soiltype_nir.tif")


## DEFINE FUNCTION --------------------------
get_areas_fun <- function(r_lcm_stack, r_lcm, r_nuts1, r_alc, r_peat, r_woodop, r_waders, r_cyc, r_natregen, r_soil,
                          lookup_lcm, lookup_nuts1, lookup_alc, 
                          .scenario, .country){
  options(dplyr.summarise.inform = FALSE)
  r_lcm_stack <- stack(r_lcm, r_lcm_stack)
  t <- nlayers(r_lcm_stack)
  
  ##### Cross-tabulate areas in each period
  print("Cross-tabulating areas...")
  
  ## Stack fixed spatial variables
  r_vars_stack <- stack(r_nuts1, r_alc, r_peat, r_woodop, r_waders, r_cyc, r_natregen, r_soil)
  
  ## Fix names
  names(r_lcm_stack) <- paste0("y", 2015 + 5 * seq(0, t - 1, by = 1))
  names(r_vars_stack)[c(1:5, 10, 11)] <- c("nuts1_layer", "alc_layer", "peat", "woodop", "waders", "natregen", "soil")
  
  ## Crosstabulate areas
  # LCM
  df_lcm_5y <- crosstabDT(stack(r_lcm_stack, r_vars_stack), digits = 2, long = TRUE, useNA = TRUE)
  
  # Tidy
  df_lcm_5y <- df_lcm_5y %>%
    as_tibble() %>% 
    filter(!is.na(y2015)) %>% 
    mutate_at(vars(woodop, waders), ~ifelse(is.na(.), 0, .))
  
  
  ## Extract annual areas
  areas <- df_lcm_5y %>% 
    as_tibble() %>% 
    gather(year, lcm_layer, -nuts1_layer, -alc_layer, -peat, -woodop, -waders, -yc_sbi, -yc_sok, -yc_sp, -yc_ss, -natregen, -soil, -Freq) %>% 
    filter(!is.na(lcm_layer)) %>% 
    mutate(year = as.integer(gsub("y", "", year)),
           area = Freq * 0.0625,
           country = .country, 
           scenario = .scenario) %>% 
    select(country, scenario, year, lcm_layer, nuts1_layer, alc_layer, peat, woodop, waders, area, yc_sbi, yc_sok, yc_sp, yc_ss, natregen, soil) %>% 
    left_join(lookup_lcm, by = "lcm_layer") %>% 
    left_join(lookup_nuts1, by = "nuts1_layer") %>% 
    left_join(lookup_alc, by = c("alc_layer", "country")) %>% 
    select(-contains("layer"))
  
  
  
  ##### Calculate land-use change areas
  if(t > 1){
    print("Calculating land-use change areas...")
    
    ## Empty list
    change_areas <- as.list(rep(NA, t - 1))
    
    for(i in 1:(t - 1)){
      change_areas[[i]] <- df_lcm_5y[, c(i, i+1, t+1:12)] %>%
        set_names(c("lcm_from", "lcm_to", names(.)[-c(1, 2)])) %>%
        mutate(country = .country) %>% 
        left_join(lookup_lcm, by = c('lcm_from' = 'lcm_layer')) %>% 
        left_join(lookup_lcm, by = c('lcm_to' = 'lcm_layer')) %>% 
        left_join(lookup_nuts1, by = "nuts1_layer") %>% 
        left_join(lookup_alc, by = c("alc_layer", "country")) %>% 
        select(-contains("layer"), -lcm_from, -lcm_to) %>% 
        rename("lcm_from" = lcm.x, "lcm_to" = lcm.y) %>% 
        mutate(year = 2015 + i * 5) %>%
        group_by(year, lcm_from, lcm_to, country, peat, woodop, waders, nuts1, alc, yc_sbi, yc_sok, yc_sp, yc_ss, natregen, soil) %>% 
        summarise(area = sum(Freq * 0.0625)) %>% 
        ungroup() %>% 
        mutate(scenario = .scenario)
    }
    
    change_areas <- bind_rows(change_areas)
    
    #### Return
    print("Done!")
    return(list(areas = areas,
                change_areas = change_areas))  
  } else{
    
    #### Return
    print("Done!")
    return(areas = areas)  
  }
}




# GET BASELINE (2015) AREAS -----------------------------------
areas_baseline_nir <- get_areas_fun(NA, r_lcm_nir, r_nuts1_nir, r_alc_nir, r_grassclass_nir, r_peat_nir, r_agcensid_nir, r_cyc_nir, r_natregen_nir, r_soil_nir,
                                    lookup_lcm, lookup_nuts1, lookup_alc, lookup_grassclass,
                                    "Baseline", "Northern Ireland")
areas_baseline_eng <- get_areas_fun(NA, r_lcm_eng, r_nuts1_eng, r_alc_eng, r_grassclass_eng, r_peat_eng, r_agcensid_eng, r_cyc_eng, r_natregen_eng, r_soil_eng,
                                    lookup_lcm, lookup_nuts1, lookup_alc, lookup_grassclass,
                                    "Baseline", "England")
areas_baseline_wal <- get_areas_fun(NA, r_lcm_wal, r_nuts1_wal, r_alc_wal, r_grassclass_wal, r_peat_wal, r_agcensid_wal, r_cyc_wal, r_natregen_wal, r_soil_wal,
                                    lookup_lcm, lookup_nuts1, lookup_alc, lookup_grassclass,
                                    "Baseline", "Wales")
areas_baseline_sct <- get_areas_fun(NA, r_lcm_sct, r_nuts1_sct, r_alc_sct, r_grassclass_sct, r_peat_sct, r_agcensid_sct, r_cyc_sct, r_natregen_sct, r_soil_sct,
                                    lookup_lcm, lookup_nuts1, lookup_alc, lookup_grassclass,
                                    "Baseline", "Scotland")
areas_baseline <- bind_rows(areas_baseline_nir,
                            areas_baseline_eng,
                            areas_baseline_wal,
                            areas_baseline_sct)

save(areas_baseline, file = "rdata/scenarios/areas_baseline.RData")


# GET SCENARIO AREAS -----------------------------------
# Get list of rasters
files <- list.files("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios") %>% 
  grep(pattern = '5y', invert = TRUE, value = TRUE) %>%
  grep(pattern = 'hedgelength', invert = TRUE, value = TRUE) %>%
  grep(pattern = 'networks', invert = TRUE, value = TRUE) %>%
  grep(pattern = '2022', value = TRUE)


# List input rasters
r_lcm_all <- lapply(paste0("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/", files), stack)
names(r_lcm_all) <- gsub(".tif", "", files)


r_input_all <- list('r_lcm_eng' = r_lcm_eng,
                    'r_lcm_sct' = r_lcm_sct,
                    'r_lcm_wal' = r_lcm_wal,
                    'r_lcm_nir' = r_lcm_nir,
                    'r_nuts1_eng' = r_nuts1_eng,
                    'r_nuts1_sct' = r_nuts1_sct,
                    'r_nuts1_wal' = r_nuts1_wal,
                    'r_nuts1_nir' = r_nuts1_nir,
                    'r_alc_eng' = r_alc_eng,
                    'r_alc_sct' = r_alc_sct,
                    'r_alc_wal' = r_alc_wal,
                    'r_alc_nir' = r_alc_nir,
                    'r_peat_eng' = r_peat_eng,
                    'r_peat_sct' = r_peat_sct,
                    'r_peat_wal' = r_peat_wal,
                    'r_peat_nir' = r_peat_nir,
                    'r_woodop_eng' = r_woodop_eng,
                    'r_woodop_sct' = r_woodop_sct,
                    'r_woodop_wal' = r_woodop_wal,
                    'r_woodop_nir' = r_woodop_nir,
                    'r_waders_eng' = r_waders_eng,
                    'r_waders_sct' = r_waders_sct,
                    'r_waders_wal' = r_waders_wal,
                    'r_waders_nir' = r_waders_nir,
                    'r_cyc_eng' = r_cyc_eng,
                    'r_cyc_sct' = r_cyc_sct,
                    'r_cyc_wal' = r_cyc_wal,
                    'r_cyc_nir' = r_cyc_nir,
                    'r_natregen_eng' = r_natregen_eng,
                    'r_natregen_sct' = r_natregen_sct,
                    'r_natregen_wal' = r_natregen_wal,
                    'r_natregen_nir' = r_natregen_nir,
                    'r_soil_eng' = r_soil_eng,
                    'r_soil_sct' = r_soil_sct,
                    'r_soil_wal' = r_soil_wal,
                    'r_soil_nir' = r_soil_nir)


# Pull out properties from filename
areas <- tibble(lcm_stack = gsub(".tif", "", files),
                scenario = case_when(grepl("waders", files) & ! grepl("soil", files) ~ paste0(substr(files, 7, 7), "_waders"),
                                     grepl("soil", files) ~ paste0(substr(files, 7, 7), "_wadersoil"),
                                     TRUE ~ substr(files, 7, 7)),
                country = case_when(grepl("waders", files) & ! grepl("soil", files) ~ substr(files, 16, 18),
                                    grepl("soil", files) ~ substr(files, 19, 21),
                                    TRUE ~ substr(files, 9, 11)),
                lcm = paste0("r_lcm_", country),
                nuts1 = paste0("r_nuts1_", country),
                alc = paste0("r_alc_", country),
                peat = paste0("r_peat_", country),
                woodop = paste0("r_woodop_", country),
                waders = paste0("r_waders_", country),
                cyc = paste0("r_cyc_", country),
                natregen = paste0("r_natregen_", country),
                soil = paste0("r_soil_", country),
                country_full = case_when(country == "eng" ~ "England",
                                         country == "sct" ~ "Scotland",
                                         country == "wal" ~ "Wales",
                                         country == "nir" ~ "Northern Ireland"),
                i = 1:length(files)) 



areas_nir <- areas %>% filter(country == "nir") %>% group_by(i) %>% nest() %>% 
  mutate(out = purrr::map(data, ~get_areas_fun(r_lcm_stack = r_lcm_all[[.$lcm_stack]], r_lcm = r_input_all[[.$lcm]], 
                                               r_nuts1 = r_input_all[[.$nuts1]], r_alc = r_input_all[[.$alc]], r_peat = r_input_all[[.$peat]],
                                               r_woodop = r_input_all[[.$woodop]], r_waders = r_input_all[[.$waders]],
                                               r_cyc = r_input_all[[.$cyc]], r_natregen = r_input_all[[.$natregen]], r_soil = r_input_all[[.$soil]],
                                               lookup_lcm = lookup_lcm, lookup_nuts1 = lookup_nuts1, lookup_alc = lookup_alc,
                                               .scenario = .$scenario, .country = .$country_full)))


areas_wal <- areas %>% filter(country == "wal") %>% group_by(i) %>% nest() %>% 
  mutate(out = purrr::map(data, ~get_areas_fun(r_lcm_stack = r_lcm_all[[.$lcm_stack]], r_lcm = r_input_all[[.$lcm]], 
                                               r_nuts1 = r_input_all[[.$nuts1]], r_alc = r_input_all[[.$alc]], r_peat = r_input_all[[.$peat]],
                                               r_woodop = r_input_all[[.$woodop]], r_waders = r_input_all[[.$waders]],
                                               r_cyc = r_input_all[[.$cyc]], r_natregen = r_input_all[[.$natregen]], r_soil = r_input_all[[.$soil]],
                                               lookup_lcm = lookup_lcm, lookup_nuts1 = lookup_nuts1, lookup_alc = lookup_alc,
                                               .scenario = .$scenario, .country = .$country_full)))

areas_sct <- areas %>% filter(country == "sct") %>% group_by(i) %>% nest() %>% 
  mutate(out = purrr::map(data, ~get_areas_fun(r_lcm_stack = r_lcm_all[[.$lcm_stack]], r_lcm = r_input_all[[.$lcm]], 
                                               r_nuts1 = r_input_all[[.$nuts1]], r_alc = r_input_all[[.$alc]], r_peat = r_input_all[[.$peat]],
                                               r_woodop = r_input_all[[.$woodop]], r_waders = r_input_all[[.$waders]],
                                               r_cyc = r_input_all[[.$cyc]], r_natregen = r_input_all[[.$natregen]], r_soil = r_input_all[[.$soil]],
                                               lookup_lcm = lookup_lcm, lookup_nuts1 = lookup_nuts1, lookup_alc = lookup_alc,
                                               .scenario = .$scenario, .country = .$country_full)))

areas_eng <- areas %>% filter(country == "eng") %>% group_by(i) %>% nest() %>% 
  mutate(out = purrr::map(data, ~get_areas_fun(r_lcm_stack = r_lcm_all[[.$lcm_stack]], r_lcm = r_input_all[[.$lcm]], 
                                               r_nuts1 = r_input_all[[.$nuts1]], r_alc = r_input_all[[.$alc]], r_peat = r_input_all[[.$peat]],
                                               r_woodop = r_input_all[[.$woodop]], r_waders = r_input_all[[.$waders]],
                                               r_cyc = r_input_all[[.$cyc]], r_natregen = r_input_all[[.$natregen]], r_soil = r_input_all[[.$soil]],
                                               lookup_lcm = lookup_lcm, lookup_nuts1 = lookup_nuts1, lookup_alc = lookup_alc,
                                               .scenario = .$scenario, .country = .$country_full)))


areas <- bind_rows(areas_nir %>% ungroup(), 
                   areas_wal %>% ungroup(), 
                   areas_sct %>% ungroup(),
                   areas_eng %>% ungroup())

rm(areas_nir, areas_wal, areas_sct, areas_eng)

# Extract scenario areas 
scenarios_areas <- areas %>%
  ungroup() %>% 
  mutate(areas = purrr::map(out, ~.[[1]])) %>% 
  select(areas) %>% 
  unnest(cols = c(areas)) 

# Extract change areas
scenarios_change_areas <- areas %>%
  ungroup() %>% 
  mutate(areas = purrr::map(out, ~.[[2]])) %>% 
  select(areas) %>% 
  unnest(cols = c(areas)) 


# Get ambition combinations
ambition_combos <- read_csv("data/ambition_combinations_new.csv", skip = 0)

# Add unique spatial scenario code (letters)
ambition_combos <- ambition_combos %>%
  group_by_at(vars(ambition_saltmarsh, ambition_peat, ambition_wood, ambition_grass, ambition_agroforestry, ambition_hedge, ambition_bioenergy, ambition_organic)) %>%
  mutate(scenario_2 = paste0(scenario, collapse = "_")) %>% 
  ungroup() %>% 
  mutate(scenario_2 = letters[as.integer(as.factor(scenario_2))],
         scenario = as.character(scenario)) %>% 
  select(name, scenario, scenario_2, everything())



# Set DT
setDT(scenarios_areas)
setDT(scenarios_change_areas)

scenarios_areas <- scenarios_areas[, scenario_2 := scenario][
  , scenario := NULL]
scenarios_change_areas <- scenarios_change_areas[, scenario_2 := scenario][
  , scenario := NULL]

scenarios_areas <- setDT(select(ambition_combos, name, scenario, scenario_2))[scenarios_areas, on = "scenario_2"]
scenarios_change_areas <- setDT(select(ambition_combos, name, scenario, scenario_2))[scenarios_change_areas, on = "scenario_2"]

scenarios_areas <- scenarios_areas[, !c("scenario_2")]
scenarios_change_areas <- scenarios_change_areas[, !c("scenario_2")]

scenarios_areas %>% select(scenario, name) %>% unique


# SAVE
save(scenarios_areas, scenarios_change_areas, file = "rdata/scenarios/scenarios_areas_2022.RData")
