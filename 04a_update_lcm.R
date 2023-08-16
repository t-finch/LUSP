source("setup.R")


## 5-year RASTERS --------------------
# Load 5-y raster with urban growth
r_lcm_5y_nir <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_nir.tif")
r_lcm_5y_wal <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_wal.tif")
r_lcm_5y_eng <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_eng.tif")
r_lcm_5y_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_sct.tif")

r_lcm_5y_nir <- r_lcm_5y_nir[[c(2:8)]]
r_lcm_5y_wal <- r_lcm_5y_wal[[c(2:8)]]
r_lcm_5y_eng <- r_lcm_5y_eng[[c(2:8)]]
r_lcm_5y_sct <- r_lcm_5y_sct[[c(2:8)]]

names(r_lcm_5y_nir) <- paste0("y", seq(2020, 2050, by = 5))#2015, 2100, by = 5))
names(r_lcm_5y_wal) <- paste0("y", seq(2020, 2050, by = 5))#2015, 2100, by = 5))
names(r_lcm_5y_eng) <- paste0("y", seq(2020, 2050, by = 5))#2015, 2100, by = 5))
names(r_lcm_5y_sct) <- paste0("y", seq(2020, 2050, by = 5))#2015, 2100, by = 5))

# LCM lookup
lcm_lookup <- read_csv("data/lcm/lcm_lookup.csv") %>% select(-full2) %>% set_names(c("lcm", "lcm_code", "lcm_full"))


## SCENARIO PARAMETERS ------------------------
# Ambition combinations
ambition_combos <- read_csv("data/ambition_combinations_new.csv", skip = 0)

# Load parameter file
scenario_params <- read_csv("data/scenario_parameters_new.csv", skip = 1)

# Load country props
load("rdata/scenarios/country_props_2022.RData")

# Intertidal habitat
params_saltmarsh <- scenario_params %>% 
  filter(par == "intertidal_rate") %>%
  rename('ha' = val) %>% 
  select(ha, ambition) %>% 
  left_join(scenario_params %>%
              filter(par == "intertidal_stopyear") %>% 
              select(ambition, val) %>% 
              rename('stopyear' = val), 
            by = "ambition")

# Peat
params_peat <- scenario_params %>%
  filter(grepl("peat", par) & par != "peat_stopyear") %>% 
  inner_join(tibble(par = c("peat_forest", "peat_lowlandgrass", "peat_lowlandgrass_paludiculture", "peat_lowlandcrop", "peat_lowlandcrop_paludiculture", "peat_upland"),
                    peatclass = c("forest", "improved_grass", "improved_grass", "crop", "crop", "extensive_grass"),
                    peatman = c("forest", "improved_grass", "grass_palud", "crop", "crop_palud", "extensive_grass")),
             by = "par") %>% 
  rename('frac' = val) %>% 
  select(peatclass, peatman, frac, ambition) %>% 
  left_join(scenario_params %>%
              filter(par == "peat_stopyear") %>% 
              select(ambition, val) %>% 
              rename('stopyear' = val), 
            by = "ambition") %>% 
  left_join(scenario_params %>%
              filter(par == "peat_foreststopyear") %>% 
              select(ambition, val) %>% 
              rename('foreststopyear' = val), 
            by = "ambition") %>% 
  mutate(stopyear = ifelse(peatclass == "forest", foreststopyear, stopyear)) %>% 
  select(-foreststopyear)

# Woodland
params_wood <- scenario_params %>%
  filter(grepl("wood_", par)) %>%
  select(-note) %>% 
  spread(par, val) %>% 
  gather(year, ha, -ambition, -wood_paws, -wood_prop_bw, -wood_openground) %>% 
  mutate(year = as.numeric(gsub("wood_rate_", "", year)),
         ha = ha / (1 - wood_openground)) %>% 
  rename('paws' = wood_paws,
         'prop_bw' = wood_prop_bw) %>% 
  select(year, ha, prop_bw, paws, ambition, wood_openground)

params_wood <- params_wood %>% 
  # Add country props
  crossing(select(country_props$wood_props, country, prop)) %>% 
  mutate(ha = ha * prop) %>% 
  select(-prop)


# Grassland
params_grass <- scenario_params %>%
  filter(par == "grass_rate") %>%
  select(-note, -par) %>% 
  rename('ha' = val) %>% 
  select(ha, ambition) %>% 
  left_join(scenario_params %>%
              filter(par == "grass_stopyear") %>% 
              select(ambition, val) %>% 
              rename('stopyear' = val), 
            by = "ambition") %>% 
  left_join(scenario_params %>%
              filter(par == "grass_prop_arable") %>% 
              select(ambition, val) %>% 
              rename('prop_arable' = val), 
            by = "ambition")

params_grass <- bind_rows(params_grass %>% 
                            # Add country props
                            crossing(select(country_props$grass_grass_props, country, prop)) %>% 
                            mutate(type = "grass_grass") %>% 
                            mutate(ha = ha * prop * (1 - prop_arable)) %>% 
                            select(-prop, -prop_arable),
                          params_grass %>% 
                            # Add country props
                            crossing(select(country_props$grass_arable_props, country, prop)) %>% 
                            mutate(type = "grass_arable") %>% 
                            mutate(ha = ha * prop * prop_arable) %>% 
                            select(-prop, -prop_arable))


# Wood pasture
params_woodpa <- scenario_params %>%
  filter(par == "agroforestry_woodpa") %>% 
  select(-note, -par) %>% 
  rename('ha' = val) %>% 
  left_join(scenario_params %>%
              filter(par == "agroforestry_stopyear") %>% 
              select(ambition, val) %>% 
              rename('stopyear' = val), 
            by = "ambition") %>% 
  left_join(scenario_params %>%
              filter(par == "woodpa_prop_improved") %>% 
              select(ambition, val) %>% 
              rename('prop_improved' = val), 
            by = "ambition")

params_woodpa <- bind_rows(params_woodpa %>% 
                             # Add country props
                             crossing(select(country_props$woodpa_sng_props, country, prop)) %>% 
                             mutate(type = "woodpa_sng") %>% 
                             mutate(ha = ha * prop * (1 - prop_improved)) %>% 
                             select(-prop, -prop_improved),
                           params_woodpa %>% 
                             # Add country props
                             crossing(select(country_props$woodpa_img_props, country, prop)) %>% 
                             mutate(type = "woodpa_img") %>% 
                             mutate(ha = ha * prop * prop_improved) %>% 
                             select(-prop, -prop_improved))


# Agroforestry
params_agfor <- scenario_params %>%
  filter(grepl("agroforestry", par) & par != "agroforestry_stopyear") %>%
  filter(par != "agroforestry_woodpa") %>% 
  select(-note) %>% 
  rename(ha = val,
         type = par) %>% 
  mutate(type = gsub("agroforestry_", "", type)) %>% 
  left_join(scenario_params %>%
              filter(par == "agroforestry_stopyear") %>% 
              select(ambition, val) %>% 
              rename('stopyear' = val), 
            by = "ambition")

params_agfor <- params_agfor %>%
  # Add country props
  crossing(select(country_props$silvoa_props, country, prop)) %>% 
  left_join(select(country_props$silvop_props, country, prop), by = "country") %>% 
  mutate(ha = case_when(type == "silvoa" ~ ha * prop.x, 
                        type == "silvop" ~ ha * prop.y)) %>% 
  select(-prop.x, -prop.y)


# Bioenergy
params_bioenergy <- scenario_params %>%
  filter(grepl("bioenergy_rate", par)) %>%
  select(-note) %>% 
  spread(par, val) %>% 
  gather(year, ha, -ambition) %>% 
  mutate(year = as.numeric(gsub("bioenergy_rate_", "", year))) %>%
  left_join(scenario_params %>%
              filter(grepl("bioenergy_prop", par)) %>%
              select(-note) %>% 
              rename(type = par) %>% 
              mutate(type = gsub("bioenergy_prop_", "", type)), 
            by = "ambition") %>% 
  mutate(ha = ha * val) %>% 
  select(-val) %>% 
  select(type, year, ha, ambition)

params_bioenergy <- params_bioenergy %>%
  # Add country props
  crossing(select(country_props$biocrop_props, country, prop)) %>% 
  left_join(select(country_props$biosrf_props, country, prop), by = "country") %>% 
  mutate(ha = case_when(type %in% c("miscanthus", "src") ~ ha * prop.x, 
                        type == "srf" ~ ha * prop.y)) %>% 
  select(-prop.x, -prop.y)


# Organic
params_organicfarming <- scenario_params %>%
  filter(grepl("organic", par)) %>% 
  select(-note) %>% 
  spread(par, val) %>% 
  rename('organicfarming_prop' = organicfarming)



## LOAD RANK RASTERS --------------------------
# Saltmarsh
r_saltmarshrank_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_saltmarshrank_nir.tif")
r_saltmarshrank_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_saltmarshrank_wal.tif")
r_saltmarshrank_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_saltmarshrank_eng.tif")
r_saltmarshrank_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_saltmarshrank_sct.tif")
load("rdata/intermediate/df_saltmarshrank.RData")

# Peat
r_peatrank_nir <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_peatrank_nir.tif")
r_peatrank_wal <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_peatrank_wal.tif")
r_peatrank_eng <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_peatrank_eng.tif")
r_peatrank_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_peatrank_sct.tif")
load("rdata/intermediate/df_peatrank.RData")

# Woodland, grassland, agfor, bioenergy, PAWS
r_otherrank_nir <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_nir_2022.tif")
r_otherrank_wal <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_wal_2022.tif")
r_otherrank_eng <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_eng_2022.tif")
r_otherrank_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_sct_2022.tif")
load("rdata/intermediate/df_otherrank_nir_2022.RData")
load("rdata/intermediate/df_otherrank_wal_2022.RData")
load("rdata/intermediate/df_otherrank_eng_2022.RData")
load("rdata/intermediate/df_otherrank_sct_2022.RData")





## SEMINATURAL GRASS SOIL --------------------------
# Seminatural grass from soiltype
r_grasssoil_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_grasssoil_nir.tif")
r_grasssoil_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_grasssoil_wal.tif")
r_grasssoil_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_grasssoil_eng.tif")
r_grasssoil_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_grasssoil_sct.tif")

# Native pinewood
r_pinewood_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_pinewood_sct.tif")
# r_lcm_5y_sct[r_pinewood_sct[] == 1] <- 2.1
r_lcm_5y_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_sct.tif")


## SAVE BASELINE RASTERS AS SCENARIO A --------------------------

writeRaster(r_lcm_5y_nir, "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_nir_2022.tif", overwrite = TRUE)
writeRaster(r_lcm_5y_eng, "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_eng_2022.tif", overwrite = TRUE)
writeRaster(r_lcm_5y_wal, "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_wal_2022.tif", overwrite = TRUE)
writeRaster(r_lcm_5y_sct, "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_sct_2022.tif", overwrite = TRUE)


## UPDATE LAND COVER FUNCTION --------------------------
# Function
update_landcover_fun <- function(
  # LCM
  r_lcm_5y, lcm_lookup,
  # Saltmarsh
  params_saltmarsh, r_saltmarshrank, df_saltmarshrank,
  # Peat
  params_peat, r_peatrank, df_peatrank,
  # Woodland, agroforestry, wood paasture, grassland, bioenergy
  params_wood, params_agfor, params_woodpa, params_grass, params_bioenergy, params_organicfarming, 
  r_otherrank, df_otherrank, df_bioenergyrank,
  # Other
  r_grasssoil, r_pinewood
){
  #### Logic: intertidal and peatland first; these are mutually exclusive. 
  ## Then calculate compensatory woodland creation
  ## Then new woodland (mutually exclusive of intertidal and peatland)
  
  # t = number of time periods
  t <- nlayers(r_lcm_5y)
  
  
  #### Saltmarsh
  print("Creating intertidal habitat...")
  if(params_saltmarsh$ha != 0){
    ## Calculate ha to restore per 5-years 
    ha <- params_saltmarsh %>% mutate(ha = ha * 5) 
    
    ## Assign year to each ranked restoration site
    df_saltmarshrank$year <- NA
    for(i in 1:((ha$stopyear - 2015) / 5)){
      df_saltmarshrank$year <- case_when(!is.na(df_saltmarshrank$year) ~ as.integer(df_saltmarshrank$year),
                                         is.na(df_saltmarshrank$year) & df_saltmarshrank$cum_area <= i * ha$ha ~ as.integer(2015 + 5 * i),                                                                                                                      TRUE ~ NA_integer_)
    }
    df_saltmarshrank <- df_saltmarshrank %>% filter(!is.na(year))
    
    ## Sub site ID for restoration year
    isFALSE <- function(x){!x}
    r_saltmarshrank <- subsDT(r_saltmarshrank, select(df_saltmarshrank, rank, year))
    v_saltmarshrank <- getValues(r_saltmarshrank)
    
    ## Update 5y LCM
    for(i in 2:t){
      r_lcm_5y[[i]][v_saltmarshrank <= (2015 + 5 * (i - 1))] <- 19
    }
    rm(v_saltmarshrank); gc()
  }
  
  
  #### Peatland
  print("Restoring peatland...")
  if(sum(params_peat$frac) != 0){
    ## Separate peat rank from fen/bog
    r_fenbog <- r_peatrank[[2]]
    r_peatrank <- r_peatrank[[1]]
    
    ## Calculate area to restore in each 5-y period (deal with sustainable managament of lowland peat separately - non-spatially)
    # Adjust stopyear if 100% restoration before 2050 (needed to leave space for paludiculture)
    params_peat <- params_peat %>%
      group_by(peatclass) %>% 
      mutate(stopyear = ifelse(sum(frac) * (stopyear - 2020) / 5 > 1,
                               as.integer(round(2020 + 5 * 1 / sum(frac), 0)),
                               stopyear))
    
    ha <- df_peatrank %>%
      summarise_at(vars(forest, crop, improved_grass, extensive_grass), list(sum)) %>%
      gather(peatclass, baseline_area) %>% 
      left_join(params_peat, by = "peatclass") %>% 
      mutate(area_restore = baseline_area * frac) %>% 
      select(-frac, -baseline_area, -ambition) 
    
    
    
    ## Calculate cumulative area of each peatclass
    df_peatrank <- df_peatrank %>%
      select(cell, peatrank, forest, crop, improved_grass, extensive_grass) %>%
      mutate_at(vars(forest, crop, improved_grass, extensive_grass), list('cumsum' = cumsum)) %>% 
      mutate(year_forest = NA,
             year_crop = NA,
             year_improved_grass = NA,
             year_extensive_grass = NA)
    
    ## Calculate restoration order of each peatclass 
    for(i in 1:((ha$stopyear[ha$peatclass == "forest"] - 2020) / 5)){
      df_peatrank$year_forest <- case_when(!is.na(df_peatrank$year_forest) ~ as.integer(df_peatrank$year_forest),
                                           is.na(df_peatrank$year_forest) & df_peatrank$forest_cumsum <= i * ha$area_restore[ha$peatclass == "forest"] ~ as.integer(2015 + 5 * i), 
                                           TRUE ~ NA_integer_)
    }
    for(i in 1:((ha$stopyear[ha$peatman == "crop"] - 2020) / 5)){
      df_peatrank$year_crop <- case_when(!is.na(df_peatrank$year_crop) ~ as.integer(df_peatrank$year_crop),
                                         is.na(df_peatrank$year_crop) & df_peatrank$crop_cumsum <= i * ha$area_restore[ha$peatman == "crop"] ~ as.integer(2015 + 5 * i), 
                                         TRUE ~ NA_integer_)
    }
    for(i in 1:((ha$stopyear[ha$peatman == "improved_grass"] - 2020) / 5)){
      df_peatrank$year_improved_grass <- case_when(!is.na(df_peatrank$year_improved_grass) ~ as.integer(df_peatrank$year_improved_grass),
                                                   is.na(df_peatrank$year_improved_grass) & df_peatrank$improved_grass_cumsum <= i * ha$area_restore[ha$peatman == "improved_grass"] ~ as.integer(2015 + 5 * i), 
                                                   TRUE ~ NA_integer_)
    }
    for(i in 1:((ha$stopyear[ha$peatclass == "extensive_grass"] - 2020) / 5)){
      df_peatrank$year_extensive_grass <- case_when(!is.na(df_peatrank$year_extensive_grass) ~ as.integer(df_peatrank$year_extensive_grass),
                                                    is.na(df_peatrank$year_extensive_grass) & df_peatrank$extensive_grass_cumsum <= i * ha$area_restore[ha$peatclass == "extensive_grass"] ~ as.integer(2015 + 5 * i), 
                                                    TRUE ~ NA_integer_)
    }
    
    
    ## Calculate cumulative area for paludiculture
    df_peatrank <- df_peatrank %>%
      mutate(crop_palud_cumsum = cumsum(ifelse(!is.na(year_crop), 0, crop)),
             grass_palud_cumsum = cumsum(ifelse(!is.na(year_improved_grass), 0, improved_grass)),
             year_crop_palud = NA,
             year_grass_palud = NA)
    
    
    for(i in 1:(t - 1)){
      df_peatrank$year_crop_palud <- case_when(!is.na(df_peatrank$year_crop_palud) ~ as.integer(df_peatrank$year_crop_palud),
                                               is.na(df_peatrank$year_crop_palud) & df_peatrank$crop_palud_cumsum <= i * ha$area_restore[ha$peatman == "crop_palud"] ~ as.integer(2015 + 5 * i), 
                                               TRUE ~ NA_integer_)
    }
    for(i in 1:(t - 1)){
      df_peatrank$year_grass_palud <- case_when(!is.na(df_peatrank$year_grass_palud) | !is.na(df_peatrank$year_improved_grass) ~ as.integer(df_peatrank$year_grass_palud),
                                                is.na(df_peatrank$year_grass_palud) & df_peatrank$grass_palud_cumsum <= i * ha$area_restore[ha$peatman == "grass_palud"] ~ as.integer(2015 + 5 * i), 
                                                TRUE ~ NA_integer_)
    }
    
    ## Tidy
    df_peatrank <- df_peatrank %>%
      # Exclude cells containing 0ha of each peatclass 
      mutate(year_forest = ifelse(forest == 0, NA, year_forest),
             year_crop = ifelse(crop == 0, NA, year_crop),
             year_crop_palud = ifelse(crop == 0 | !is.na(year_crop), NA, year_crop_palud),
             year_improved_grass = ifelse(improved_grass == 0, NA, year_improved_grass),
             year_grass_palud = ifelse(improved_grass == 0 | !is.na(year_improved_grass), NA, year_grass_palud),
             year_extensive_grass = ifelse(extensive_grass == 0, NA, year_extensive_grass)) %>% 
      select(peatrank, contains("year")) %>%  
      filter(rowSums(., na.rm = TRUE) != peatrank)

    ## Sub 100-ha cell rank with restoration year
    r_peatrank <- subsDT(stack(replicate(6, r_peatrank)), df_peatrank, by = 1, which = c(2:7))
    names(r_peatrank) <- c("forest", "crop", "improved_grass", "extensive_grass", "crop_palud", "grass_palud")
    
    ## If 0.0625-ha cell is not focal peat class <- NA
    v_lcm <- getValues(r_lcm_5y[[1]])
    r_peatrank$forest[v_lcm != 2] <- NA # Exclude cal pine, which is coded as 2.1
    r_peatrank$crop[v_lcm != 3] <- NA
    r_peatrank$crop_palud[v_lcm != 3] <- NA
    r_peatrank$improved_grass[v_lcm != 4] <- NA
    r_peatrank$grass_palud[v_lcm != 4] <- NA
    r_peatrank$extensive_grass[!v_lcm %in% c(5, 6, 7, 9, 10)] <- NA
    rm(v_lcm)
    
    ## Summarise using mean (only 1 non-NA per cell)
    r_wetland <- mean(r_peatrank[[c(1:4)]], na.rm = TRUE) 
    
    ## Update 5y LCM
    v_wetland <- getValues(r_wetland)
    v_crop_palud <- getValues(r_peatrank[[5]])
    v_grass_palud <- getValues(r_peatrank[[6]])
    v_fenbog <- getValues(r_fenbog)
    
    for(i in 2:t){
      l <- which(v_wetland <= (2015 + 5 * (i - 1)))
      r_lcm_5y[[i]][l] <- v_fenbog[l]
      r_lcm_5y[[i]][which(v_crop_palud <= (2015 + 5 * (i - 1)))] <- 3.4 # Crop-based paludiculture
      r_lcm_5y[[i]][which(v_grass_palud <= (2015 + 5 * (i - 1)))] <- 4.4 # Grass-based paludiculture
    }
    
    rm(v_wetland, v_fenbog, r_peatrank, r_wetland, r_fenbog, v_crop_palud, v_grass_palud); gc()
  }
  
  
  ### Woodland, grassland & agroforestry
  print("Creating new woodland, grassland & agroforestry...")
  if(params_saltmarsh$ha != 0 | sum(params_peat$frac) != 0 | sum(params_wood$ha) != 0 | sum(params_grass$ha) != 0 | sum(params_agfor$ha) != 0 | sum(params_woodpa$ha) != 0){
    ## Calculate woodland area lost to intertidal/peatland
    compensatory_areas <- freqDT(r_lcm_5y) 
    
    for(i in 1:length(compensatory_areas)){
      compensatory_areas[[i]] <- compensatory_areas[[i]] %>% mutate(year = 2015 + 5 * (i - 1))
    }
    
    compensatory_areas <- compensatory_areas %>% 
      bind_rows() %>% 
      as.tbl() %>% 
      filter(ID %in% c(1, 2)) %>% 
      mutate(area = freq * 0.0625) %>% 
      rename('lcm' = ID) %>% 
      arrange(lcm, year) %>% 
      group_by(lcm) %>%
      mutate(area = -(area - lag(area)),
             area = area / (1 - unique(params_wood$wood_openground))) %>%
      ungroup() %>% 
      filter(!is.na(area)) %>% 
      select(lcm, year, area)
    
    ## Calculate ha new woodland per 5-y
    ha <- params_wood %>% 
      select(year, ha, prop_bw) %>% 
      crossing(lcm = c(1, 2)) %>% 
      mutate(ha = case_when(lcm == 1 ~ ha * prop_bw, 
                            lcm == 2 ~ ha * (1 - prop_bw))) %>% 
      full_join(compensatory_areas, by = c("lcm", "year")) %>% 
      mutate(ha = ha + area) %>% 
      select(lcm, year, ha) %>% 
      spread(lcm, ha) %>% 
      set_names(c("year", "bw", "cw"))
    
    ## Calculate ha new grassland, wood pasture and agroforestry
    ha <- ha %>% mutate(grass_grass = ifelse(year <= unique(params_grass$stopyear), params_grass$ha[params_grass$type == "grass_grass"] * 5, 0),
                        grass_arable = ifelse(year <= unique(params_grass$stopyear), params_grass$ha[params_grass$type == "grass_arable"] * 5, 0),
                        silvoa = ifelse(year <= unique(params_agfor$stopyear), params_agfor$ha[params_agfor$type == "silvoa"] * 5, 0),
                        silvop = ifelse(year <= unique(params_agfor$stopyear), params_agfor$ha[params_agfor$type == "silvop"] * 5, 0),
                        woodpa_sng = ifelse(year <= unique(params_woodpa$stopyear), params_woodpa$ha[params_woodpa$type == "woodpa_sng"] * 5, 0),
                        woodpa_img = ifelse(year <= unique(params_woodpa$stopyear), params_woodpa$ha[params_woodpa$type == "woodpa_img"] * 5, 0)) 
    
    
    ## Create empty df for restoration year
    df_otherrank2 <- df_otherrank %>% 
      mutate(bw_year = NA_real_, cw_year = NA_real_, grass_grass_year = NA_real_, grass_arable_year = NA_real_, silvoa_year = NA_real_, silvop_year = NA_real_, woodpa_sng_year = NA_real_, woodpa_img_year = NA_real_,
             bw_add = 0, cw_add = 0, grass_grass_add = 0, grass_arable_add = 0, silvoa_add = 0, silvop_add = 0, woodpa_sng_add = 0, woodpa_img_add = 0,
             bw_area = 0, cw_area = 0, grass_grass_area = 0, grass_arable_area = 0, silvoa_area = 0, silvop_area = 0, woodpa_sng_area = 0, woodpa_img_area = 0) %>% 
      mutate_at(vars(grassopwoodop_arable, grassopwoodop_grass, silvoa, silvop, silvoabmv, silvopbmv, woodpa_sng, woodpa_img), list(~ifelse(is.na(.), 0, .))) %>% 
      select(-contains("_local"), -contains("_dist"))
    
    ## Woodland first
    if(params_saltmarsh$ha != 0 | sum(params_peat$frac) != 0 | sum(params_wood$ha) != 0){
      for(i in 1:(t-1)){
        # Woodland
        df_otherrank2 <- df_otherrank2 %>%
          # Add new broadleaved woodland in rank order
          arrange(bw_rank) %>% 
          mutate(bw_add = case_when(!is.na(bw_year) ~ 0,
                                    is.na(bw_year) & cumsum(woodop) <= ha$bw[i] ~ woodop,
                                    is.na(bw_year) & cumsum(woodop) >  ha$bw[i] ~ 0)) %>%
          mutate(woodop = woodop - bw_add) %>% 
          # Add new coniferous woodland in rank order
          arrange(cw_rank) %>% 
          mutate(cw_add = case_when(!is.na(cw_year) ~ 0,
                                    is.na(cw_year) & cumsum(woodop) <= ha$cw[i] ~ woodop,
                                    is.na(cw_year) & cumsum(woodop) >  ha$cw[i]  ~ 0)) %>%
          # Add creation year
          mutate(bw_year = ifelse(bw_add == 0, bw_year, ha$year[i]),
                 cw_year = ifelse(cw_add == 0, cw_year, ha$year[i])) %>% 
          # Update woodland area
          mutate(woodop = woodop - cw_add,
                 bw_area = bw_area + bw_add,
                 cw_area = cw_area + cw_add,
                 bw_add = 0,
                 cw_add = 0)
      }
    }
    
    
    ## Then wood pasture 
    if(sum(params_woodpa$ha) != 0){
      for(i in 1:(t-1)){
        # Remove reforested area (from prev. step) from woodpa potential
        df_otherrank2 <- df_otherrank2 %>%
          mutate(woodpa_sng = ifelse(woodop == 0, 0, woodpa_sng),
                 woodpa_img = ifelse(woodop == 0, 0, woodpa_img))
        
        # If not enough space for woodpa on SNG, use IMG instead
        if(sum(df_otherrank2$woodpa_sng) - ha$woodpa_sng[i] < 1){
          ha$woodpa_img[i] <- ha$woodpa_img[i] + ha$woodpa_sng[i] - sum(df_otherrank2$woodpa_sng)
        }
        
        # Add new wood pasture in rank order
        df_otherrank2 <- df_otherrank2 %>%
          arrange(agfor_rank) %>%
          mutate(woodpa_sng_add = case_when(!is.na(woodpa_sng_year) ~ 0,
                                            is.na(woodpa_sng_year) & cumsum(woodpa_sng) <= ha$woodpa_sng[i] ~ woodpa_sng,
                                            is.na(woodpa_sng_year) & cumsum(woodpa_sng) >  ha$woodpa_sng[i] ~ 0)) %>%
          mutate(woodpa_img_add = case_when(!is.na(woodpa_img_year) ~ 0,
                                            is.na(woodpa_img_year) & cumsum(woodpa_img) <= ha$woodpa_img[i] ~ woodpa_img,
                                            is.na(woodpa_img_year) & cumsum(woodpa_img) >  ha$woodpa_img[i] ~ 0)) %>%
          mutate(woodpa_sng = woodpa_sng - woodpa_sng_add,
                 woodpa_img = woodpa_img - woodpa_img_add) %>%
          # Add creation year
          mutate(woodpa_sng_year = ifelse(woodpa_sng_add == 0, woodpa_sng_year, ha$year[i]),
                 woodpa_img_year = ifelse(woodpa_img_add == 0, woodpa_img_year, ha$year[i])) %>%
          # Update agroforestry area
          mutate(woodpa_sng_area = woodpa_sng_area + woodpa_sng_add,
                 woodpa_img_area = woodpa_img_area + woodpa_img_add,
                 # Remove woodland opportunity if now agfor
                 woodop = woodop - (woodpa_sng_add + woodpa_img_add),
                 woodpa_sng_add = 0,
                 woodpa_img_add = 0) 
      }
    }
    
    ## Then grassland
    if(sum(params_grass$ha) != 0){
      for(i in 1:(t-1)){
        # Grassland
        df_otherrank2 <- df_otherrank2 %>%
          # Remove reforested area from grassop
          mutate(grassop_arable = ifelse((bw_area != 0 | cw_area != 0) & grassop_arable != 0, grassop_arable - grassopwoodop_arable, grassop_arable),
                 grassop_grass = ifelse((bw_area != 0 | cw_area != 0 | woodpa_img_area != 0) & grassop_grass != 0, grassop_grass - grassopwoodop_grass, grassop_grass),
                 grassopwoodop_arable = ifelse(woodop == 0, 0, grassopwoodop_arable),
                 grassopwoodop_grass = ifelse(woodop == 0, 0, grassopwoodop_grass)) %>%
          # Add new grassland in rank order
          arrange(grass_rank) %>%
          mutate(grass_arable_add = case_when(!is.na(grass_arable_year) ~ 0,
                                              is.na(grass_arable_year) & cumsum(grassop_arable) <= ha$grass_arable[i] ~ grassop_arable,
                                              is.na(grass_arable_year) & cumsum(grassop_arable) >  ha$grass_arable[i] ~ 0),
                 grass_grass_add = case_when(!is.na(grass_grass_year) ~ 0,
                                             is.na(grass_grass_year) & cumsum(grassop_grass) <= ha$grass_grass[i] ~ grassop_grass,
                                             is.na(grass_grass_year) & cumsum(grassop_grass) >  ha$grass_grass[i] ~ 0)) %>%
          # Add creation year
          mutate(grass_arable_year = ifelse(grass_arable_add == 0, grass_arable_year, ha$year[i]),
                 grass_grass_year = ifelse(grass_grass_add == 0, grass_grass_year, ha$year[i])) %>%
          # Update grassland area
          mutate(grassop_arable = grassop_arable - grass_arable_add,
                 grassop_grass = grassop_grass - grass_grass_add,
                 grass_arable_area = grass_arable_area + grass_arable_add,
                 grass_grass_area = grass_grass_area + grass_grass_add,
                 woodop = ifelse(grass_arable_add == 0, woodop, woodop - grassopwoodop_arable),
                 woodop = ifelse(grass_grass_add == 0, woodop, woodop - grassopwoodop_grass),
                 grassopwoodop_arable = ifelse(grassop_arable == 0, grassopwoodop_arable, 0),
                 grassopwoodop_grass = ifelse(grassop_grass == 0, grassopwoodop_grass, 0),
                 grass_arable_add = 0,
                 grass_grass_add = 0) 
        
      }
    }
    
    ## Finally agroforestry 
    if(sum(params_agfor$ha) != 0){
      for(i in 1:(t-1)){
        df_otherrank2 <- df_otherrank2 %>%
          # Remove reforested area and new grassland from agroforestry potential
          mutate(silvop = ifelse(bw_area != 0 | cw_area != 0 | grass_grass_area != 0 | woodpa_img_area != 0, silvopbmv, silvop),
                 silvoa = ifelse(bw_area != 0 | cw_area != 0 | grass_arable_area != 0, silvoabmv, silvoa)) %>%
          # Add new agroforestry in rank order
          arrange(agfor_rank) %>%
          mutate(silvoa_add = case_when(!is.na(silvoa_year) ~ 0,
                                        is.na(silvoa_year) & cumsum(silvoa) <= ha$silvoa[i] ~ silvoa,
                                        is.na(silvoa_year) & cumsum(silvoa) >  ha$silvoa[i] ~ 0)) %>%
          mutate(silvop_add = case_when(!is.na(silvop_year) ~ 0,
                                        is.na(silvop_year) & cumsum(silvop) <= ha$silvop[i] ~ silvop,
                                        is.na(silvop_year) & cumsum(silvop) >  ha$silvop[i] ~ 0)) %>%
          mutate(silvoa = silvoa - silvoa_add,
                 silvop = silvop - silvop_add,
                 silvoabmv = silvoabmv - silvoa_add,
                 silvopbmv = silvopbmv - silvop_add) %>%
          # Add creation year
          mutate(silvoa_year = ifelse(silvoa_add == 0, silvoa_year, ha$year[i]),
                 silvop_year = ifelse(silvop_add == 0, silvop_year, ha$year[i])) %>%
          # Update agroforestry area
          mutate(silvoa_area = silvoa_area + silvoa_add,
                 silvop_area = silvop_area + silvop_add,
                 # Remove woodland opportunity if now agfor
                 woodop = woodop - (silvoa_add + silvop_add),
                 silvoa_add = 0,
                 silvop_add = 0) 
      }
    }
    
    df_otherrank2 <- df_otherrank2 %>% 
      select(bw_rank, cw_rank, agfor_rank, grass_rank, bw_year, cw_year, silvop_year, silvoa_year, woodpa_sng_year, woodpa_img_year, grass_grass_year, grass_arable_year) 
    
    ## Sub wood rank with year
    r_year <- subsDT(r_otherrank[[c(1, 2, 3, 4, 5, 10, 6, 11)]], df_otherrank2, by = c(1, 2, 3, 3, 3, 3, 4, 4), which = c(5, 6, 7, 8, 9, 10, 11, 12))
    
    # Get values
    v_bw <- getValues(r_year[[1]])
    v_cw <- getValues(r_year[[2]])
    v_sp <- getValues(r_year[[3]])
    v_sa <- getValues(r_year[[4]])
    v_wps <- getValues(r_year[[5]])
    v_wpi <- getValues(r_year[[6]])
    v_grg <- getValues(r_year[[7]])
    v_gra <- getValues(r_year[[8]])
    
    # Not sure why, but need to do this  
    v_grg[!is.na(v_wpi)] <- NA
    v_grg[!is.na(v_cw)] <- NA
    v_grg[!is.na(v_bw)] <- NA
    v_gra[!is.na(v_wpi)] <- NA
    v_gra[!is.na(v_cw)] <- NA
    v_gra[!is.na(v_bw)] <- NA
    v_sp[!is.na(v_wpi)] <- NA
    v_sp[!is.na(v_grg)] <- NA
    v_sp[!is.na(v_cw)] <- NA
    v_sp[!is.na(v_bw)] <- NA
    v_sa[!is.na(v_gra)] <- NA
    v_sa[!is.na(v_cw)] <- NA
    v_sa[!is.na(v_bw)] <- NA
    
    v_pinewood <- r_pinewood[]
    
    ## Update 5y LCM
    for(i in 2:t){
      r_lcm_5y[[i]][v_bw <= (2015 + 5 * (i - 1))] <- 1 # Broadleaved
      r_lcm_5y[[i]][v_cw <= (2015 + 5 * (i - 1)) & !v_pinewood %in% 2] <- 2 # Coniferous
      r_lcm_5y[[i]][v_cw <= (2015 + 5 * (i - 1)) &  v_pinewood %in% 2] <- 2.1 # Pinewood
      r_lcm_5y[[i]][v_sp <= (2015 + 5 * (i - 1))] <- 4.1 # Silvopasture
      r_lcm_5y[[i]][v_sa <= (2015 + 5 * (i - 1))] <- 3.1 # Silvoarable
      r_lcm_5y[[i]][v_wps <= (2015 + 5 * (i - 1))] <- r_lcm_5y[[i]][v_wps <= (2015 + 5 * (i - 1))] + 0.1 # Wood pasture
      r_lcm_5y[[i]][v_wpi <= (2015 + 5 * (i - 1))] <- r_grasssoil[v_wpi <= (2015 + 5 * (i - 1))] + 0.1 # New semi-natural grass
      r_lcm_5y[[i]][v_grg <= (2015 + 5 * (i - 1))] <- r_grasssoil[v_grg <= (2015 + 5 * (i - 1))] + 0.2 # New semi-natural grass
      r_lcm_5y[[i]][v_gra <= (2015 + 5 * (i - 1))] <- r_grasssoil[v_gra <= (2015 + 5 * (i - 1))] + 0.2 # New semi-natural grass
    }
    
    rm(v_bw, v_cw, v_sp, v_sa, v_wps, v_wpi, v_grg, v_gra, v_pinewood); gc()
  }
  
  ### PAWS
  print("Restoring PAWS")
  if(unique(params_wood$paws) != 0){
    ## Hectares to convert per 5-year period
    ha <- sum(df_otherrank$paws, na.rm = TRUE) * unique(params_wood$paws)
    
    ## Calculate cumulative area of each peatclass
    df_pawsrank <- df_otherrank %>%
      select(cell, paws, paws_rank) %>%
      filter(paws != 0) %>% 
      arrange(paws_rank) %>% 
      mutate(cumsum = cumsum(paws)) %>% 
      mutate(year_paws = NA)
    
    ## Calculate restoration order of each peatclass 
    for(i in 1:t){
      df_pawsrank$year_paws <- case_when(!is.na(df_pawsrank$year_paws) ~ as.integer(df_pawsrank$year_paws),
                                         is.na(df_pawsrank$year_paws) & df_pawsrank$cumsum <= i * ha ~ as.integer(2015 + 5 * i), 
                                         TRUE ~ NA_integer_)
    }
    
    ## Tidy
    df_pawsrank <- df_pawsrank %>%
      select(paws_rank, year_paws)
    
    ## Sub 100-ha cell rank with restoration year
    r_pawsrank <- subsDT(r_otherrank[[7]], df_pawsrank, by = 1, which = 2)
    
    ## Update 5y LCM
    v_pawsrank <- getValues(r_pawsrank)
    v_pinewood <- r_pinewood[]
    
    for(i in 2:t){
      l <- v_pawsrank <= (2015 + 5 * (i - 1))
      r_lcm_5y[[i]][l & !v_pinewood %in% 2] <- 1
      r_lcm_5y[[i]][l & v_pinewood %in% 2] <- 2.1 # Pinewood
    }
    rm(r_pawsrank, v_pawsrank, v_pinewood); gc()
  }
  
  
  ### Bioenergy crops - Miscanthus, SRC & SRF
  print("Creating new bioenergy crops")
  if(sum(params_bioenergy$ha) != 0){
    ## Hectares to convert per 5-year period
    ha <- params_bioenergy %>% 
      select(type, year, ha) %>% 
      spread(type, ha) %>% 
      mutate(ha_crop = miscanthus + src,
             ha_srf = srf) %>% 
      select(year, ha_crop, ha_srf) %>% 
      mutate(cumsum_crop = cumsum(ha_crop),
             cumsum_srf = cumsum(ha_srf))
    
    ## Restrict to pixels with compatible land cover in 2100 (non-organic, non-agroforestry farmland)
    v_lcm_t <- r_lcm_5y[[t]][]
    
    r_otherrank[[8]][v_lcm_t != 3] <- NA
    r_otherrank[[9]][v_lcm_t != 4] <- NA
    
    ## Recalculate area
    r20_crop <- aggregate(!is.na(r_otherrank[[8]]), fact = 12, fun = sum) * 0.0625
    r20_srf <- aggregate(!is.na(r_otherrank[[9]]), fact = 12, fun = sum) * 0.0625
    
    df_bioenergyrank <- df_bioenergyrank %>%
      select(cell, bioenergy_rank) %>% 
      left_join(as.tbl(as.data.frame(stack(r20_crop, r20_srf))) %>% 
                  set_names(c("crop", "srf")) %>% 
                  mutate(cell = 1:ncell(r20_crop)),
                by = "cell")
    
    
    ## Calculate cumulative area of each peatclass
    df_bioenergyrank <- df_bioenergyrank %>% 
      arrange(bioenergy_rank) %>% 
      mutate(crop = ifelse(is.na(crop), 0, crop),
             srf = ifelse(is.na(srf), 0, srf),
             cumsum_crop = cumsum(crop),
             cumsum_srf = cumsum(srf)) %>% 
      mutate(year_crop = NA,
             year_srf = NA)
    
    
    ## Calculate restoration order of each peatclass 
    for(i in 1:(t-1)){
      df_bioenergyrank$year_crop <- case_when(!is.na(df_bioenergyrank$year_crop) ~ as.integer(df_bioenergyrank$year_crop),
                                              is.na(df_bioenergyrank$year_crop) & df_bioenergyrank$cumsum_crop < ha$cumsum_crop[i] ~ as.integer(2015 + 5 * i), 
                                              TRUE ~ NA_integer_)
      df_bioenergyrank$year_srf <- case_when(!is.na(df_bioenergyrank$year_srf) ~ as.integer(df_bioenergyrank$year_srf),
                                             is.na(df_bioenergyrank$year_srf) & df_bioenergyrank$cumsum_srf < ha$cumsum_srf[i] ~ as.integer(2015 + 5 * i), 
                                             TRUE ~ NA_integer_)
    }
    
    
    ## Tidy
    df_bioenergyrank <- df_bioenergyrank %>%
      select(bioenergy_rank, year_crop, year_srf)
    
    ## Sub 100-ha cell rank with restoration year
    r_croprank <- subsDT(r_otherrank[[8]], df_bioenergyrank, by = 1, which = 2)
    r_srfrank <- subsDT(r_otherrank[[9]], df_bioenergyrank, by = 1, which = 3)
    
    ## Update 5y LCM
    v_croprank <- getValues(r_croprank)
    v_srfrank <- getValues(r_srfrank)
    
    for(i in 2:t){
      l <- which(v_croprank <= (2015 + 5 * (i - 1)))
      r_lcm_5y[[i]][l] <- 3.2 # 3.2 = Bioenergy crop (Miscanthus/SRC)
      l <- which(v_srfrank <= (2015 + 5 * (i - 1)))
      r_lcm_5y[[i]][l] <- 4.2 # 4.2 = SRF
    }
    
    rm(v_lcm_t, r20_crop, r20_srf, r_croprank, r_srfrank, v_croprank, v_srfrank); gc()
  }
  
  
  ### Organic farmland
  print("Converting organic farmland")
  if(params_organicfarming$organicfarming_prop != 0){
    organic_frac <- params_organicfarming$organicfarming_prop / ((params_organicfarming$organicfarming_year - 2020) / 5)
    
    # Farmland in 2035 (1km2) - excluding bioenergy, but including agroforestry
    r_farmed <- aggregate(r_lcm_5y[[(params_organicfarming$organicfarming_year - 2015) / 5]] %in% c(3, 3.1, 4, 4.1), 40)
    
    set.seed(1)
    df_organicyear <- as.data.frame(r_farmed) %>%
      as.tbl() %>% 
      set_names(c("area")) %>% 
      mutate(id = 1:ncell(r_farmed)) %>% 
      select(id, area) %>% 
      filter(area != 0) %>%
      sample_n(nrow(.)) %>% 
      mutate(rank = 1:nrow(.),
             cumulative = cumsum(area)) %>% 
      mutate(year = 2020 + ceiling(cumulative / (max(cumulative) * organic_frac)) * 5) %>% 
      filter(year <= params_organicfarming$organicfarming_year) 
    
    r_organicyear <- r_farmed
    r_organicyear[] <- 1:ncell(r_organicyear)
    r_organicyear <- subsDT(r_organicyear, df_organicyear, 1, 5)
    r_organicyear <- projectRaster(r_organicyear, r_lcm_5y[[1]], method = 'ngb')
    
    # If 25-m pixel unfarmed, <- NA
    r_organicyear[!r_lcm_5y[[(params_organicfarming$organicfarming_year - 2015) / 5]] %in% c(3, 3.1, 4, 4.1)] <- NA
    v_organicyear <- r_organicyear[]
    
    for(i in 2:t){
      l <- v_organicyear <= (2015 + 5 * i)
      v <- round(r_lcm_5y[[i]][], 1)
      
      r_lcm_5y[[i]][l & v == 3] <- 3.3
      r_lcm_5y[[i]][l & v == 3.1] <- 3.31
      r_lcm_5y[[i]][l & v == 4] <- 4.3
      r_lcm_5y[[i]][l & v == 4.1] <- 4.31
      
    }
    rm(r_organicyear, v_organicyear); gc()
    
    
    freq(r_lcm_5y %in% c(4.1, 4.31))
  }
  
  
  ### Return
  print("Done!")
  return(r_lcm_5y)
}

## RUN --------------------------
ambition_combos <- ambition_combos %>% 
  select(-ambition_hedge, -ambition_lowcarbonfarming, -name) %>%
  arrange(scenario) %>% 
  mutate(lab = letters[1:nrow(.)]) %>% 
  select(scenario, lab, everything())

# No PAWS restoration - not a mitigation measure
params_wood$paws <- 0

# Northern Ireland
for(i in 2:nrow(ambition_combos)){
  r <- update_landcover_fun(r_lcm_5y_nir, lcm_lookup,
                            filter(params_saltmarsh, ambition == ambition_combos$ambition_saltmarsh[i]), r_saltmarshrank_nir, df_saltmarshrank,
                            filter(params_peat, ambition == ambition_combos$ambition_peat[i]), r_peatrank_nir, df_peatrank_nir,
                            filter(params_wood, ambition == ambition_combos$ambition_wood[i] & country == "Northern Ireland"), 
                            filter(params_agfor, ambition == ambition_combos$ambition_agroforestry[i] & country == "Northern Ireland"), 
                            filter(params_woodpa, ambition == ambition_combos$ambition_woodpasture[i] & country == "Northern Ireland"), 
                            filter(params_grass, ambition == ambition_combos$ambition_grass[i] & country == "Northern Ireland"), 
                            filter(params_bioenergy, ambition == ambition_combos$ambition_bioenergy[i] & country == "Northern Ireland"), 
                            filter(params_organicfarming, ambition == ambition_combos$ambition_organic[i]), 
                            r_otherrank_nir, df_otherrank_nir, df_bioenergyrank_nir,
                            r_grasssoil_nir, NA) 
  writeRaster(r, paste0("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_", ambition_combos$lab[i], "_nir_2022.tif"), overwrite = TRUE)
  rm(r); gc()
  removeTmpFiles(h=0)
}

# Wales
for(i in 2:nrow(ambition_combos)){
  r <- update_landcover_fun(r_lcm_5y_wal, lcm_lookup,
                            filter(params_saltmarsh, ambition == ambition_combos$ambition_saltmarsh[i]), r_saltmarshrank_wal, df_saltmarshrank,
                            filter(params_peat, ambition == ambition_combos$ambition_peat[i]), r_peatrank_wal, df_peatrank_wal,
                            filter(params_wood, ambition == ambition_combos$ambition_wood[i] & country == "Wales"), 
                            filter(params_agfor, ambition == ambition_combos$ambition_agroforestry[i] & country == "Wales"), 
                            filter(params_woodpa, ambition == ambition_combos$ambition_woodpasture[i] & country == "Wales"), 
                            filter(params_grass, ambition == ambition_combos$ambition_grass[i] & country == "Wales"), 
                            filter(params_bioenergy, ambition == ambition_combos$ambition_bioenergy[i] & country == "Wales"), 
                            filter(params_organicfarming, ambition == ambition_combos$ambition_organic[i]), 
                            r_otherrank_wal, df_otherrank_wal, df_bioenergyrank_wal,
                            r_grasssoil_wal, NA) 
  writeRaster(r, paste0("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_", ambition_combos$lab[i], "_wal_2022.tif"), overwrite = TRUE)
  rm(r); gc()
  removeTmpFiles(h=0)
}

# Scotland
for(i in 2:nrow(ambition_combos)){
  r <- update_landcover_fun(r_lcm_5y_sct, lcm_lookup,
                            filter(params_saltmarsh, ambition == ambition_combos$ambition_saltmarsh[i]), r_saltmarshrank_sct, df_saltmarshrank,
                            filter(params_peat, ambition == ambition_combos$ambition_peat[i]), r_peatrank_sct, df_peatrank_sct,
                            filter(params_wood, ambition == ambition_combos$ambition_wood[i] & country == "Scotland"), 
                            filter(params_agfor, ambition == ambition_combos$ambition_agroforestry[i] & country == "Scotland"), 
                            filter(params_woodpa, ambition == ambition_combos$ambition_woodpasture[i] & country == "Scotland"), 
                            filter(params_grass, ambition == ambition_combos$ambition_grass[i] & country == "Scotland"), 
                            filter(params_bioenergy, ambition == ambition_combos$ambition_bioenergy[i] & country == "Scotland"), 
                            filter(params_organicfarming, ambition == ambition_combos$ambition_organic[i]), 
                            r_otherrank_sct, df_otherrank_sct, df_bioenergyrank_sct,
                            r_grasssoil_sct, r_pinewood_sct) 
  writeRaster(r, paste0("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_", ambition_combos$lab[i], "_sct_2022.tif"), overwrite = TRUE)
  rm(r); gc()
  removeTmpFiles(h=0)
}

# England
for(i in 2:nrow(ambition_combos)){
  r <- update_landcover_fun(r_lcm_5y_eng, lcm_lookup,
                            filter(params_saltmarsh, ambition == ambition_combos$ambition_saltmarsh[i]), r_saltmarshrank_eng, df_saltmarshrank,
                            filter(params_peat, ambition == ambition_combos$ambition_peat[i]), r_peatrank_eng, df_peatrank_eng,
                            filter(params_wood, ambition == ambition_combos$ambition_wood[i] & country == "England"), 
                            filter(params_agfor, ambition == ambition_combos$ambition_agroforestry[i] & country == "England"), 
                            filter(params_woodpa, ambition == ambition_combos$ambition_woodpasture[i] & country == "England"), 
                            filter(params_grass, ambition == ambition_combos$ambition_grass[i] & country == "England"), 
                            filter(params_bioenergy, ambition == ambition_combos$ambition_bioenergy[i] & country == "England"), 
                            filter(params_organicfarming, ambition == ambition_combos$ambition_organic[i]), 
                            r_otherrank_eng, df_otherrank_eng, df_bioenergyrank_eng,
                            r_grasssoil_eng, NA) 
  writeRaster(r, paste0("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_", ambition_combos$lab[i], "_eng_2022.tif"), overwrite = TRUE)
  rm(r); gc()
  removeTmpFiles(h=0)
}