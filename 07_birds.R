## INIT --------------------
source("setup.R")


# Cross-join function for data.table
CJ.dt <- function(X, Y) {
  stopifnot(is.data.table(X), is.data.table(Y))
  
  if(nrow(X) > 0 & nrow(Y) > 0){
    k = NULL
    X = X[, c(k = 1, .SD)]
    setkey(X, k)
    Y = Y[, c(k = 1, .SD)]
    setkey(Y, NULL)
    return(X[Y, allow.cartesian = T][, k := NULL][])
    
  } else {
    duplicatedNames <- names(Y)[names(Y) %in% names(X)]
    if(length(duplicatedNames) > 0) {
      setnames(Y, duplicatedNames, paste0("i.", duplicatedNames))
    }
    setkey(Y)
    setkey(X)
    return(cbind(X[!X], Y[!Y]))
  }
  
}


## PROCESS BBS COUNT DATA -----------------------------
## Read focal species list
bird_spp <- fread("data/bbs/bird_spp.csv", skip = 1)[keep == "yes"]

## Load BBS data (to get square IDs)
df_bbs <- read_rds(file = "data/bbs/from_bto/BBS20132017.rds") %>% 
  as.data.table() 

# Reshape & tidy
df_bbs <- df_bbs[, `:=`(gridref = square, square = NULL,
                        spp = sp2, sp2 = NULL, 
                        date = NULL, type = NULL)
][distband %in% c("1", "2")
][, melt(.SD, id.vars = c("year", "gridref", "visit", "spp", "distband"), variable.name = "section", value.name = "count")
][, .(count = sum(count)), by = .(year, gridref, visit, section, spp)][
][, section := as.integer(substr(section, 2, 3))]




## Get all visits per section per year 
# (assume at least one record per square-visit, and that all sections are walked each visit)
visits <- CJ.dt(merge(unique(df_bbs[, .(year, gridref, visit)]),
                      unique(df_bbs[, .(gridref, section)]),
                      by = "gridref", all = TRUE, allow.cartesian = TRUE),
                data.table(spp = unique(df_bbs$spp)))

# Join counts with visits
df_bbs <- merge(df_bbs,
                visits, 
                by = c("year", "gridref", "visit", "section", "spp"), all = TRUE)[, count := ifelse(is.na(count), 0, count) ]
rm(visits)


## Join counts with habitat
# Load habitat data
load("data/bbs/from_bto/habitat_primary_levels1&2.rdata")

# Get missing habitat data
habitat <- as.data.table(habitat)
habitat <- habitat[h1l1 %in% LETTERS
][, ':='(section = tsection, habitat = paste0(h1l1, h1l2), tsection = NULL, h1l1 = NULL, h1l2 = NULL)]
habitat <- unique(habitat)
set.seed(29071989) # For modal fun, which uses random to deal with ties
habitat_modal <- habitat[, .(habitat_modal = raster::modal(habitat)), by = .(gridref, section)]

# Join with counts
df_bbs <- merge(merge(df_bbs,
                      habitat,
                      by = c("gridref", "year", "visit", "section"), 
                      all.x = TRUE),
                habitat_modal,
                by = c("gridref", "section"))[, ':='(habitat = ifelse(is.na(habitat), habitat_modal, habitat),
                                                     habitat_modal = NULL)]





## Join counts with detprob (inner join - discard if no detprob)
# Load detection prob
detprob <- fread("data/bbs/from_bto/allspecies_9420_det.csv")

# Expand grouped habitat classes
detprob <- merge(detprob,
                 data.table(habitat = c("E", "F", "A", "B", "C", "D", "G", "H", "I", "AB", "AB", "CD", "CD", "GHI", "GHI", "GHI"),
                            habitat_ = c("E", "F", "A", "B", "C", "D", "G", "H", "I", "A", "B", "C", "D", "G", "H", "I")),
                 by = "habitat", all.x = TRUE, allow.cartesian = TRUE)[, ':='(habitat = habitat_, spp = species, habitat_ = NULL, se.detprob = NULL, species = NULL)]

# Join with counts
df_bbs <- merge(df_bbs[, ':='(habitat_det = substr(habitat, 1, 1))],
                detprob, 
                by = c("spp", "visit", "habitat_det" = "habitat"), all.x = TRUE)


# Fix NA detprob (remove if 100% NA - otherwise average of remaining habitats)
df_bbs <- df_bbs[, nna := mean(is.na(detprob)), by = .(spp)][
  nna < 1
][, nna := NULL
][, detprob := ifelse(is.na(detprob), mean(detprob, na.rm = TRUE), detprob), by = .(spp, visit)
][, habitat_det := NULL]

# Merge with spp list
df_bbs <- merge(df_bbs, bird_spp[, .(spp, species, gb_only)], by = "spp", all = FALSE)


# # Discard if fewer than 25 non-0 records per species
# df_bbs <- df_bbs[, include := sum(count > 0) > 25, by = spp][include == TRUE]

## Summarise across year-squares  
df_bbs_counts_sq <- df_bbs[, .(count = sum(count),
                               detprob = mean(detprob),
                               n_visits = n_distinct(paste0(visit, year)),
                               n_sections = n_distinct(section)), 
                           by = .(gridref, spp, species, gb_only)
][, effective_area := n_visits * n_sections * 0.04 * detprob] 

## Summarise across sections  
set.seed(29071989)
df_bbs_counts_sx <- df_bbs[, .(count = sum(count),
                               detprob = mean(detprob),
                               n_visits = n_distinct(paste0(visit, year)),
                               n_sections = n_distinct(section),
                               habitat = modal(habitat)), 
                           by = .(gridref, section, spp, species, gb_only)
][, ':='(effective_area = n_visits * n_sections * 0.04 * detprob)] 

## Save RData
save(df_bbs_counts_sq, df_bbs_counts_sx, file = "rdata/bbs_data_2023.RData")



## GET BBS GEOMETRY --------------------
## Get 1-km square geometry
# GB
sf_1km_gbr <- st_read("data/bbs/grid/osgrid001.shp") %>%
  rename(gridref = PLAN_NO) %>%
  mutate(gridref = toupper(gridref)) %>% 
  filter(gridref %in% df_bbs_counts_sq$gridref)
# NIR
sf_1km_nir <- st_read("data/bbs/grid/nirgrid001.shp") %>%
  select(-PERSONS, -HOUSEHOLDS, -X, -Y) %>%
  rename(gridref = GRIDSQ) %>%
  mutate(gridref = gsub(" ", "", gridref)) %>% 
  mutate(gridref = paste0("I", gridref)) %>% 
  filter(gridref %in% df_bbs_counts_sq$gridref)


## Create 1km buffer
# GB
sf_r1000_gbr <- sf_1km_gbr %>%
  st_centroid() %>%
  st_buffer(dist = 1000)
# NIR
sf_r1000_nir <- sf_1km_nir %>%
  st_centroid() %>%
  st_buffer(dist = 1000)


## Create 2km buffer
# GB
sf_r2000_gbr <- sf_1km_gbr %>%
  st_centroid() %>%
  st_buffer(dist = 2000)
# NIR
sf_r2000_nir <- sf_1km_nir %>%
  st_centroid() %>%
  st_buffer(dist = 2000)



## Get transect lines
set.seed(29071989) # Set seed from random sample
sf_sections <- st_read("data/bbs/from_bto/BBS_transects.shp") %>% 
  rename(gridref = GRIDREF, 
         section = TRANSECT) %>% 
  mutate(gridref = toupper(gridref)) %>% 
  select(gridref, section) %>% 
  filter(as.numeric(st_length(.)) > 0) %>% 
  group_by(gridref, section) %>% 
  sample_n(1) %>% 
  ungroup() 

# Separate GBR and NIR - then 100m FLAT buffer
sf_sections_gbr <- sf_sections %>%
  filter(gridref %in% sf_1km_gbr$gridref) %>% 
  st_transform(crs(sf_1km_gbr)) %>% 
  st_simplify(dTolerance = 20) %>% 
  st_buffer(100, endCapStyle = 'FLAT')

sf_sections_nir <- sf_sections %>% 
  filter(gridref %in% sf_1km_nir$gridref) %>% 
  st_transform(crs(sf_1km_nir)) %>% 
  st_simplify(dTolerance = 20) %>% 
  st_buffer(100, endCapStyle = 'FLAT') 



## LOAD COVARIATE DATA -------------------------
load("rdata/raster_lookup_nuts1.RData")
load("rdata/raster_lookup_lcm.RData")

# Read updated (2015) LCM rasters
r_lcm_nir <- raster("D:/bigdata/rasters/r_lcm_new_nir.tif")
r_lcm_eng <- raster("D:/bigdata/rasters/r_lcm_new_eng.tif")
r_lcm_sct <- raster("D:/bigdata/rasters/r_lcm_new_sct.tif")
r_lcm_wal <- raster("D:/bigdata/rasters/r_lcm_new_wal.tif")

# Read NUTS1 rasters
r_nuts1_nir <- raster("D:/bigdata/rasters/r_nuts1_nir.tif")
r_nuts1_eng <- raster("D:/bigdata/rasters/r_nuts1_eng.tif")
r_nuts1_sct <- raster("D:/bigdata/rasters/r_nuts1_sct.tif")
r_nuts1_wal <- raster("D:/bigdata/rasters/r_nuts1_wal.tif")

# Read hedge length raster
r_hedges_eng <- stack("D:/bigdata/rasters/r_hedges_eng.tif")
r_hedges_sct <- stack("D:/bigdata/rasters/r_hedges_sct.tif")
r_hedges_wal <- stack("D:/bigdata/rasters/r_hedges_wal.tif")
r_hedges_nir <- stack("D:/bigdata/rasters/r_hedges_nir.tif")

# Read island raster
r_island_eng <- stack("D:/bigdata/rasters/r_island_eng.tif")
r_island_sct <- stack("D:/bigdata/rasters/r_island_sct.tif")
r_island_wal <- stack("D:/bigdata/rasters/r_island_wal.tif")
r_island_nir <- stack("D:/bigdata/rasters/r_island_nir.tif")

# Read elevation raster
r_elev_eng <- stack("D:/bigdata/rasters/r_elev_eng.tif")
r_elev_sct <- stack("D:/bigdata/rasters/r_elev_sct.tif")
r_elev_wal <- stack("D:/bigdata/rasters/r_elev_wal.tif")
r_elev_nir <- stack("D:/bigdata/rasters/r_elev_nir.tif")

# Merge GB countries
r_lcm_gbr <- merge(merge(r_lcm_eng, r_lcm_wal), r_lcm_sct)
r_nuts1_gbr <- merge(merge(r_nuts1_eng, r_nuts1_wal), r_nuts1_sct)
r_hedges_gbr <- merge(merge(r_hedges_eng, r_hedges_wal), r_hedges_sct)
r_island_gbr <- merge(merge(r_island_eng, r_island_wal), r_island_sct)
r_elev_gbr <- merge(merge(r_elev_eng, r_elev_wal), r_elev_sct)


### Rescale hedge data
## Fix NIR
# Isolate missing areas
r_hedgesna_nir <- r_hedges_nir == -1
r_hedgesna_gbr <- r_hedges_gbr == -1 # should be none

# Fraction with no data (-1)
hedge_freq_nir <- freq(r_hedges_nir)$layer
hedge_missing_nir <- hedge_freq_nir %>% 
  as.data.frame() %>%
  as_tibble() %>% 
  filter(!is.na(value)) %>% 
  summarise(prop_missing = count[value == -1] / sum(count))

# Replace no data (-1) with NA
r_hedges_nir[r_hedges_nir == -1] <- NA


## Get estimated baseline hedge length
hedge_length <- read_csv("data/hedges/cs_hedge_length.csv", skip = 1) %>% select(-notes) %>%
  mutate(country = ifelse(country == "Northern Ireland", country, "GB")) %>% 
  group_by(country) %>% 
  summarise(length = sum(length), .groups = "drop") %>% 
  mutate(pixels = c(cellStats(r_hedges_gbr[[1]], sum), # 13265839
                    (1 - hedge_missing_nir$prop_missing) * cellStats(r_hedges_nir[[1]], sum))) %>% # 886347.6
  mutate(km_per_pix = length/pixels)


## Rescale hedge length
r_hedges_gbr <- r_hedges_gbr * hedge_length$km_per_pix[hedge_length$country == "GB"] # 0.0531 / 0.0625 ha = 0.85 km / ha
r_hedges_nir <- r_hedges_nir * hedge_length$km_per_pix[hedge_length$country == "Northern Ireland"] # 0.13 / 0.0625 ha = 2.1 km / ha




## EXTRACT COVARIATES ------------------------
# Function for xtracting LCM coverage in polygon 
mode_fun <- function(x, y) {
  x = x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

## Fun
# Function for squares
get_covars_fun_sq <- function(squares, buffers, lcm, nuts1, elev, hedge, hedgena, island, lookup_lcm, lookup_nuts1){
  # XY
  df_xy <- st_coordinates(st_centroid(st_transform(squares, bng))) %>%
    as.data.frame() %>% 
    as_tibble() %>% 
    set_names(c("x", "y"))
  
  # Mean altitude
  df_elev <- exactextractr::exact_extract(elev, st_transform(squares, crs(lcm)), 'mean')
  
  # Modal island/mainland
  df_island <- exactextractr::exact_extract(island, st_transform(squares, crs(lcm)), mode_fun)
  
  # Modal NUTS1
  df_nuts1 <- squares %>% 
    st_set_geometry(NULL) %>% 
    mutate(nuts1 = exact_extract(nuts1, st_transform(squares, crs(lcm)), fun = mode_fun)) %>% 
    rename(nuts1_layer = nuts1) %>% 
    left_join(lookup_nuts1, by = "nuts1_layer") %>% 
    select(-nuts1_layer)
  
  # LCM 100m
  df_lcm_sq <- exactextractr::exact_extract(lcm, st_transform(squares, crs(lcm)))
  df_lcm_sq <- df_lcm_sq %>% 
    enframe("id") %>% 
    mutate(value = purrr::map(value, ~{.x %>% 
        group_by(value) %>% 
        summarise(prop = sum(coverage_fraction), .groups = "drop")})) %>% 
    unnest(cols = c('value')) %>%
    rename(lcm_layer = value) %>% 
    left_join(lookup_lcm, by = "lcm_layer") %>% 
    group_by(id, lcm) %>% 
    summarise(prop = sum(prop), .groups = "drop") %>% 
    group_by(id) %>% 
    mutate(prop = prop / sum(prop)) %>% 
    ungroup() %>% 
    spread(lcm, prop, fill = 0) %>% 
    set_names(paste0(names(.), "_sq")) %>% 
    mutate_all(list(~ifelse(is.na(.), 0, .))) %>%
    rename(na_sq = `<NA>_sq`, id = id_sq)        
  
  # LCM 1km
  df_lcm_buf <- exactextractr::exact_extract(lcm, st_transform(buffers, crs(lcm)))  
  df_lcm_buf <- df_lcm_buf %>% 
    enframe("id") %>% 
    mutate(value = purrr::map(value, ~{.x %>% 
        group_by(value) %>% 
        summarise(prop = sum(coverage_fraction)) %>% 
        ungroup()})) %>% 
    unnest(cols = c('value')) %>%
    rename(lcm_layer = value) %>% 
    left_join(lookup_lcm, by = "lcm_layer") %>% 
    group_by(id, lcm) %>% 
    summarise(prop = sum(prop), .groups = "drop") %>% 
    group_by(id) %>% 
    mutate(prop = prop / sum(prop)) %>% 
    ungroup() %>% 
    spread(lcm, prop, fill = 0) %>% 
    set_names(paste0(names(.), "_buf")) %>% 
    mutate_all(list(~ifelse(is.na(.), 0, .))) %>%
    rename(na_buf = `<NA>_buf`, id = id_buf)
  
  # Hedge (sq)
  df_hedge <- exactextractr::exact_extract(hedge, st_transform(squares, crs(lcm)), 'sum')
  df_hedge <- tibble(hedge_total = df_hedge)
  
  df_hedgena <- exactextractr::exact_extract(hedgena, st_transform(squares, crs(lcm)), 'sum')
  df_hedgena <- tibble(hedgena_total = df_hedgena)
  
  
  # Combine & return
  return(bind_cols(as_tibble(df_nuts1),
                   df_xy,
                   elev = df_elev,
                   island = df_island,
                   df_hedge,
                   df_hedgena,
                   df_lcm_sq %>% select(-id),
                   df_lcm_buf %>% select(-id)))
}

# Function for sections
get_covars_fun_sx <- function(sections, lcm, lookup_lcm){
  # LCM 
  df_lcm_sx <- exactextractr::exact_extract(lcm, st_transform(sections, crs(lcm)))
  df_lcm_sx <- df_lcm_sx %>%
    enframe("id") %>% 
    mutate(value = purrr::map(value, ~{.x %>% 
        group_by(value) %>% 
        summarise(prop = sum(coverage_fraction), 
                  .groups = "drop")})) %>% 
    unnest(cols = c('value')) %>%
    rename(lcm_layer = value) %>% 
    left_join(lookup_lcm, by = "lcm_layer") %>% 
    group_by(id, lcm) %>% 
    summarise(prop = sum(prop), .groups = "drop") %>% 
    group_by(id) %>% 
    mutate(prop = prop / sum(prop)) %>% 
    ungroup() %>% 
    spread(lcm, prop, fill = 0) %>% 
    set_names(paste0(names(.), "_sx")) %>% 
    mutate_all(list(~ifelse(is.na(.), 0, .))) %>%
    rename(na_sx = `<NA>_sx`, id = id_sx)        
  
  
  # Combine & return
  return(bind_cols(as_tibble(st_set_geometry(sections, NULL)),
                   df_lcm_sx %>% select(-id)))
}

## Run
# GB
covars_gbr_sq <- get_covars_fun_sq(sf_1km_gbr, sf_r2000_gbr, 
                                   r_lcm_gbr, r_nuts1_gbr, r_elev_gbr, r_hedges_gbr, r_hedgesna_gbr, r_island_gbr,
                                   lookup_lcm, lookup_nuts1)
covars_gbr_sx <- get_covars_fun_sx(sf_sections_gbr, 
                                   r_lcm_gbr, 
                                   lookup_lcm)
# NI
covars_nir_sq <- get_covars_fun_sq(sf_1km_nir, sf_r2000_nir,
                                   r_lcm_nir, r_nuts1_nir, r_elev_nir, r_hedges_nir, r_hedgesna_nir, r_island_nir,
                                   lookup_lcm, lookup_nuts1)
covars_nir_sx <- get_covars_fun_sx(sf_sections_nir,
                                   r_lcm_nir,
                                   lookup_lcm)


## Bind
covars_sq <- bind_rows(covars_gbr_sq, covars_nir_sq) %>% 
  mutate_all(~ifelse(is.na(.), 0, .)) %>% 
  # Drops 5 Northern Irish sites with no hedge data
  filter(hedgena_total == 0) %>% 
  rename(hedge = hedge_total) %>% 
  select(-hedgena_total) %>% 
  # Tidy country/island attributes
  mutate(country = ifelse(nuts1 %in% c("Scotland", "Wales", "Northern Ireland"), nuts1, "England"),
         island = as.factor(ifelse(country == "Northern Ireland", 2, island)),
         nuts1 = ifelse(nuts1 %chin% c("South East (England)", "London"), "South East & London (England)", nuts1)) %>% 
  # Tidy buffer attributes
  mutate(w.d_buf = b.w_buf + c.w_buf) %>% 
  select(-contains("buf"), w.d_buf, b.t_buf, a.h_buf) %>% 
  # Combine categories
  mutate(s.g_sq = c.g_sq + n.g_sq + a.g_sq,
         c.l_sq = c.l_sq + s.m_sq) %>% 
  select(-c.g_sq, -n.g_sq, -a.g_sq, -s.m_sq, -na_sq) 

covars_sx <- bind_rows(covars_gbr_sx, covars_nir_sx) %>% 
  mutate_all(~ifelse(is.na(.), 0, .)) %>% 
  # Combine categories
  mutate(s.g_sx = c.g_sx + n.g_sx + a.g_sx,
         c.l_sx = c.l_sx + s.m_sx) %>% 
  select(-c.g_sx, -n.g_sx, -a.g_sx, -s.m_sx, -na_sx) 

save(covars_sq, covars_sx, file = "rdata/bbs_covars_2023.RData")


## FIT MODELS ------------------------
# Load progress from above
load("rdata/bbs_data_2023.RData")
load("rdata/bbs_covars_2023.RData")


# Identify rarest species
spp_exl <- df_bbs_counts_sq %>%
  group_by(spp, species) %>% 
  summarise(nn0 = sum(count != 0), .groups = "drop") %>% 
  filter(nn0 < 25) %$%
  spp

# Drop rarest species
df_bbs_counts_sq <- df_bbs_counts_sq %>% filter(!spp %in% spp_exl)
df_bbs_counts_sx <- df_bbs_counts_sx %>% filter(!spp %in% spp_exl)



### Model A - 1-km land cover composition 
# Data
mod_a_data <- df_bbs_counts_sq %>% 
  as_tibble()  %>% 
  # Join with square-level covariates
  inner_join(covars_sq %>% 
               select(gridref, nuts1, x, y, elev, island, contains("_sq"), contains("_buf"), hedge),
             by = "gridref") %>% 
  select(gridref, spp, species, gb_only, count, effective_area, nuts1, x, y, elev, island, contains("_sq"), contains("_buf"), hedge) %>%
  # Convert _sq to proportional areas (summing to 1, ignoring sea)
  mutate(sum = rowSums(across(contains("_sq")))) %>% 
  mutate_at(vars(contains("_sq")), list(~./sum)) %>% 
  # Log-transform continuous variables
  mutate_at(vars(-gridref, -spp, -species, -gb_only, -count, -effective_area, -nuts1, -x, -y, -elev, -island), list(~log(.+0.01))) %>% 
  # Join with atlas data
  mutate(grid = paste0(substr(gridref, 1, 3), substr(gridref, 5, 5))) %>%
  left_join(atlas_distributions, by = c("species" = "english_name", "grid")) %>% 
  select(-grid) 


# Fun
mod_a_fun <- function(data){
  # Drop NI rows for GB-only species
  if(unique(data$gb_only) == 1){
    data <- filter(data, nuts1 != "Northern Ireland")
  }
  
  # Basic model
  mod_a1 <- tryCatch(gam(count ~ a.h_sq + b.w_sq + c.w_sq + i.g_sq + s.g_sq + b.g_sq + h.r_sq +
                           b.t_sq + f.s_sq + f.w_sq + c.l_sq +
                           island + s(y, x, elev, bs = "tp", k = 20) +
                           offset(log(effective_area)), 
                         family = "nb", 
                         data = data,
                         method = "REML",
                         na.action = "na.fail"),
                     error = function(e) NA,
                     warning = function(e) NA)
  
  # Stop if error/warning
  if(unique(class(mod_a1) == "logical")){
    return(NA)
  }
  
  # Add extra vars
  mod_a2 <- update(mod_a1, . ~ . + hedge)
  mod_a3 <- update(mod_a1, . ~ . + a.h_buf + w.d_buf + b.t_buf)
  mod_a4 <- update(mod_a1, . ~ . + hedge + a.h_buf + w.d_buf + b.t_buf)

  # Return
  return(list(mod_a1,
              mod_a2,
              mod_a3,
              mod_a4))
}



# Run
mod_a_out <- mod_a_data %>% 
  group_by(spp) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(mod = purrr::map(data, ~mod_a_fun(.))) %>% 
  select(-data) %>% 
  mutate(success = ifelse(purrr::map_chr(mod, ~class(.)) == "logical", 0, 1))




### Model B - 1-km mixture model
# Data
mod_b_data <- df_bbs_counts_sq %>% 
  as_tibble() %>%
  # Join with square-level covariates
  inner_join(covars_sq %>% 
               select(gridref, nuts1, contains("_sq"), contains("buf"), hedge),
             by = "gridref") %>% 
  select(spp, gb_only, count, effective_area, nuts1, contains("_sq"), contains("buf"), hedge) %>%
  # Add dummy variables for NUTS regions
  mutate(EM = as.numeric(nuts1 %in% "East Midlands (England)"),
         EE = as.numeric(nuts1 %in% "East of England"),
         NE = as.numeric(nuts1 %in% "North East (England)"),
         NW = as.numeric(nuts1 %in% "North West (England)"),
         NI = as.numeric(nuts1 %in% "Northern Ireland"),
         SC = as.numeric(nuts1 %in% "Scotland"),
         SE = as.numeric(nuts1 %in% "South East & London (England)"),
         SW = as.numeric(nuts1 %in% "South West (England)"),
         WA = as.numeric(nuts1 %in% "Wales"),
         WM = as.numeric(nuts1 %in% "West Midlands (England)"),
         YH = as.numeric(nuts1 %in% "Yorkshire and The Humber")) %>% 
  select(-nuts1) %>% 
  set_names(gsub("_sq", "", names(.))) %>% 
  # Log-transform continuous variables
  mutate_at(vars(w.d_buf, b.t_buf, a.h_buf, hedge), list(~log(.+0.01)))


# Fun 
mod_b_fun <- function(data){
  # Remove NI rows if GB-only 
  if(unique(data$gb_only) == 1){
    data <- filter(data, NI == 0)
  }
  
  # Define params
  params_list_b1 <- tibble(param_name = c(paste0("density_", c("a.h", "b.g", "b.t", "b.w", "c.l", "c.w", "f.s", "f.w", "h.r", "i.g", "r.k", "s.g")),
                                          paste0("beta_", c("EM", "EE", "NE", "NW", "NI", "SC", "SE", "SW", "WA", "YH")))) # WM = reference    params_list_b2 <- params_list_b2 %>% 
  params_list_b2 <- params_list_b1 %>% 
    add_row(param_name = "beta_hedge")
  params_list_b3 <- params_list_b1 %>% 
    add_row(param_name = c("beta_a.h_buf", "beta_b.t_buf", "beta_w.d_buf"))
  params_list_b4 <- params_list_b1 %>% 
    add_row(param_name = c("beta_hedge", "beta_a.h_buf", "beta_b.t_buf", "beta_w.d_buf"))
  
  k_b1 <- nrow(params_list_b1)
  k_b2 <- nrow(params_list_b2)
  k_b3 <- nrow(params_list_b3)
  k_b4 <- nrow(params_list_b4)
  
  fn_b1 <- function(params, data){
    fixed <- 
      exp(params[13] * data$EM) * 
      exp(params[14] * data$EE) * 
      exp(params[15] * data$NE) * 
      exp(params[16] * data$NW) * 
      exp(params[17] * data$NI) * 
      exp(params[18] * data$SC) * 
      exp(params[19] * data$SE) * 
      exp(params[20] * data$SW) * 
      exp(params[21] * data$WA) * 
      exp(params[22] * data$YH)
    
    y <- 
      (exp(params[ 1]) * fixed) * data$a.h +
      (exp(params[ 2]) * fixed) * data$b.g +
      (exp(params[ 3]) * fixed) * data$b.t +
      (exp(params[ 4]) * fixed) * data$b.w +
      (exp(params[ 5]) * fixed) * data$c.l +
      (exp(params[ 6]) * fixed) * data$c.w +
      (exp(params[ 7]) * fixed) * data$f.s +
      (exp(params[ 8]) * fixed) * data$f.w +
      (exp(params[ 9]) * fixed) * data$h.r +
      (exp(params[10]) * fixed) * data$i.g +
      (exp(params[11]) * fixed) * data$r.k +
      (exp(params[12]) * fixed) * data$s.g
    
    # Return ?quasi-likelihood? (from Rhys)
    -sum(data$count / data$effective_area * log(y) - y)
  }
  
  fn_b2 <- function(params, data){
    fixed <- 
      exp(params[13] * data$EM) * 
      exp(params[14] * data$EE) * 
      exp(params[15] * data$NE) * 
      exp(params[16] * data$NW) * 
      exp(params[17] * data$NI) * 
      exp(params[18] * data$SC) * 
      exp(params[19] * data$SE) * 
      exp(params[20] * data$SW) * 
      exp(params[21] * data$WA) * 
      exp(params[22] * data$YH) * 
      exp(params[23] * data$hedge)
    
    
    y <- 
      (exp(params[ 1]) * fixed) * data$a.h +
      (exp(params[ 2]) * fixed) * data$b.g +
      (exp(params[ 3]) * fixed) * data$b.t +
      (exp(params[ 4]) * fixed) * data$b.w +
      (exp(params[ 5]) * fixed) * data$c.l +
      (exp(params[ 6]) * fixed) * data$c.w +
      (exp(params[ 7]) * fixed) * data$f.s +
      (exp(params[ 8]) * fixed) * data$f.w +
      (exp(params[ 9]) * fixed) * data$h.r +
      (exp(params[10]) * fixed) * data$i.g +
      (exp(params[11]) * fixed) * data$r.k +
      (exp(params[12]) * fixed) * data$s.g
    
    # Return ?quasi-likelihood? (from Rhys)
    -sum(data$count / data$effective_area * log(y) - y)
  }
  
  fn_b3 <- function(params, data){
    fixed <- 
      exp(params[13] * data$EM) * 
      exp(params[14] * data$EE) * 
      exp(params[15] * data$NE) * 
      exp(params[16] * data$NW) * 
      exp(params[17] * data$NI) * 
      exp(params[18] * data$SC) * 
      exp(params[19] * data$SE) * 
      exp(params[20] * data$SW) * 
      exp(params[21] * data$WA) * 
      exp(params[22] * data$YH) * 
      exp(params[23] * data$a.h_buf) * 
      exp(params[24] * data$b.t_buf) * 
      exp(params[25] * data$w.d_buf)  
    
    
    y <- 
      (exp(params[ 1]) * fixed) * data$a.h +
      (exp(params[ 2]) * fixed) * data$b.g +
      (exp(params[ 3]) * fixed) * data$b.t +
      (exp(params[ 4]) * fixed) * data$b.w +
      (exp(params[ 5]) * fixed) * data$c.l +
      (exp(params[ 6]) * fixed) * data$c.w +
      (exp(params[ 7]) * fixed) * data$f.s +
      (exp(params[ 8]) * fixed) * data$f.w +
      (exp(params[ 9]) * fixed) * data$h.r +
      (exp(params[10]) * fixed) * data$i.g +
      (exp(params[11]) * fixed) * data$r.k +
      (exp(params[12]) * fixed) * data$s.g
    
    # Return ?quasi-likelihood? (from Rhys)
    -sum(data$count / data$effective_area * log(y) - y)
  }
  
  fn_b4 <- function(params, data){
    fixed <- 
      exp(params[13] * data$EM) * 
      exp(params[14] * data$EE) * 
      exp(params[15] * data$NE) * 
      exp(params[16] * data$NW) * 
      exp(params[17] * data$NI) * 
      exp(params[18] * data$SC) * 
      exp(params[19] * data$SE) * 
      exp(params[20] * data$SW) * 
      exp(params[21] * data$WA) * 
      exp(params[22] * data$YH) * 
      exp(params[23] * data$hedge) * 
      exp(params[24] * data$a.h_buf) * 
      exp(params[25] * data$b.t_buf) * 
      exp(params[26] * data$w.d_buf)  
    
    
    y <- 
      (exp(params[ 1]) * fixed) * data$a.h +
      (exp(params[ 2]) * fixed) * data$b.g +
      (exp(params[ 3]) * fixed) * data$b.t +
      (exp(params[ 4]) * fixed) * data$b.w +
      (exp(params[ 5]) * fixed) * data$c.l +
      (exp(params[ 6]) * fixed) * data$c.w +
      (exp(params[ 7]) * fixed) * data$f.s +
      (exp(params[ 8]) * fixed) * data$f.w +
      (exp(params[ 9]) * fixed) * data$h.r +
      (exp(params[10]) * fixed) * data$i.g +
      (exp(params[11]) * fixed) * data$r.k +
      (exp(params[12]) * fixed) * data$s.g
    
    # Return ?quasi-likelihood? (from Rhys)
    -sum(data$count / data$effective_area * log(y) - y)
  }
  
  
  ## Run optimisation - B1
  set.seed(29071989)
  i <- 1
  out_b1 <- tryCatch("1"+1, error = function(e) e)
  while(class(out_b1)[[1]] == "simpleError" & i <= 20){
    out_b1 <- tryCatch(Rcgmin(par = ifelse(rep(unique(data$gb_only) == 0, k_b1), 
                                           runif(k_b1, -10, 10),
                                           c(runif(16, -10, 10), 0, runif(k_b1 - 17, -10, 10))),
                              fn = fn_b1,
                              gr = NULL,
                              lower = ifelse(rep(unique(data$gb_only) == 0, k_b1), 
                                             rep(-10, k_b1), 
                                             c(rep(-10, 16), 0, rep(-10, k_b1 - 17))),
                              upper = ifelse(rep(unique(data$gb_only) == 0, k_b1), 
                                             rep(10, k_b1), 
                                             c(rep(10, 16), 0, rep(10, k_b1 - 17))),
                              data = data),
                       error = function(e) e)
    i <- i+1
  }
  
  # Stop if error after 20 attempts
  if(class(out_b1)[[1]] == "simpleError"){
    return(NA)
  }
  
  
  ## Run optimisation - B2
  i <- 1
  out_b2 <- tryCatch("1"+1, error = function(e) e)
  while(class(out_b2)[[1]] == "simpleError" & i <= 20){
    out_b2 <- tryCatch(Rcgmin(par = ifelse(rep(unique(data$gb_only) == 0, k_b2), 
                                           runif(k_b2, -10, 10),
                                           c(runif(16, -10, 10), 0, runif(k_b2 - 17, -10, 10))),
                              fn = fn_b2,
                              gr = NULL,
                              lower = ifelse(rep(unique(data$gb_only) == 0, k_b2), 
                                             rep(-10, k_b2), 
                                             c(rep(-10, 16), 0, rep(-10, k_b2 - 17))),
                              upper = ifelse(rep(unique(data$gb_only) == 0, k_b2), 
                                             rep(10, k_b2), 
                                             c(rep(10, 16), 0, rep(10, k_b2 - 17))),
                              data = data),
                       error = function(e) e)
    i <- i+1
  }
  
  # NA if error after 20 attempts
  if(class(out_b2)[[1]] == "simpleError"){
    out_b2 <- NA
  }
  
  
  
  ## Run optimisation - B3
  i <- 1
  out_b3 <- tryCatch("1"+1, error = function(e) e)
  while(class(out_b3)[[1]] == "simpleError" & i <= 20){
    out_b3 <- tryCatch(Rcgmin(par = ifelse(rep(unique(data$gb_only) == 0, k_b3), 
                                           runif(k_b3, -10, 10),
                                           c(runif(16, -10, 10), 0, runif(k_b3 - 17, -10, 10))),
                              fn = fn_b3,
                              gr = NULL,
                              lower = ifelse(rep(unique(data$gb_only) == 0, k_b3), 
                                             rep(-10, k_b3), 
                                             c(rep(-10, 16), 0, rep(-10, k_b3 - 17))),
                              upper = ifelse(rep(unique(data$gb_only) == 0, k_b3), 
                                             rep(10, k_b3), 
                                             c(rep(10, 16), 0, rep(10, k_b3 - 17))),
                              data = data),
                       error = function(e) e)
    i <- i+1
  }
  
  # NA if error after 20 attempts
  if(class(out_b3)[[1]] == "simpleError"){
    out_b3 <- NA
  }
  
  
  ## Run optimisation - B2
  i <- 1
  out_b4 <- tryCatch("1"+1, error = function(e) e)
  while(class(out_b4)[[1]] == "simpleError" & i <= 20){
    out_b4 <- tryCatch(Rcgmin(par = ifelse(rep(unique(data$gb_only) == 0, k_b4), 
                                           runif(k_b4, -10, 10),
                                           c(runif(16, -10, 10), 0, runif(k_b4 - 17, -10, 10))),
                              fn = fn_b4,
                              gr = NULL,
                              lower = ifelse(rep(unique(data$gb_only) == 0, k_b4), 
                                             rep(-10, k_b4), 
                                             c(rep(-10, 16), 0, rep(-10, k_b4 - 17))),
                              upper = ifelse(rep(unique(data$gb_only) == 0, k_b4), 
                                             rep(10, k_b4), 
                                             c(rep(10, 16), 0, rep(10, k_b4 - 17))),
                              data = data),
                       error = function(e) e)
    i <- i+1
  }
  
  # NA if error after 20 attempts
  if(class(out_b4)[[1]] == "simpleError"){
    out_b4 <- NA
  }
  
  
  # Calculate densities
  params_list_b1$param <- out_b1$par
  params_list_b2$param <- out_b2$par
  params_list_b3$param <- out_b3$par
  params_list_b4$param <- out_b4$par
  
  densities_b1 <- params_list_b1 %>% 
    filter(grepl("density", param_name)) %>% 
    rename(lcm = param_name,
           density = param) %>% 
    mutate(lcm = gsub("density_", "", lcm)) %>% 
    crossing(params_list_b1 %>% 
               filter(grepl("beta", param_name)) %>% 
               rename(nuts1 = param_name,
                      beta_nuts1 = param) %>% 
               mutate(nuts1 = gsub("beta_", "", nuts1)) %>% 
               # Add reference level
               add_row(nuts1 = "WM",
                       beta_nuts1 = 0) %>%
               # Add reference level
               add_row(nuts1 = "WM",
                       beta_nuts1 = 0) %>% 
               {if(unique(data$gb_only) == 1) add_row(., nuts1 = "NI", beta_nuts1 = 0) else .}) %>% 
    mutate(method = "B1",
           value = out_b1$value)
  
  densities_b2 <- params_list_b2 %>%
    filter(grepl("density", param_name)) %>% 
    rename(lcm = param_name,
           density = param) %>% 
    mutate(lcm = gsub("density_", "", lcm)) %>% 
    crossing(params_list_b2 %>% 
               filter(grepl("beta", param_name) & param_name != "beta_hedge") %>% 
               rename(nuts1 = param_name,
                      beta_nuts1 = param) %>% 
               mutate(nuts1 = gsub("beta_", "", nuts1)) %>% 
               # Add reference level
               add_row(nuts1 = "WM",
                       beta_nuts1 = 0) %>% 
               {if(unique(data$gb_only) == 1) add_row(., nuts1 = "NI", beta_nuts1 = 0) else .}) %>% 
    mutate(beta_hedge = params_list_b2$param[params_list_b2$param_name == "beta_hedge"],
           method = "B2",
           value = out_b2$value)
  
  densities_b3 <- params_list_b3 %>%
    filter(grepl("density", param_name)) %>% 
    rename(lcm = param_name,
           density = param) %>% 
    mutate(lcm = gsub("density_", "", lcm)) %>% 
    crossing(params_list_b3 %>% 
               filter(grepl("beta", param_name) & !grepl("_buf", param_name) & param_name != "beta_hedge") %>% 
               rename(nuts1 = param_name,
                      beta_nuts1 = param) %>% 
               mutate(nuts1 = gsub("beta_", "", nuts1)) %>% 
               # Add reference level
               add_row(nuts1 = "WM",
                       beta_nuts1 = 0) %>% 
               {if(unique(data$gb_only) == 1) add_row(., nuts1 = "NI", beta_nuts1 = 0) else .}) %>% 
    mutate(beta_a.h_buf = params_list_b3$param[params_list_b3$param_name == "beta_a.h_buf"],
           beta_b.t_buf = params_list_b3$param[params_list_b3$param_name == "beta_b.t_buf"],
           beta_w.d_buf = params_list_b3$param[params_list_b3$param_name == "beta_w.d_buf"],
           method = "B3",
           value = out_b3$value)
  
  densities_b4 <- params_list_b4 %>%
    filter(grepl("density", param_name)) %>% 
    rename(lcm = param_name,
           density = param) %>% 
    mutate(lcm = gsub("density_", "", lcm)) %>% 
    crossing(params_list_b4 %>% 
               filter(grepl("beta", param_name) & !grepl("_buf", param_name)) %>% 
               rename(nuts1 = param_name,
                      beta_nuts1 = param) %>% 
               mutate(nuts1 = gsub("beta_", "", nuts1)) %>% 
               # Add reference level
               add_row(nuts1 = "WM",
                       beta_nuts1 = 0) %>% 
               {if(unique(data$gb_only) == 1) add_row(., nuts1 = "NI", beta_nuts1 = 0) else .}) %>% 
    mutate(beta_hedge = params_list_b2$param[params_list_b2$param_name == "beta_hedge"],
           beta_a.h_buf = params_list_b4$param[params_list_b4$param_name == "beta_a.h_buf"],
           beta_b.t_buf = params_list_b4$param[params_list_b4$param_name == "beta_b.t_buf"],
           beta_w.d_buf = params_list_b4$param[params_list_b4$param_name == "beta_w.d_buf"],
           method = "B4",
           value = out_b4$value)
  
  
  # Return
  return(bind_rows(densities_b1,
                   densities_b2,
                   densities_b3,
                   densities_b4))
}

# Run
mod_b_out <- mod_b_data %>% 
  group_by(spp) %>% 
  nest() %>%
  ungroup() %>%
  mutate(mod = purrr::map(data, ~mod_b_fun(.))) %>% 
  select(-data) %>% 
  mutate(success = ifelse(purrr::map_int(mod, ~nrow(.)) > 0, 1, 0)) 


# Save
save(mod_a_out, mod_b_out, file = "rdata/bbs_mods_2023.RData")



## EXTRACT COVARS FROM SCENARIOS  -------------------
## Add novel land covers to lookup
lookup_lcm <- lookup_lcm %>% 
  bind_rows(tibble(lcm_layer = c(2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 9.1, 10.1, 3.2, 4.2, 3.3, 4.3, 3.31, 4.31, 3.4, 4.4, 5.2, 6.2, 7.2, 10.2),
                   lcm = c("c.w_pinewood", "a.h_silvoa", "i.g_silvop", "n.g_woodpa", "c.g_woodpa", "a.g_woodpa", "h.r_woodpa", "h.r_woodpa", "a.h_energy", "i.g_energy", "a.h_organic", "i.g_organic", "a.h_organic_silvoa", "i.g_organic_silvop", "a.h_palud", "i.g_palud", "n.g_new", "c.g_new", "a.g_new", "h.r_new")))


## Get geometry
# Aggregate raster 40-fold, so 25m grid becomes sqm
grid_sq_nir <- aggregate(!is.na(r_lcm_nir), 40, fun = sum)
grid_sq_gbr <- aggregate(!is.na(r_lcm_gbr), 40, fun = sum)

# Polygonise
grid_sq_nir <- rasterToPolygons(grid_sq_nir/1600, fun = function(x)x>0)
grid_sq_gbr <- rasterToPolygons(grid_sq_gbr/1600, fun = function(x)x>0)

# Convert to SF
grid_sq_nir <- st_as_sf(grid_sq_nir)
grid_sq_gbr <- st_as_sf(grid_sq_gbr)

# Add xy
xy_nir <- st_coordinates(st_centroid(grid_sq_nir))
xy_gbr <- st_coordinates(st_centroid(grid_sq_gbr))

grid_sq_nir <- grid_sq_nir %>% mutate(x = xy_nir[,1], y = xy_nir[,2])
grid_sq_gbr <- grid_sq_gbr %>% mutate(x = xy_gbr[,1], y = xy_gbr[,2])

# 2000m buffer around centroid
grid_buf_nir <- st_buffer(st_centroid(grid_sq_nir), dist = 2000, nQuadSegs = 10)
grid_buf_gbr <- st_buffer(st_centroid(grid_sq_gbr), dist = 2000, nQuadSegs = 10)

# Transform
grid_sq_nir <- st_transform(grid_sq_nir, crs(r_lcm_nir))
grid_sq_gbr <- st_transform(grid_sq_gbr, crs(r_lcm_gbr))
grid_buf_nir <- st_transform(grid_buf_nir, crs(r_lcm_nir))
grid_buf_gbr <- st_transform(grid_buf_gbr, crs(r_lcm_gbr))


# Function for xtracting LCM coverage in polygon 
mode_fun <- function(x, y) {
  x = x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

## Fun
get_covars_fun <- function(squares, buffers, lcm, nuts1, elev, hedge, hedgena, island, lookup_lcm, lookup_nuts1){
  # XY
  df_xy <- st_coordinates(st_centroid(st_transform(squares, bng))) %>%
    as.data.frame() %>% 
    as_tibble() %>% 
    set_names(c("x", "y"))
  
  # Mean altitude
  df_elev <- exactextractr::exact_extract(elev, st_transform(squares, crs(lcm)), 'mean')
  
  # Modal island/mainland
  df_island <- exactextractr::exact_extract(island, st_transform(squares, crs(lcm)), mode_fun)
  
  # Modal NUTS1
  df_nuts1 <- squares %>% 
    st_set_geometry(NULL) %>% 
    mutate(nuts1 = exact_extract(nuts1, st_transform(squares, crs(lcm)), fun = mode_fun)) %>% 
    rename(nuts1_layer = nuts1) %>% 
    left_join(lookup_nuts1, by = "nuts1_layer") %>% 
    select(-nuts1_layer)
  
  # LCM 1km
  df_lcm_sq <- exactextractr::exact_extract(lcm, st_transform(squares, crs(lcm)))
  df_lcm_sq <- df_lcm_sq %>% 
    enframe("id") %>% 
    mutate(value = purrr::map(value, ~{.x %>% 
        group_by(value) %>% 
        summarise(prop = sum(coverage_fraction), .groups = "drop")})) %>% 
    unnest(cols = c('value')) %>%
    rename(lcm_layer = value) %>% 
    mutate(lcm_layer = round(lcm_layer, 2)) %>% 
    left_join(lookup_lcm, by = "lcm_layer") %>% 
    group_by(id, lcm) %>% 
    summarise(prop = sum(prop), .groups = "drop") %>% 
    group_by(id) %>% 
    mutate(prop = prop / sum(prop)) %>% 
    ungroup() %>% 
    spread(lcm, prop, fill = 0) %>% 
    select(-`<NA>`) %>% 
    mutate_all(list(~ifelse(is.na(.), 0, .))) %>%
    set_names(paste0(names(.), "_sq")) %>% 
    rename(id = id_sq)        
  
  # LCM 2km
  df_lcm_buf <- exactextractr::exact_extract(lcm, st_transform(buffers, crs(lcm)))  
  df_lcm_buf <- df_lcm_buf %>% 
    enframe("id") %>% 
    mutate(value = purrr::map(value, ~{.x %>% 
        group_by(value) %>% 
        summarise(prop = sum(coverage_fraction)) %>% 
        ungroup()})) %>% 
    unnest(cols = c('value')) %>%
    rename(lcm_layer = value) %>% 
    mutate(lcm_layer = round(lcm_layer, 2)) %>% 
    left_join(lookup_lcm, by = "lcm_layer") %>%
    mutate(lcm = substr(lcm, 1, 3)) %>% # Use simplified LCM classes for buffer
    group_by(id, lcm) %>% 
    summarise(prop = sum(prop), .groups = "drop") %>% 
    group_by(id) %>% 
    mutate(prop = prop / sum(prop)) %>% 
    ungroup() %>% 
    spread(lcm, prop, fill = 0) %>% 
    select(-`<NA>`) %>% 
    mutate_all(list(~ifelse(is.na(.), 0, .))) %>%
    set_names(paste0(names(.), "_buf")) %>% 
    rename(id = id_buf)
  
  # Hedge (sq)
  df_hedge <- exactextractr::exact_extract(hedge, st_transform(squares, crs(lcm)), 'sum')
  df_hedge <- tibble(hedge_total = df_hedge)
  
  df_hedgena <- exactextractr::exact_extract(hedgena, st_transform(squares, crs(lcm)), 'sum')
  df_hedgena <- tibble(hedgena_total = df_hedgena)
  
  
  # Combine & return
  return(bind_cols(as_tibble(df_nuts1),
                   df_xy,
                   elev = df_elev,
                   island = df_island,
                   df_hedge,
                   df_hedgena,
                   df_lcm_sq %>% select(-id),
                   df_lcm_buf %>% select(-id)))
}

# Do NI  
bbs_covars_nir <- tibble(file = c("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_nir.tif", 
                                  "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_nir_2022.tif", 
                                  "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_b_nir_2022.tif",
                                  "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_c_nir_2022.tif",
                                  "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_c_waders_nir_2022.tif",
                                  "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_d_nir_2022.tif",
                                  "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_e_nir_2022.tif",
                                  "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_e_waders_nir_2022.tif",
                                  "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_f_nir_2022.tif", 
                                  "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_g_nir_2022.tif",
                                  "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_h_nir_2022.tif",
                                  "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_i_nir_2022.tif"),
                         band = c(1, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7),
                         scenario = c("2015", "0", "1", "2", "2w", "3", "4", "4w", "5", "6", "7", "8")) %>% 
  mutate(raster = purrr::map2(file, band, ~raster(x = .x, band = .y)),
         covars = purrr::map(raster, ~get_covars_fun(grid_sq_nir, grid_buf_nir, 
                                                     ., r_nuts1_nir, r_elev_nir, r_hedges_nir, r_hedgesna_nir, r_island_nir, 
                                                     lookup_lcm, lookup_nuts1)))


# Do GB (merge eng, sct & wal)  - slow!
bbs_covars_gbr <- tibble(file = list(list("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_eng.tif", 
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_wal.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_sct.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_eng_2022.tif", 
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_a_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_b_eng_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_b_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_b_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_c_eng_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_c_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_c_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_c_waders_eng_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_c_waders_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_c_waders_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_c_wadersoil_eng_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_c_wadersoil_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_c_wadersoil_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_d_eng_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_d_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_d_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_e_eng_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_e_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_e_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_e_waders_eng_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_e_waders_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_e_waders_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_e_wadersoil_eng_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_e_wadersoil_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_e_wadersoil_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_f_eng_2022.tif", 
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_f_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_f_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_g_eng_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_g_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_g_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_h_eng_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_h_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_h_sct_2022.tif"),
                                     list("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_i_eng_2022.tif", 
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_i_wal_2022.tif",
                                          "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_i_sct_2022.tif")),
                         band = c(1, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7),
                         scenario = c("2015", "0", "1", "2", "2w", "2s", "3", "4", "4w", "4s", "5", "6", "7", "8")) %>% 
  mutate(raster = purrr::map2(file, band, ~do.call(merge, lapply(.x, raster, band = .y))),
         covars = purrr::map(raster, ~get_covars_fun(grid_sq_gbr, grid_buf_gbr, 
                                                     ., r_nuts1_gbr, r_elev_gbr, r_hedges_gbr, r_hedgesna_gbr, r_island_gbr, 
                                                     lookup_lcm, lookup_nuts1)))


# Save 
save(bbs_scenarios_covars, file = "rdata/bbs_scenarios_covars_2023.RData")



## ORGANIC MULTIPLIERS ------------------
# Read in organic/conv abundances 
organic_multipliers <- read_csv("data/bbs/organic_multipliers.csv", skip = 1) %>% 
  select(-study, -scale, -unit, -source, -n_conv, -n_org) 

# Calculate mean weighted relative abundance (org / conv) per species
organic_multipliers <- organic_multipliers %>%
  filter(density_conv != 0 & density_org != 0) %>% 
  mutate(organic_multiplier = density_org / density_conv) %>% 
  group_by(spp) %>%
  mutate(weight = n_av / sum(n_av)) %>%
  summarise(organic_multiplier = sum(organic_multiplier * weight)) 

# Apply grand mean to missing farmland specialists
organic_multipliers <- organic_multipliers %>% 
  bind_rows(bbs_species %>% 
              filter(farmland_list == "specialist" & !spp %in% organic_multipliers$spp) %>% 
              select(spp) %>% 
              mutate(organic_multiplier = mean(organic_multipliers$organic_multiplier)))



## YIELD MULTIPLIERS -----------------------
# List CBC/BBS trend files (for FBI species, UK or England, whichever gives longest time-series)
files <- list.files("data/bbs/trends/") 
files <- files[files != "wheat_yield.csv"]

# Read in wheat yields
wheat_yields <- read_csv("data/bbs/trends/wheat_yield.csv") %>% select(-source)

# Generate new data (0 to 50% increase in wheat yields, in 1% increments)
newdata <- tibble(rel_yield = c(seq(1, 1.5, by = 0.01)),
                  wheat_yield = rel_yield * wheat_yields$wheat_yield[wheat_yields$year == 2015])

# Process data, fit models, generate predictions
bbs_yield_fits <- tibble(file = paste0("data/bbs/trends/", files)) %>% 
  mutate(# Read in and tidy data
    data = purrr::map(file, ~read_csv(.) %>% 
                        select(year, unsm) %>% 
                        rename(pop = unsm) %>%
                        mutate(logpop = log(pop)) %>% 
                        inner_join(wheat_yields, by = "year")),
    spp = substr(file, 17, 18),
    # Fit 2 models
    mod1 = purrr::map(data, ~lm(logpop ~ wheat_yield, data = .)),
    mod0 = purrr::map(data, ~lm(logpop ~ 1, data = .)),
    # Get AIC, identify top model
    aic1 = purrr::map_dbl(mod1, AIC),
    aic0 = purrr::map_dbl(mod0, AIC),
    r2_1 = purrr::map_dbl(mod1, ~summary(.)$r.squared),
    r2_0 = purrr::map_dbl(mod0, ~summary(.)$r.squared),
    top = ifelse(aic1<aic0, "mod1", "mod0"),
    # Get model predictions
    fit1 = purrr::map(mod1, ~predict(., newdata, interval = "confidence") %>% 
                        as_tibble() %>% 
                        set_names(c("fit1", "lwr1", "upr1"))),
    fit0 = purrr::map(mod0, ~predict(., newdata, interval = "confidence") %>% 
                        as_tibble() %>% 
                        set_names(c("fit0", "lwr0", "upr0"))))


# Tidy output
yield_multipliers <- bbs_yield_fits %>% 
  select(spp, top, fit1, fit0, r2_1, r2_0) %>% 
  mutate(newdata = list(newdata)) %>% 
  unnest(cols = c(fit1, fit0, newdata, r2_1, r2_0)) %>% 
  mutate(logpop = ifelse(top == "mod1", fit1, fit0),
         lwr = ifelse(top == "mod1", lwr1, lwr0),
         upr = ifelse(top == "mod1", upr1, upr0),
         r2 = ifelse(top == "mod1", r2_1, r2_0),
         pop = exp(logpop),
         lwr = exp(lwr),
         upr = exp(upr)) %>% 
  group_by(spp) %>% 
  mutate(yield_multiplier = pop/pop[rel_yield == 1],
         lwr = lwr/pop[rel_yield == 1],
         upr = upr/pop[rel_yield == 1]) %>%
  ungroup() %>% 
  select(spp, rel_yield, yield_multiplier, lwr, upr, r2) %>% 
  mutate(spp = toupper(spp)) %>% 
  filter(spp %in% (bbs_species %>% 
                     filter(!is.na(farmland_list)) %$% 
                     spp))


## PREDICT POP SIZE -----------------------------
load("rdata/bbs_mods_2023.RData")
load("rdata/bbs_scenarios_covars_2023.RData")

# Add empty columns to covars
names_full <- unique(unlist(lapply(bbs_scenarios_covars$covars, names)))
bbs_scenarios_covars <- bbs_scenarios_covars %>% 
  mutate(covars = purrr::map(covars, {. %>% 
      crossing(spread(tibble(var = names_full[!names_full %in% names(.)],
                             val = 0), 
                      var, val))}))


## Tidy density multipliers
# Tidy organic multipliers
organic_multipliers <- organic_multipliers %>%
  full_join(unique(select(mod_a_out, spp)), by = "spp") %>% 
  mutate(organic_multiplier = ifelse(is.na(organic_multiplier), 1, organic_multiplier)) 

# Yield growth
yield_multipliers <- yield_multipliers %>% 
  select(spp, rel_yield, yield_multiplier) %>% 
  mutate(rel_yield = round(rel_yield, 2)) 

load("rdata/food/food_results_gapclosed_2022.RData")
yield_multipliers <- food_results_gapclosed %>% 
  ungroup() %>% 
  unnest(cols = c(data)) %>% 
  filter(year == 2050 & option == "Focus on all three measures") %>% 
  select(scenario, yield) %>% 
  rename(rel_yield = yield) %>%
  mutate(rel_yield = round(rel_yield, 2)) %>%
  left_join(yield_multipliers, by = "rel_yield") %>% 
  select(spp, scenario, yield_multiplier) 

yield_multipliers <- yield_multipliers %>% filter(scenario == "5") %>% select(-scenario)



# Hedges
# Ambition combinations
ambition_combos <- read_csv("data/ambition_combinations_new.csv", skip = 0)
# Scenario params
scenario_params <- read_csv("data/scenario_parameters_new.csv", skip = 1)
# Get hedge params
hedge_increase <- ambition_combos %>% 
  select(scenario, ambition_hedge) %>% 
  rename(ambition = ambition_hedge) %>% 
  left_join(scenario_params %>% 
              filter(par == "hedge") %>% 
              mutate(hedge_increase = 1 + val * 6) %>% 
              select(ambition, hedge_increase), 
            by = "ambition") %>% 
  select(-ambition) %>% 
  mutate(scenario = as.character(scenario)) %>% 
  # Add row for 2015
  add_row(scenario = "2015",
          hedge_increase = 1)


# Tidy model outputs
mod_a_out. <- mod_a_out %>% 
  left_join(bird_spp %>% select(spp, species, gb_only), by = "spp") %>% 
  filter(success == 1)
mod_b_out. <- mod_b_out %>%
  filter(success == 1) %>% 
  left_join(bird_spp %>% select(spp, species, gb_only), by = "spp") %>% 
  mutate(mod = purrr::map(mod, . %>% 
                            mutate(nuts1 = case_when(nuts1 == "EM" ~ "East Midlands (England)",
                                                     nuts1 == "EE" ~ "East of England",
                                                     nuts1 == "NE" ~ "North East (England)",
                                                     nuts1 == "NW" ~ "North West (England)",
                                                     nuts1 == "NI" ~ "Northern Ireland",
                                                     nuts1 == "SC" ~ "Scotland",
                                                     nuts1 == "SE" ~ "South East & London (England)",
                                                     nuts1 == "SW" ~ "South West (England)",
                                                     nuts1 == "WA" ~ "Wales",
                                                     nuts1 == "WM" ~ "West Midlands (England)",
                                                     nuts1 == "YH" ~ "Yorkshire and The Humber")))) 



## Functions
# Model A
bird_predict_fun_a <- function(mods, x, organic_multipliers, yield_multipliers, yield_growth, hedge_increase){
  yield_multipliers <- yield_multipliers %>%
    full_join(select(mods, spp), by = "spp") %>% 
    mutate(yield_growth = yield_growth) %>% 
    mutate(yield_multiplier = ifelse(is.na(yield_multiplier), 1, yield_multiplier),
           yield_multiplier = ifelse(!yield_growth, 1, yield_multiplier)) %>% 
    select(-yield_growth) %>% 
    arrange(spp)
  
  
  newdata <- x %>% 
    # Fix hedges
    mutate(hedge_total = ifelse(hedgena_total > 0, 
                                mean(hedge_total[hedgena_total == 0 & nuts1 == "Northern Ireland"]),
                                hedge_total)) %>% 
    rename(hedge = hedge_total) %>% 
    select(-hedgena_total) %>% 
    # Tidy country/island attributes
    mutate(country = ifelse(nuts1 %in% c("Scotland", "Wales", "Northern Ireland"), nuts1, "England"),
           island = as.factor(ifelse(country == "Northern Ireland", 2, island)),
           nuts1 = ifelse(nuts1 %chin% c("South East (England)", "London"), "South East & London (England)", nuts1)) %>% 
    # Tidy buffer attributes
    mutate(w.d_buf = b.w_buf + c.w_buf) %>% 
    select(-contains("buf"), w.d_buf, b.t_buf, a.h_buf) %>% 
    # Combine categories
    mutate_at(vars(contains("_sq")), list(~ifelse(is.na(.), 0, .))) %>% 
    mutate(s.g_sq = c.g_sq + n.g_sq + a.g_sq + c.g_new_sq + n.g_new_sq + a.g_new_sq + c.g_woodpa_sq + n.g_woodpa_sq + a.g_woodpa_sq,
           h.r_sq = h.r_sq + h.r_woodpa_sq,
           a.h_sq = a.h_sq + a.h_energy_sq + a.h_palud_sq + a.h_silvoa_sq + a.h_organic_sq + a.h_organic_silvoa_sq,
           i.g_sq = i.g_sq + i.g_palud_sq + i.g_silvop_sq + i.g_organic_sq + i.g_organic_silvop_sq,
           c.l_sq = c.l_sq + s.m_sq,
           c.w_sq = c.w_sq + c.w_pinewood_sq + i.g_energy_sq,
           agfor = a.h_silvoa_sq + a.h_organic_silvoa_sq + i.g_silvop_sq + i.g_organic_silvop_sq + c.g_woodpa_sq + n.g_woodpa_sq + a.g_woodpa_sq + h.r_woodpa_sq,
           organic = a.h_organic_sq + a.h_organic_silvoa_sq + i.g_organic_sq + i.g_organic_silvop_sq) %>% 
    select(-c.g_sq, -n.g_sq, -a.g_sq, -c.g_new_sq, -n.g_new_sq, -a.g_new_sq, -c.g_woodpa_sq, -n.g_woodpa_sq, -a.g_woodpa_sq, -h.r_woodpa_sq,
           -a.h_energy_sq, -a.h_palud_sq, -a.h_silvoa_sq, -a.h_organic_silvoa_sq, -a.h_organic_sq,
           -i.g_energy_sq, -i.g_palud_sq, -i.g_silvop_sq, -i.g_organic_silvop_sq, -i.g_organic_sq,
           -c.w_pinewood_sq) %>%
    # Convert _sq to proportional areas (summing to 1, ignoring sea)
    mutate(sum = rowSums(across(contains("_sq")))) %>% 
    mutate_at(vars(contains("_sq")), list(~./sum)) %>%
    # Increase hedge length
    mutate(hedge_increase = ifelse(is.na(hedge_increase), 1, hedge_increase),
           hedge = hedge * hedge_increase) %>%
    select(-hedge_increase) %>%
    # Agroforestry
    mutate(hedge = hedge + 1000/30 * agfor) %>%
    select(-agfor) %>% 
    # Log-transform continuous variables
    mutate_at(vars(-nuts1, -x, -y, -elev, -island, -country, -grid, -sum, -organic), list(~log(.+0.01))) %>% 
    mutate(effective_area = sum)
  
  
  mods$pop <- NA
  for(i in 1:nrow(mods)){
    mods$pop[i] <- list(bind_rows(newdata %>%
                                    filter(if(mods$gb_only[i] == 1) nuts1 != "Northern Ireland" else is.character(nuts1)) %>% 
                                    mutate(pop = predict(mods$mod[[i]][[1]], ., type = "response"),
                                           pop_clip = ifelse(pop > quantile(pop, 0.99), quantile(pop, 0.99), pop)) %>% 
                                    mutate(pop = pop + pop * organic * (filter(organic_multipliers, spp == mods$spp[i])$organic_multiplier - 1),
                                           pop_clip = pop_clip + pop_clip * organic * (filter(organic_multipliers, spp == mods$spp[i])$organic_multiplier - 1)) %>% 
                                    mutate(farmed_sq = (exp(a.h_sq) - 0.01) + (exp(i.g_sq) - 0.01) - organic,
                                           pop = pop + pop * farmed_sq * (filter(yield_multipliers, spp == mods$spp[i])$yield_multiplier - 1),
                                           pop_clip = pop_clip + pop_clip * farmed_sq * (filter(yield_multipliers, spp == mods$spp[i])$yield_multiplier - 1)) %>% 
                                    group_by(country) %>%
                                    summarise(pop = sum(pop),
                                              pop_clip = sum(pop_clip)) %>% 
                                    mutate(method = "A1"),
                                  newdata %>%
                                    filter(if(mods$gb_only[i] == 1) nuts1 != "Northern Ireland" else is.character(nuts1)) %>% 
                                    mutate(pop = predict(mods$mod[[i]][[2]], ., type = "response"),
                                           pop_clip = ifelse(pop > quantile(pop, 0.99), quantile(pop, 0.99), pop)) %>% 
                                    mutate(pop = pop + pop * organic * (filter(organic_multipliers, spp == mods$spp[i])$organic_multiplier - 1),
                                           pop_clip = pop_clip + pop_clip * organic * (filter(organic_multipliers, spp == mods$spp[i])$organic_multiplier - 1)) %>% 
                                    mutate(farmed_sq = (exp(a.h_sq) - 0.01) + (exp(i.g_sq) - 0.01) - organic,
                                           pop = pop + pop * farmed_sq * (filter(yield_multipliers, spp == mods$spp[i])$yield_multiplier - 1),
                                           pop_clip = pop_clip + pop_clip * farmed_sq * (filter(yield_multipliers, spp == mods$spp[i])$yield_multiplier - 1)) %>% 
                                    group_by(country) %>% 
                                    summarise(pop = sum(pop),
                                              pop_clip = sum(pop_clip)) %>% 
                                    mutate(method = "A2"),
                                  newdata %>%
                                    filter(if(mods$gb_only[i] == 1) nuts1 != "Northern Ireland" else is.character(nuts1)) %>% 
                                    mutate(pop = predict(mods$mod[[i]][[3]], ., type = "response"),
                                           pop_clip = ifelse(pop > quantile(pop, 0.99), quantile(pop, 0.99), pop)) %>% 
                                    mutate(pop = pop + pop * organic * (filter(organic_multipliers, spp == mods$spp[i])$organic_multiplier - 1),
                                           pop_clip = pop_clip + pop_clip * organic * (filter(organic_multipliers, spp == mods$spp[i])$organic_multiplier - 1)) %>% 
                                    mutate(farmed_sq = (exp(a.h_sq) - 0.01) + (exp(i.g_sq) - 0.01) - organic,
                                           pop = pop + pop * farmed_sq * (filter(yield_multipliers, spp == mods$spp[i])$yield_multiplier - 1),
                                           pop_clip = pop_clip + pop_clip * farmed_sq * (filter(yield_multipliers, spp == mods$spp[i])$yield_multiplier - 1)) %>% 
                                    group_by(country) %>% 
                                    summarise(pop = sum(pop),
                                              pop_clip = sum(pop_clip)) %>% 
                                    mutate(method = "A3"),
                                  newdata %>%
                                    filter(if(mods$gb_only[i] == 1) nuts1 != "Northern Ireland" else is.character(nuts1)) %>% 
                                    mutate(pop = predict(mods$mod[[i]][[4]], ., type = "response"),
                                           pop_clip = ifelse(pop > quantile(pop, 0.99), quantile(pop, 0.99), pop)) %>% 
                                    mutate(pop = pop + pop * organic * (filter(organic_multipliers, spp == mods$spp[i])$organic_multiplier - 1),
                                           pop_clip = pop_clip + pop_clip * organic * (filter(organic_multipliers, spp == mods$spp[i])$organic_multiplier - 1)) %>%
                                    mutate(farmed_sq = (exp(a.h_sq) - 0.01) + (exp(i.g_sq) - 0.01) - organic,
                                           pop = pop + pop * farmed_sq * (filter(yield_multipliers, spp == mods$spp[i])$yield_multiplier - 1),
                                           pop_clip = pop_clip + pop_clip * farmed_sq * (filter(yield_multipliers, spp == mods$spp[i])$yield_multiplier - 1)) %>% 
                                    group_by(country) %>%
                                    summarise(pop = sum(pop),
                                              pop_clip = sum(pop_clip)) %>% 
                                    mutate(method = "A4")))  
  }
  
  mods %>%
    select(spp, species, pop) %>% 
    unnest(cols = c(pop))
}

# Model B
bird_predict_fun_b <- function(mods, x, organic_multipliers, yield_multipliers, yield_growth, hedge_increase){
  newdata <- x %>% 
    # Fix hedges
    mutate(hedge_total = ifelse(hedgena_total > 0, 
                                mean(hedge_total[hedgena_total == 0 & nuts1 == "Northern Ireland"]),
                                hedge_total)) %>% 
    rename(hedge = hedge_total) %>% 
    select(-hedgena_total) %>% 
    # Tidy country/island attributes
    mutate(country = ifelse(nuts1 %in% c("Scotland", "Wales", "Northern Ireland"), nuts1, "England"),
           nuts1 = ifelse(nuts1 %chin% c("South East (England)", "London"), "South East & London (England)", nuts1)) %>% 
    # Tidy buffer attributes
    mutate(w.d_buf = b.w_buf + c.w_buf) %>% 
    select(-contains("buf"), w.d_buf, b.t_buf, a.h_buf) %>% 
    # Combine categories
    mutate_at(vars(contains("_sq")), list(~ifelse(is.na(.), 0, .))) %>% 
    mutate(s.g_sq = c.g_sq + n.g_sq + a.g_sq + c.g_new_sq + n.g_new_sq + a.g_new_sq + c.g_woodpa_sq + n.g_woodpa_sq + a.g_woodpa_sq,
           h.r_sq = h.r_sq + h.r_woodpa_sq,
           a.h_sq = a.h_sq + a.h_energy_sq + a.h_palud_sq + a.h_silvoa_sq,
           a.h_sq_organic = a.h_organic_sq + a.h_organic_silvoa_sq,
           i.g_sq = i.g_sq + i.g_palud_sq + i.g_silvop_sq,
           i.g_sq_organic = i.g_organic_sq + i.g_organic_silvop_sq,
           c.l_sq = c.l_sq + s.m_sq,
           c.w_sq = c.w_sq + c.w_pinewood_sq + i.g_energy_sq,
           agfor = a.h_silvoa_sq + a.h_organic_silvoa_sq + i.g_silvop_sq + i.g_organic_silvop_sq + c.g_woodpa_sq + n.g_woodpa_sq + a.g_woodpa_sq + h.r_woodpa_sq) %>% 
    select(-c.g_sq, -n.g_sq, -a.g_sq, -c.g_new_sq, -n.g_new_sq, -a.g_new_sq, -c.g_woodpa_sq, -n.g_woodpa_sq, -a.g_woodpa_sq, -h.r_woodpa_sq,
           -a.h_energy_sq, -a.h_palud_sq, -a.h_silvoa_sq, -a.h_organic_silvoa_sq, -a.h_organic_sq,
           -i.g_energy_sq, -i.g_palud_sq, -i.g_silvop_sq, -i.g_organic_silvop_sq, -i.g_organic_sq,
           -c.w_pinewood_sq, -s.m_sq) %>%
    # Increase hedge length
    mutate(hedge_increase = ifelse(is.na(hedge_increase), 1, hedge_increase),
           hedge = hedge * hedge_increase) %>% 
    select(-hedge_increase) %>% 
    # Agroforestry
    mutate(hedge = hedge + 1000/30 * agfor) %>%
    select(-agfor, -x, -y, -elev, -grid, -island) %>% 
    # Gather
    gather(lcm, area, -nuts1, -country, -hedge, -a.h_buf, -b.t_buf, -w.d_buf) %>%  
    mutate(lcm = gsub("_sq", "", lcm),
           organic = grepl("_organic", lcm),
           lcm = gsub("_organic", "", lcm)) %>% 
    # Log-transform continuous variables
    mutate_at(vars(hedge, w.d_buf, b.t_buf, a.h_buf), list(~log(.+0.01))) %>% 
    filter(area != 0)
  
  mods <- mods %>%
    left_join(organic_multipliers, by = "spp") %>% 
    left_join(yield_multipliers, by = "spp") %>% 
    mutate(yield_growth = yield_growth) %>% 
    mutate(yield_multiplier = ifelse(is.na(yield_multiplier), 1, yield_multiplier),
           yield_multiplier = ifelse(!yield_growth, 1, yield_multiplier)) %>% 
    select(-yield_growth) %>% 
    arrange(spp)
  
  mods$pop <- NA
  
  for(i in 1:nrow(mods)){
    mods$pop[i] <- list(newdata %>% 
                          left_join(mods$mod[[i]], by = c("nuts1", "lcm")) %>%
                          mutate_at(vars(beta_hedge, beta_a.h_buf, beta_b.t_buf, beta_w.d_buf), ~ifelse(is.na(.), 0, .)) %>% 
                          mutate(beta_nuts1 = ifelse(beta_nuts1 == 0 & nuts1 == "Northern Ireland", -Inf, beta_nuts1)) %>% 
                          mutate(density = 
                                   exp(density) * exp(beta_nuts1) * 
                                   exp(beta_hedge * hedge) * 
                                   exp(beta_a.h_buf * a.h_buf) * exp(beta_b.t_buf * b.t_buf) * exp(beta_w.d_buf * w.d_buf),
                                 density = density + density * (organic * (mods$organic_multiplier[i] - 1)),
                                 density = density + density * ((lcm %in% c("a.h", "i.g") & !organic) * (mods$yield_multiplier[i] - 1))) %>%
                          group_by(country, method) %>% 
                          summarise(pop = sum(area * density),
                                    .groups = "drop"))
  }
  
  mods %>%
    select(spp, species, pop) %>% 
    unnest(cols = c(pop))
}


# Run and combine
out_a <- bbs_scenarios_covars %>%
  mutate(scenario1 = ifelse(scenario == "2015", scenario, substr(scenario, 1, 1))) %>% 
  left_join(hedge_increase, by = c("scenario1" = "scenario")) %>%
  mutate(yield_growth = FALSE) %>%
  bind_rows((.) %>% 
              filter(scenario == 5) %>% 
              mutate(yield_growth = TRUE)) %>% 
  mutate(args = purrr::transpose(list(yield_growth, hedge_increase))) %>% 
  mutate(pop = purrr::map2(covars, args, ~bird_predict_fun_a(mod_a_out., .x, organic_multipliers, yield_multipliers, .y[[1]], .y[[2]]))) %>% 
  select(-covars, -scenario1, -hedge_increase, -args) %>% 
  unnest(cols = c(pop)) 
out_b <- bbs_scenarios_covars %>% 
  mutate(scenario1 = ifelse(scenario == "2015", scenario, substr(scenario, 1, 1))) %>% 
  left_join(hedge_increase, by = c("scenario1" = "scenario")) %>% 
  mutate(yield_growth = FALSE) %>%
  bind_rows((.) %>% 
              filter(scenario == 5) %>% 
              mutate(yield_growth = TRUE)) %>% 
  mutate(args = purrr::transpose(list(yield_growth, hedge_increase))) %>% 
  mutate(pop = purrr::map2(covars, args, ~bird_predict_fun_b(mod_b_out., .x, organic_multipliers, yield_multipliers, .y[[1]], .y[[2]]))) %>% 
  select(-covars, -scenario1, -hedge_increase, -args) %>% 
  unnest(cols = c(pop)) 



## SUMMARISE GEOMETRIC MEAN ACROSS GROUPS -------------------
# Define species groups
bird_spp <- as_tibble(bird_spp) %>% 
  mutate_all(~ifelse(. == "", NA, .))

spp_groups <- bind_rows(bird_spp %>% 
                          select(spp) %>% 
                          mutate(group = "All species"),
                        bird_spp %>%
                          filter(!is.na(farmland_list)) %>%
                          select(spp) %>%
                          mutate(group = "Farmland birds"),
                        bird_spp %>% 
                          filter(farmland_list == "specialist") %>% 
                          select(spp) %>% 
                          mutate(group = "Farmland specialists"),
                        bird_spp %>% 
                          filter(farmland_list == "generalist") %>% 
                          select(spp) %>% 
                          mutate(group = "Farmland generalists"),
                        bird_spp %>%
                          filter(!is.na(woodland_list)) %>%
                          select(spp) %>%
                          mutate(group = "Woodland birds"),
                        bird_spp %>% 
                          filter(woodland_list  == "specialist") %>% 
                          select(spp) %>% 
                          mutate(group = "Woodland specialists"),
                        bird_spp %>% 
                          filter(woodland_list  == "generalist") %>% 
                          select(spp) %>% 
                          mutate(group = "Woodland generalists"),
                        bird_spp %>% 
                          filter(bocc4 != "green") %>% 
                          select(spp) %>% 
                          mutate(group = "BoCC Red/Amber (old)"),
                        bird_spp %>% 
                          filter(bocc5 != "green") %>% 
                          select(spp) %>% 
                          mutate(group = "BoCC Red/Amber"),
                        bird_spp %>% 
                          filter(rspb_list == "priority") %>% 
                          select(spp) %>% 
                          mutate(group = "RSPB Priority Species"),
                        bird_spp %>% 
                          filter(spp %in% c("CU", "RK", "SN", "L.", "OC")) %>% 
                          mutate(group = "Farmland waders")) %>% 
  select(spp, group)


# Calculate relative population size
pop_rel <- bind_rows(out_a %>% select(-pop) %>% rename(pop = pop_clip),
                     out_b) %>% 
  bind_rows((.) %>% 
              mutate(country = "UK")) %>% 
  bind_rows((.) %>% 
              filter(scenario == "2015") %>% 
              mutate(yield_growth = TRUE)) %>% 
  group_by(scenario, spp, species, method, country, yield_growth) %>% 
  summarise(pop = sum(pop),
            .groups = "drop") %>%
  group_by(species, country, method, yield_growth) %>% 
  mutate(pop_rel = pop/pop[scenario == "2015"]) %>%
  ungroup()

# Identify top model
mod_a_out. <- mod_a_out. %>% 
  mutate(topmod_a = paste0("A", purrr::map_chr(mod, ~names(get.models(model.sel(.[1:4]), 1)))))

mod_b_out. <- mod_b_out. %>% 
  mutate(topmod_b = purrr::map_chr(mod, ~unique(.$method[.$value == min(.$value)])))

bbs_model_list <- mod_a_out. %>% 
  mutate(topmod_a = as.integer(substr(topmod_a, 2, 2))) %>%
  rowwise() %>% 
  mutate(topmod = list(mod[[topmod_a]])) %>%
  ungroup() %>% 
  select(spp, gb_only, topmod)


# Identify species subsets
pop_rel <- pop_rel %>% 
  group_by(spp, scenario, yield_growth, country) %>% 
  mutate(species_subset = ifelse(length(method) == 8, "Restricted", "Full")) %>% 
  ungroup() 

pop_rel <- bind_rows(pop_rel %>% 
                       filter(species_subset == "Restricted"),
                     pop_rel %>% 
                       mutate(species_subset = "Full")) %>% 
  filter(!is.na(pop_rel))

pop_rel %>% 
  left_join(select(mod_a_out., spp, topmod_a), by = "spp") %>% 
  left_join(select(mod_b_out., spp, topmod_b), by = "spp") %>% 
  filter(method == topmod_a | method == topmod_b) %>% 
  mutate(method = substr(method, 1, 1)) %>% 
  left_join(spp_groups, by = "spp") %>% 
  filter(group == "RSPB Priority Species") %>% 
  filter(country == "UK" & !yield_growth & species_subset == "Full" & scenario %in% c("3", "4", "5", "8")) %>% 
  group_by(scenario, species) %>%  
  summarise(pop_rel = exp(mean(log(pop_rel))),
            nmod = length(spp),
            .groups = "drop") %>%
  filter(pop_rel < 0.9 & nmod == 2) %>% View

pop_sum <- bind_rows(
  # All 8 models
  pop_rel %>% 
    left_join(spp_groups, by = "spp") %>% 
    group_by(scenario, yield_growth, country, method, group, species_subset) %>% 
    summarise(n = length(spp),
              pop_rel = exp(mean(log(pop_rel))), 
              .groups = "drop"),
  # Top model only
  pop_rel %>% 
    left_join(select(mod_a_out., spp, topmod_a), by = "spp") %>% 
    left_join(select(mod_b_out., spp, topmod_b), by = "spp") %>% 
    filter(method == topmod_a | method == topmod_b) %>% 
    mutate(method = substr(method, 1, 1)) %>% 
    left_join(spp_groups, by = "spp") %>% 
    group_by(scenario, yield_growth, country, method, group, species_subset) %>% 
    summarise(n = length(spp),
              pop_rel = exp(mean(log(pop_rel))), 
              .groups = "drop") 
)


save(pop_rel, pop_sum, file = "rdata/bird_results_2023.RData")