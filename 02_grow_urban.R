## INITS -------------------------------
source("setup.R")


## LCM RASTERS -----------------------------
r_lcm_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_nir.tif")
r_lcm_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_eng.tif")
r_lcm_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_wal.tif")
r_lcm_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_sct.tif")


## PEAT RASTERS -----------------------------
r_peat_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_nir.tif")
r_peat_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_eng.tif")
r_peat_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_wal.tif")
r_peat_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_sct.tif")


## SALTMARSH RASTERS -----------------------------
r_saltmarsh_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_nir.tif")
r_saltmarsh_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_eng.tif")
r_saltmarsh_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_wal.tif")
r_saltmarsh_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_sct.tif")


## POPULATION TRAJECTORIES (UN) -------------------------
pop <- read_csv("data/population.csv", skip = 1) %>% select(-source, -notes)

# Use UN medium, which comes close (73.0m) to ONS projection of 72.4m by 2043 https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/nationalpopulationprojections/2018based
pop <- pop %>% 
  filter(est %in% c("estimate", "medium variant")) %>%
  filter(!(year == 2020 & est == "estimate")) %>% 
  select(year, pop) %>% 
  mutate(pop_rel = pop/pop[year == 2015]) %>% 
  filter(year %in% seq(2015, 2100, by = 5)) 



# 2019 and 2050 built area from CCC 2020 (data from Fig 7.10, from spreadsheet "The charts and data behind the 6th Carbon Budget reports")
built <- tibble(year = c(2019, 2050),
                kha = c(1796, 2226)) %>% 
  # Calculate annual % increase
  mutate(d_year = max(year) - min(year),
         d_kha = max(kha) - min(kha)) %>% 
  summarise(annual_rate = unique(d_kha/d_year) / min(kha)) %>% 
  # Cross with all years
  crossing(year = seq(2015, 2050, by = 5)) %>%
  # Calculate 5-yearly increase (relative to 2015)
  mutate(built = 1 + annual_rate * (year - 2015)) %>% 
  select(-annual_rate)



## IDENTIFY ORDER FOR NEW BUILT LAND ----------------------------
# Function - rank by distance to existing urban
lcm <- r_lcm_nir; peat <- r_peat_nir; saltmarsh <- r_saltmarsh_nir


builtorder_fun <- function(lcm, peat, saltmarsh, built, t){
  # Built land == 1
  r_built <- lcm %in% c(20, 21)
  
  # Everything except arable/i.g == NA
  r_built[!(lcm %in% c(3, 4, 20, 21))] <- NA
  
  # NA if peat
  r_built[peat == 1] <- NA
  
  # NA if managed realignment
  r_built[!is.na(saltmarsh)] <- NA
  
  # Distance to nearest built function
  dist_to_focal <- function(r){
    # Target cells - non-0
    target_cells <- which(r[] != 0)
    target_xy <- cbind(xyFromCell(r, target_cells))
    target_tree <- createTree(coordinates(target_xy))
    # Focal cells - non-NA 
    focal_cells <- which(!(is.na(r[])))
    focal_xy <- cbind(xyFromCell(r, focal_cells))
    ids <- knnLookup(target_tree, newdat= coordinates(focal_xy), k = 1)
    
    r[focal_cells] <- sqrt((focal_xy[, 1]-target_xy[ids, 1])^2 + (focal_xy[,2]-target_xy[ids, 2])^2)
    return(r)
  }
  
  # Calculate disance to nearest built
  r_built$dist <- dist_to_focal(r_built)
  r_built$dist[r_built$layer == 1] <- NA
  
  
  # Rank order
  set.seed(1)
  r_built$rank <- rank(r_built$dist[], ties = "random")
  r_built$rank[is.na(r_built$dist)] <- NA
  
  # Number of urban pixels in 2015
  n_init <- cellStats(r_built[[1]], sum)
  
  # Year order
  r_built$year <- NA
  
  # Loop through years, add year order
  for(i in 1:(t-1)){
    r_built$year[r_built$rank <= round(n_init * (built$built[i+1] - 1), 0) & r_built$rank > round(n_init * (built$built[i] - 1), 0)] <- i
  }
  
  return(r_built$year)
}

# Run (each country)
t <- 8 # 8 periods, 2020-2050
r_builtorder_nir <- builtorder_fun(r_lcm_nir, r_peat_nir, r_saltmarsh_nir, built, t)
r_builtorder_eng <- builtorder_fun(r_lcm_eng, r_peat_eng, r_saltmarsh_eng, built, t)
r_builtorder_wal <- builtorder_fun(r_lcm_wal, r_peat_wal, r_saltmarsh_wal, built, t)
r_builtorder_sct <- builtorder_fun(r_lcm_sct, r_peat_sct, r_saltmarsh_sct, built, t)


## CREATE 5-YEARLY RASTERS -------------------------
# Create raster for each period
r_lcm_5y_nir <- rep(list(r_lcm_nir), t)
r_lcm_5y_eng <- rep(list(r_lcm_eng), t)
r_lcm_5y_sct <- rep(list(r_lcm_sct), t)
r_lcm_5y_wal <- rep(list(r_lcm_wal), t)

# Loop through each period, create new urban land to keep pace with pop growth (LCM = 21, suburban, which is treated the same as urban)
v_builtorder_nir <- r_builtorder_nir[]
v_builtorder_eng <- r_builtorder_eng[]
v_builtorder_sct <- r_builtorder_sct[]
v_builtorder_wal <- r_builtorder_wal[]

for(i in 1:(t - 1)){
  r_lcm_5y_nir[[i+1]][v_builtorder_nir <= i] <- 21
  r_lcm_5y_eng[[i+1]][v_builtorder_eng <= i] <- 21
  r_lcm_5y_sct[[i+1]][v_builtorder_sct <= i] <- 21
  r_lcm_5y_wal[[i+1]][v_builtorder_wal <= i] <- 21
}

# Stack rasters
r_lcm_5y_nir <- stack(r_lcm_5y_nir)
r_lcm_5y_eng <- stack(r_lcm_5y_eng)
r_lcm_5y_sct <- stack(r_lcm_5y_sct)
r_lcm_5y_wal <- stack(r_lcm_5y_wal)



## SAVE --------------------------------
# Save
writeRaster(r_lcm_5y_nir, "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_nir.tif", overwrite = TRUE)
writeRaster(r_lcm_5y_eng, "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_eng.tif", overwrite = TRUE)
writeRaster(r_lcm_5y_sct, "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_sct.tif", overwrite = TRUE)
writeRaster(r_lcm_5y_wal, "C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_wal.tif", overwrite = TRUE)
