## INITS -------------------------------
source("setup.R")

## LOAD RASTERS -----------------------------
# Peat extent
r_peat_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_nir.tif")
r_peat_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_eng.tif")
r_peat_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_wal.tif")
r_peat_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_sct.tif")

# LCM
r_lcm_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_nir.tif")
r_lcm_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_eng.tif")
r_lcm_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_wal.tif")
r_lcm_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_sct.tif")

# ALC
r_alc_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc_nir.tif")
r_alc_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc_eng.tif")
r_alc_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc_wal.tif")
r_alc_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc_sct.tif")

# Protected areas
r_protected_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_protected_nir.tif")
r_protected_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_protected_eng.tif")
r_protected_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_protected_wal.tif")
r_protected_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_protected_sct.tif")

# Fen/bog classification
r_fenbog_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_fenbog_nir.tif")
r_fenbog_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_fenbog_eng.tif")
r_fenbog_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_fenbog_wal.tif")
r_fenbog_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_fenbog_sct.tif")

# Caledonian pinewood
r_pinewood_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_pinewood_sct.tif")

# PAWS
r_paws_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_paws_nir.tif")
r_paws_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_paws_eng.tif")
r_paws_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_paws_wal.tif")
r_paws_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_paws_sct.tif")


## CALCULATE AREAS OF EACH PEAT*LCM CLASS --------------------------
# Distance to focal function
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


# Function for calculating areas of each peat*LCM class, and rank for restoration
# peat <- r_peat_sct; lcm <- r_lcm_sct; alc <- r_alc_sct; protected <- r_protected_sct; paws <- r_paws_sct; pinewood <- r_pinewood_sct
peatrank_fun <- function(peat, lcm, alc, protected, paws, pinewood = NA){
  v_lcm <- lcm[]
  
  # Assume built, rock, etc. is non-peat
  # Create stack, 1 layer for each peatclass 
  r_peatclass <- stack(replicate(7, peat))
  
  # Identify peat classes, based on LCM
  if(class(pinewood) == "RasterLayer"){
    r_peatclass[[2]][!v_lcm %in% 2 | pinewood[] == 1] <- NA # Coniferous woodland, excluding Caledonian Pinewood
  } else{
    r_peatclass[[2]][!v_lcm %in% 2] <- NA # Coniferous woodland
  }
  r_peatclass[[3]][!v_lcm %in% 3] <- NA
  r_peatclass[[4]][!v_lcm %in% 4] <- NA
  r_peatclass[[5]][!v_lcm %in% c(5, 6, 7, 9, 10)] <- NA
  r_peatclass[[6]][!v_lcm %in% c(8, 11)] <- NA
  r_peatclass[[7]][!v_lcm %in% 14] <- NA
  names(r_peatclass) <- c("peat", "forest", "crop", "improved_grass", "extensive_grass", "seminatural", "water")
  
  # Aggregate 40x (mean) - 0.0625 to 1 ha, or 25-m to 1000-m
  r40_peatclass <- aggregate(r_peatclass, fact = 40, fun = sum) * 0.0625
  r40_peatclass[is.na(r40_peatclass)] <- 0
  r40_peatclass[r40_peatclass$peat == 0] <- NA
  
  # Distance to bog/fen 
  r40_peatclass$dist <- dist_to_focal(r40_peatclass$seminatural)
  
  # Get cell ID
  r40_peatclass$cell <- 1:ncell(r40_peatclass)
  
  # Aggregate ALC 40x (modal)
  r40_peatclass$alc <- aggregate(alc, fact = 40, fun = modal) 
  
  # Aggregate protected 40x (mean)
  r40_peatclass$protected <- aggregate(protected, fact = 40, fun = mean) 
  
  # Aggregate PAWS 40x (mean)
  r40_peatclass$paws <- aggregate(paws, fact = 40, fun = sum) / 1600
  
  # Dataframe  
  set.seed(1)
  df_peatrank <- r40_peatclass %>%
    as.data.frame() %>% 
    as.tbl() %>% 
    filter(!is.na(peat)) %>%
    mutate(paws = ifelse(is.na(paws), 0, paws)) %>% 
    # Randomise
    sample_frac(1) %>%
    # Then arrange & rank
    arrange(paws, -protected, -alc, dist) %>% 
    mutate(peatrank = 1:length(dist)) %>% 
      mutate(seminatural = seminatural + water) %>% 
    select(-water)
  
  # Project rank back to 25 ha
  r40_peatrank <- subsDT(r40_peatclass$cell, select(df_peatrank, cell, peatrank))
  
  # Project cell IDS back to 0.0625 ha
  r_peatrank <- projectRaster(r40_peatrank, peat, method = 'ngb')
  
  # If no peat, cell ID <- NA 
  r_peatrank[!peat %in% 1] <- NA
  
  # Return
  return(list(df_peatrank = df_peatrank,
              r_peatrank = r_peatrank))
}

# Run
peatrank_nir <- peatrank_fun(r_peat_nir, r_lcm_nir, r_alc_nir, r_protected_nir, r_paws_nir)
peatrank_eng <- peatrank_fun(r_peat_eng, r_lcm_eng, r_alc_eng, r_protected_eng, r_paws_eng)
peatrank_wal <- peatrank_fun(r_peat_wal, r_lcm_wal, r_alc_wal, r_protected_wal, r_paws_wal)
peatrank_sct <- peatrank_fun(r_peat_sct, r_lcm_sct, r_alc_sct, r_protected_sct, r_paws_sct, r_pinewood_sct)

# Extract df
df_peatrank_nir <- peatrank_nir$df_peatrank
df_peatrank_eng <- peatrank_eng$df_peatrank
df_peatrank_wal <- peatrank_wal$df_peatrank
df_peatrank_sct <- peatrank_sct$df_peatrank

# Extract raster (stack with fen/bog)
r_peatrank_nir <- stack(peatrank_nir$r_peatrank, r_fenbog_nir)
r_peatrank_eng <- stack(peatrank_eng$r_peatrank, r_fenbog_eng)
r_peatrank_wal <- stack(peatrank_wal$r_peatrank, r_fenbog_wal)
r_peatrank_sct <- stack(peatrank_sct$r_peatrank, r_fenbog_sct)


## SAVE ------------------------------------
# Df
save(df_peatrank_nir, df_peatrank_eng, df_peatrank_wal, df_peatrank_sct, file = "rdata/intermediate/df_peatrank.RData")
load("rdata/intermediate/df_peatrank.RData")

# Raster
writeRaster(r_peatrank_nir, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_peatrank_nir.tif", overwrite = TRUE)
writeRaster(r_peatrank_eng, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_peatrank_eng.tif", overwrite = TRUE)
writeRaster(r_peatrank_sct, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_peatrank_sct.tif", overwrite = TRUE)
writeRaster(r_peatrank_wal, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_peatrank_wal.tif", overwrite = TRUE)
