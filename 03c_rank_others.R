## INITS -------------------------------
source("setup.R")


## RASTERS -----------------------------
# LCM
r_lcm_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_nir.tif")
r_lcm_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_eng.tif")
r_lcm_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_wal.tif")
r_lcm_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_lcm_new_sct.tif")

# ALC
r_alc_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc_nir.tif")
r_alc_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc2_eng.tif") # New version with 3a/b
r_alc_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc_wal.tif")
r_alc_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_alc_sct.tif")

# Woodland opportunity
r_woodop_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_woodop_nir.tif")
r_woodop_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_woodop_eng.tif")
r_woodop_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_woodop_wal.tif")
r_woodop_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_woodop_sct.tif")

# PAWS
r_paws_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_paws_nir.tif")
r_paws_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_paws_eng.tif")
r_paws_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_paws_wal.tif")
r_paws_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_paws_sct.tif")

# Peat
r_peat_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_nir.tif")
r_peat_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_eng.tif")
r_peat_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_wal.tif")
r_peat_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_peat_sct.tif")

# Saltmarsh
r_saltmarsh_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_nir.tif")
r_saltmarsh_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_eng.tif")
r_saltmarsh_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_wal.tif")
r_saltmarsh_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_sct.tif")

# Caledonian pinewood
r_pinewood_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_pinewood_sct.tif")

# Soiltype (mineral or not)
r_soiltype_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_soiltype_nir.tif")
r_soiltype_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_soiltype_eng.tif")
r_soiltype_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_soiltype_wal.tif")
r_soiltype_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_soiltype_sct.tif")

# Yield Class from ESC (SBI only)
r_cyc_nir <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_cyc_nir.tif")[[6]]
r_cyc_eng <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_cyc_eng.tif")[[6]]
r_cyc_wal <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_cyc_wal.tif")[[6]]
r_cyc_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/r_cyc_sct.tif")[[6]]



## MASK OUT FUTURE URBAN LAND ------------------------------
# Load 5-y raster with urban growth
r_lcm_5y_nir <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_nir.tif")
r_lcm_5y_eng <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_eng.tif")
r_lcm_5y_wal <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_wal.tif")
r_lcm_5y_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/scenarios/r_lcm_5y_sct.tif")

# Grab 2050 urban 
v_urban_nir <- r_lcm_5y_nir[[nlayers(r_lcm_5y_nir)]][] %in% c(20, 21)
v_urban_eng <- r_lcm_5y_eng[[nlayers(r_lcm_5y_eng)]][] %in% c(20, 21)
v_urban_wal <- r_lcm_5y_wal[[nlayers(r_lcm_5y_wal)]][] %in% c(20, 21)
v_urban_sct <- r_lcm_5y_sct[[nlayers(r_lcm_5y_sct)]][] %in% c(20, 21)

# If urban by end of time-series, woodop <- NA
r_woodop_nir[v_urban_nir] <- NA
r_woodop_eng[v_urban_eng] <- NA
r_woodop_wal[v_urban_wal] <- NA
r_woodop_sct[v_urban_sct] <- NA

# If urban by end of time-series, LCM <- NA
r_lcm_nir[v_urban_nir] <- NA
r_lcm_eng[v_urban_eng] <- NA
r_lcm_wal[v_urban_wal] <- NA
r_lcm_sct[v_urban_sct] <- NA

# If SBI yield class <2 (rounds to 0) then assume no woodop
r_woodop_nir[r_cyc_nir < 2] <- NA
r_woodop_eng[r_cyc_eng < 2] <- NA
r_woodop_wal[r_cyc_wal < 2] <- NA
r_woodop_sct[r_cyc_sct < 2] <- NA


rm(v_urban_nir, v_urban_eng, v_urban_wal, v_urban_sct)
gc()




## HABITAT CHANGE ORDER ---------------------------
# Function for calculating distance from non-NA cells to non-0 cells
dist_to_focal <- function(r){
  # Target cells - wooded
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



# Function for calculating woodland / agroforestry / grassland ranking
otherrank_fun <- function(r_lcm, r_alc, r_woodop, r_peat, r_saltmarsh, r_paws, r_soiltype, r_pinewood = NA, country){
  print("Aggregating rasters 20-fold")
  
  # Get BMV land (1, 2, 3A) - country-specific codes
  if(country == "eng"){r_bmv <- r_alc %in% c(1, 2, 3)}
  if(country == "nir"){r_bmv <- r_alc %in% c(1, 2)}
  if(country == "sct"){r_bmv <- r_alc %in% c(1, 2, 3)}
  if(country == "wal"){r_bmv <- r_alc %in% c(1, 2, 3)}
  
  # Calculate modal ALC (25 ha units)
  r20 <- aggregate(r_alc, fact = 20, fun = modal)
  names(r20) <- "alc"
  
  # Get 25-ha cell ID
  r20$cell <- 1:ncell(r20)
  
  # Calculate area of woodland opportunity (25 ha units)
  r20$woodop <- aggregate(r_woodop & !r_bmv, fact = 20, fun = sum) * 0.0625
=  
  # Calculate potential area of each agroforestry system, constrained by woodop
  r20$silvop <- aggregate((r_lcm %in% 4) & r_woodop, fact = 20, fun = sum) * 0.0625
  r20$silvopbmv <- aggregate((r_lcm %in% 4) & r_woodop & r_bmv, fact = 20, fun = sum) * 0.0625
  r20$silvoa <- aggregate((r_lcm %in% 3) & r_woodop, fact = 20, fun = sum) * 0.0625
  r20$silvoabmv <- aggregate((r_lcm %in% 3) & r_woodop & r_bmv, fact = 20, fun = sum) * 0.0625
  r20$woodpa_sng <- aggregate((r_lcm %in% c(5, 6, 7, 9, 10)) & r_woodop & !r_bmv, fact = 20, fun = sum) * 0.0625
  r20$woodpa_img <- aggregate(r_lcm %in% 4 & r_woodop & !r_bmv, fact = 20, fun = sum) * 0.0625
  
  # Calculate potential area of seminatural grassland (from i.g. or a.h)
  r20$grassop_arable <- aggregate(r_lcm %in% 3 & r_peat == 0 & !r_bmv & is.na(r_saltmarsh), fact = 20, fun = sum) * 0.0625
  r20$grassop_grass <- aggregate(r_lcm %in% 4 & r_peat == 0 & !r_bmv & is.na(r_saltmarsh), fact = 20, fun = sum) * 0.0625
  
  # And also potential area of sng which competes with potential woodland/agroforestry
  r20$grassopwoodop_arable <- aggregate(r_lcm %in% 3 & r_woodop & !r_bmv & r_peat == 0 & is.na(r_saltmarsh), fact = 20, fun = sum) * 0.0625
  r20$grassopwoodop_grass <- aggregate(r_lcm %in% 4 & r_woodop & !r_bmv & r_peat == 0 & is.na(r_saltmarsh), fact = 20, fun = sum) * 0.0625
  
  # Calculate area of PAWS
  r20$paws <- aggregate(r_paws, fact = 20, fun = sum) * 0.0625
  
  # Calculate area of mineral soil 
  r20$mineral <- aggregate(r_soiltype == 1, fact = 20, fun = sum) * 0.0625
  
  # Native pinewood opportunities
  if(class(r_pinewood) == "RasterLayer"){
    r20$pinewood <- aggregate(r_pinewood == 2 & r_woodop, fact = 20, fun = sum) * 0.0625
  } else{
    r20$pinewood <- 0
  }
  
  
  # Get local area, aggregate 20-fold; 0.0625 ha becomes 25 ha
  # Broadleaved woodland
  r20$bw_local <- aggregate(r_lcm == 1, fact = 20, fun = mean)
  # Coniferous woodland
  r20$cw_local <- aggregate(r_lcm == 2, fact = 20, fun = mean)
  # Any woodland
  r20$wd_local <- aggregate(r_lcm %in% c(1, 2), fact = 20, fun = mean)
  # Semi-natural grass
  r20$gr_local <- aggregate(r_lcm %in% c(5, 6, 7), fact = 20, fun = mean)
  
  
  # Calculate distance to nearest 
  r20$bw_dist <- dist_to_focal(r20$bw_local[[1]])
  r20$cw_dist <- dist_to_focal(r20$cw_local[[1]])
  r20$wd_dist <- dist_to_focal(r20$wd_local[[1]])
  r20$gr_dist <- dist_to_focal(r20$gr_local[[1]])
  
  
  print("Aggregating rasters 12-fold")
  # Calculate potential area of bioenergy crops. Aggregate 12-fold to 9ha
  r12 <- aggregate(r_alc, fact = 12, fun = modal)
  names(r12) <- "alc"
  r12$cell <- 1:ncell(r12)
  r12$bioenergy_crop <- aggregate((r_lcm %in% 3) & r_woodop, fact = 12, fun = sum) * 0.0625 # arable
  r12$bioenergy_srcf <- aggregate((r_lcm %in% 4) & r_woodop, fact = 12, fun = sum) * 0.0625 # improved grassland
  
  print("Constructing attribute table")
  # Construct attribute ranking table - 25ha
  set.seed(1)
  df20_rank <- as.tbl(as.data.frame(r20)) %>%
    filter(!is.na(alc)) %>% 
    mutate(woodop = ifelse(is.na(woodop), 0, woodop),
           paws = ifelse(is.na(paws), 0, paws),
           pinewood = ifelse(is.na(pinewood), 0, pinewood)) %>% 
    # Broadleaved woodland ranking 
    sample_frac(1) %>% 
    arrange(pinewood, -mineral, -alc, -bw_local, bw_dist) %>% # Avoid pinewood first, then use up mineral soils, then low ALC, then large woodland cover, then low woodland distance
    mutate(bw_rank = 1:nrow(.)) %>% 
    # Coniferous woodland ranking 
    sample_frac(1) %>% 
    arrange(-pinewood, -mineral, -alc, -cw_local, cw_dist) %>% # Fill pinewood first, then use up mineral soils, then low ALC, then large woodland cover, then low woodland distance
    mutate(cw_rank = 1:nrow(.)) %>% 
    # Agroforestry ranking
    sample_frac(1) %>% 
    arrange(pinewood, wd_dist) %>% # Avoid pinewood first, then low woodland distance
    mutate(agfor_rank = 1:nrow(.)) %>%
    # Seminatural grassland ranking
    sample_frac(1) %>% 
    arrange(pinewood, mineral, gr_dist, -wd_dist) %>% # Avoid pinewood first, then use up non-mineral soils, then low grassland distance, then high woodland distance
    mutate(grass_rank = 1:nrow(.)) %>%  
    # PAWS
    sample_frac(1) %>% 
    arrange(-pinewood, -bw_local, bw_dist) %>% # Fill pinewood first, then large woodland cover, then low woodland distance
    mutate(paws_rank = 1:nrow(.))
  
  # Construct attribute ranking table - 9ha
  set.seed(1)
  df12_rank <- as.tbl(as.data.frame(r12)) %>%
    filter(!is.na(alc)) %>%
    # Bioenergy
    sample_frac(1) %>% # Random
    mutate(bioenergy_rank = 1:nrow(.)) %>%
    arrange(cell)
  
  print("Projecting rasters back to 25-m")
  # Project rank to 25 ha
  r20_rank <- subsDT(stack(replicate(7, r20$cell)), select(df20_rank, cell, bw_rank, cw_rank, agfor_rank, grass_rank, paws_rank), by = 1, which = c(2, 3, 4, 4, 4, 5, 6))
  
  # Project rank to 9 ha
  r12_rank <- subsDT(stack(replicate(2, r12$cell)), select(df12_rank, cell, bioenergy_rank), by = 1, which = c(2, 2))
  
  # Project rank back to 0.0625 ha
  r_rank <- stack(projectRaster(r20_rank, r_lcm, method = 'ngb'),
                  projectRaster(r12_rank, r_lcm, method = 'ngb'))
  names(r_rank) <- c("bw_rank", "cw_rank", "silvop_rank", "silvoa_rank", "woodpa_sng_rank", "grass_grass_rank", "paws_rank", "biocrop_rank", "biosrf_rank")
  
  # Duplicate woodpa and SNG (different starting land covers)
  r_rank$woodpa_img_rank <- r_rank$woodpa_sng_rank
  r_rank$grass_arable_rank <- r_rank$grass_grass_rank
  
  print("Replacing 25-m values")
  v_lcm <- getValues(r_lcm)
  v_lcm_na <- is.na(v_lcm)
  r_rank$bw_rank[is.na(v_lcm_na)] <- NA
  r_rank$cw_rank[is.na(v_lcm_na)] <- NA
  r_rank$silvop_rank[is.na(v_lcm_na)] <- NA
  r_rank$silvoa_rank[is.na(v_lcm_na)] <- NA
  r_rank$woodpa_sng_rank[is.na(v_lcm_na)] <- NA
  r_rank$woodpa_img_rank[is.na(v_lcm_na)] <- NA
  r_rank$grass_grass_rank[is.na(v_lcm_na)] <- NA
  r_rank$grass_arable_rank[is.na(v_lcm_na)] <- NA
  r_rank$paws_rank[is.na(v_lcm_na)] <- NA
  r_rank$biocrop_rank[is.na(v_lcm_na)] <- NA
  r_rank$biosrf_rank[is.na(v_lcm_na)] <- NA
  rm(v_lcm_na); gc()
  
  # If incompatible land-use, rank <- NA
  r_rank$silvop_rank[!v_lcm %in% 4] <- NA
  r_rank$silvoa_rank[!v_lcm %in% 3] <- NA
  r_rank$woodpa_sng_rank[!v_lcm %in% c(5, 6, 7, 9, 10)] <- NA
  r_rank$woodpa_img_rank[!v_lcm %in% 4] <- NA
  r_rank$grass_grass_rank[!v_lcm %in% 4] <- NA
  r_rank$grass_arable_rank[!v_lcm %in% 3] <- NA
  r_rank$paws_rank[v_lcm != 2] <- NA
  r_rank$biocrop_rank[!v_lcm %in% 3] <- NA
  r_rank$biosrf_rank[!v_lcm %in% 4] <- NA
  rm(v_lcm); gc()
  
  # If no new woodland opportunity (or, for woodland, BMV), rank <- NA 
  v_woodop <- getValues(r_woodop)
  v_bmv <- getValues(r_bmv)
  r_rank$bw_rank[v_woodop != 1 | is.na(v_woodop) | v_bmv == 1] <- NA
  r_rank$cw_rank[v_woodop != 1 | is.na(v_woodop) | v_bmv == 1] <- NA
  r_rank$silvop_rank[v_woodop != 1 | is.na(v_woodop)] <- NA
  r_rank$silvoa_rank[v_woodop != 1 | is.na(v_woodop)] <- NA
  r_rank$woodpa_sng_rank[v_woodop != 1 | is.na(v_woodop) | v_bmv == 1] <- NA
  r_rank$woodpa_img_rank[v_woodop != 1 | is.na(v_woodop) | v_bmv == 1] <- NA
  r_rank$grass_grass_rank[v_bmv == 1] <- NA
  r_rank$grass_arable_rank[v_bmv == 1] <- NA
  r_rank$biocrop_rank[v_woodop != 1 | is.na(v_woodop)] <- NA
  r_rank$biosrf_rank[v_woodop != 1 | is.na(v_woodop)] <- NA
  rm(v_woodop); gc()
  
  # If peat or saltmarsh, grass_rank <- NA
  v_peat <- getValues(r_peat)
  r_rank$grass_grass_rank[v_peat == 1] <- NA
  r_rank$grass_arable_rank[v_peat == 1] <- NA
  r_rank$biocrop_rank[v_peat == 1] <- NA
  r_rank$biosrf_rank[v_peat == 1] <- NA
  rm(v_peat); gc()
  
  v_saltmarsh <- getValues(r_saltmarsh)
  r_rank$grass_grass_rank[!is.na(v_saltmarsh)] <- NA
  r_rank$grass_arable_rank[!is.na(v_saltmarsh)] <- NA
  r_rank$biocrop_rank[!is.na(v_saltmarsh)] <- NA
  r_rank$biosrf_rank[!is.na(v_saltmarsh)] <- NA
  rm(v_saltmarsh); gc()
  
  # If not PAWS, PAWS <- NA
  r_rank$paws_rank[is.na(r_paws)] <- NA
  
  # Return
  print("Done")
  return(list(df_otherrank = df20_rank,
              df_bioenergyrank = df12_rank,
              r_otherrank = r_rank))
}


## DEFAULT RANKING -----------------------
# If organomineral soil, no woodop
r_woodop_nir2 <- r_woodop_nir; r_woodop_nir2[r_soiltype_nir == 2] <- NA
r_woodop_wal2 <- r_woodop_wal; r_woodop_wal2[r_soiltype_wal == 2] <- NA
r_woodop_sct2 <- r_woodop_sct; r_woodop_sct2[r_soiltype_sct == 2] <- NA
r_woodop_eng2 <- r_woodop_eng; r_woodop_eng2[r_soiltype_eng == 2] <- NA

# Run 
otherrank_nir <- otherrank_fun(r_lcm_nir, r_alc_nir, r_woodop_nir2, r_peat_nir, r_saltmarsh_nir, r_paws_nir, r_soiltype_nir, r_pinewood = NA, country = "nir")
gc()
df_otherrank_nir <- otherrank_nir$df_otherrank
df_bioenergyrank_nir <- otherrank_nir$df_bioenergyrank
save(df_otherrank_nir, df_bioenergyrank_nir, file = "rdata/intermediate/df_otherrank_nir_2022.RData")
writeRaster(otherrank_nir$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_nir_2022.tif", overwrite = TRUE)

otherrank_wal <- otherrank_fun(r_lcm_wal, r_alc_wal, r_woodop_wal2, r_peat_wal, r_saltmarsh_wal, r_paws_wal, r_soiltype_wal, r_pinewood = NA, country = "wal")
gc()
df_otherrank_wal <- otherrank_wal$df_otherrank
df_bioenergyrank_wal <- otherrank_wal$df_bioenergyrank
save(df_otherrank_wal, df_bioenergyrank_wal, file = "rdata/intermediate/df_otherrank_wal_2022.RData")
writeRaster(otherrank_wal$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_wal_2022.tif", overwrite = TRUE)

otherrank_sct <- otherrank_fun(r_lcm_sct, r_alc_sct, r_woodop_sct2, r_peat_sct, r_saltmarsh_sct, r_paws_sct, r_soiltype_sct, r_pinewood = r_pinewood_sct, country = "sct")
gc()
df_otherrank_sct <- otherrank_sct$df_otherrank
df_bioenergyrank_sct <- otherrank_sct$df_bioenergyrank
save(df_otherrank_sct, df_bioenergyrank_sct, file = "rdata/intermediate/df_otherrank_sct_2022.RData")
writeRaster(otherrank_sct$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_sct_2022.tif", overwrite = TRUE)

otherrank_eng <- otherrank_fun(r_lcm_eng, r_alc_eng, r_woodop_eng2, r_peat_eng, r_saltmarsh_eng, r_paws_eng, r_soiltype_eng, r_pinewood = NA, country = "eng")
gc()
df_otherrank_eng <- otherrank_eng$df_otherrank
df_bioenergyrank_eng <- otherrank_eng$df_bioenergyrank
save(df_otherrank_eng, df_bioenergyrank_eng, file = "rdata/intermediate/df_otherrank_eng_2022.RData")
writeRaster(otherrank_eng$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_eng_2022.tif", overwrite = TRUE)



## REPEAT WITH WADER MASK (NO SOIL MASK) ------------------------------
# Wader rasters
r_waders_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_nir.tif")
r_waders_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_eng.tif")
r_waders_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_wal.tif")
r_waders_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_sct.tif")

# If waders, no woodop
r_woodop_waders_nir <- r_woodop_nir
r_woodop_waders_eng <- r_woodop_eng
r_woodop_waders_wal <- r_woodop_wal
r_woodop_waders_sct <- r_woodop_sct
r_woodop_waders_nir[r_waders_nir == 1] <- NA
r_woodop_waders_eng[r_waders_eng == 1] <- NA
r_woodop_waders_wal[r_waders_wal == 1] <- NA
r_woodop_waders_sct[r_waders_sct == 1] <- NA

# Run 
otherrank_waders_nir <- otherrank_fun(r_lcm_nir, r_alc_nir, r_woodop_waders_nir, r_peat_nir, r_saltmarsh_nir, r_paws_nir, r_soiltype_nir, r_pinewood = NA, country = "nir")
gc()
df_otherrank_waders_nir <- otherrank_waders_nir$df_otherrank
df_bioenergyrank_waders_nir <- otherrank_waders_nir$df_bioenergyrank
save(df_otherrank_waders_nir, df_bioenergyrank_waders_nir, file = "rdata/intermediate/df_otherrank_waders_nir_2022.RData")
writeRaster(otherrank_waders_nir$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_waders_nir_2022.tif", overwrite = TRUE)

otherrank_waders_wal <- otherrank_fun(r_lcm_wal, r_alc_wal, r_woodop_waders_wal, r_peat_wal, r_saltmarsh_wal, r_paws_wal, r_soiltype_wal, r_pinewood = NA, country = "wal")
gc()
df_otherrank_waders_wal <- otherrank_waders_wal$df_otherrank
df_bioenergyrank_waders_wal <- otherrank_waders_wal$df_bioenergyrank
save(df_otherrank_waders_wal, df_bioenergyrank_waders_wal, file = "rdata/intermediate/df_otherrank_waders_wal_2022.RData")
writeRaster(otherrank_waders_wal$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_waders_wal_2022.tif", overwrite = TRUE)

otherrank_waders_sct <- otherrank_fun(r_lcm_sct, r_alc_sct, r_woodop_waders_sct, r_peat_sct, r_saltmarsh_sct, r_paws_sct, r_soiltype_sct, r_pinewood = r_pinewood_sct, country = "sct")
gc()
df_otherrank_waders_sct <- otherrank_waders_sct$df_otherrank
df_bioenergyrank_waders_sct <- otherrank_waders_sct$df_bioenergyrank
save(df_otherrank_waders_sct, df_bioenergyrank_waders_sct, file = "rdata/intermediate/df_otherrank_waders_sct_2022.RData")
writeRaster(otherrank_waders_sct$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_waders_sct_2022.tif", overwrite = TRUE)


otherrank_waders_eng <- otherrank_fun(r_lcm_eng, r_alc_eng, r_woodop_waders_eng, r_peat_eng, r_saltmarsh_eng, r_paws_eng, r_soiltype_eng, r_pinewood = NA, country = "eng")
gc()
df_otherrank_waders_eng <- otherrank_waders_eng$df_otherrank
df_bioenergyrank_waders_eng <- otherrank_waders_eng$df_bioenergyrank
save(df_otherrank_waders_eng, df_bioenergyrank_waders_eng, file = "rdata/intermediate/df_otherrank_waders_eng_2022.RData")
writeRaster(otherrank_waders_eng$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_waders_eng_2022.tif", overwrite = TRUE)

rm(otherrank_waders_nir, df_otherrank_waders_nir, df_bioenergyrank_waders_nir,
   otherrank_waders_wal, df_otherrank_waders_wal, df_bioenergyrank_waders_wal,
   otherrank_waders_sct, df_otherrank_waders_sct, df_bioenergyrank_waders_sct,
   otherrank_waders_eng, df_otherrank_waders_eng, df_bioenergyrank_waders_eng)


## REPEAT WITH WADER *AND* SOIL MASK ------------------------------
# Wader rasters
r_waders_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_nir.tif")
r_waders_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_eng.tif")
r_waders_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_wal.tif")
r_waders_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_waders_sct.tif")

# If organomineral soil, no woodop
r_woodop_waders_nir <- r_woodop_nir; r_woodop_waders_nir[r_soiltype_nir == 2] <- NA
r_woodop_waders_eng <- r_woodop_eng; r_woodop_waders_eng[r_soiltype_eng == 2] <- NA
r_woodop_waders_wal <- r_woodop_wal; r_woodop_waders_wal[r_soiltype_wal == 2] <- NA
r_woodop_waders_sct <- r_woodop_sct; r_woodop_waders_sct[r_soiltype_sct == 2] <- NA

# If waders, no woodop
r_woodop_waders_nir[r_waders_nir == 1] <- NA
r_woodop_waders_eng[r_waders_eng == 1] <- NA
r_woodop_waders_wal[r_waders_wal == 1] <- NA
r_woodop_waders_sct[r_waders_sct == 1] <- NA

# Run 
otherrank_wadersoil_nir <- otherrank_fun(r_lcm_nir, r_alc_nir, r_woodop_waders_nir, r_peat_nir, r_saltmarsh_nir, r_paws_nir, r_soiltype_nir, r_pinewood = NA, country = "nir")
gc()
df_otherrank_wadersoil_nir <- otherrank_wadersoil_nir$df_otherrank
df_bioenergyrank_wadersoil_nir <- otherrank_wadersoil_nir$df_bioenergyrank
save(df_otherrank_wadersoil_nir, df_bioenergyrank_wadersoil_nir, file = "rdata/intermediate/df_otherrank_wadersoil_nir_2022.RData")
writeRaster(otherrank_wadersoil_nir$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_wadersoil_nir_2022.tif", overwrite = TRUE)

otherrank_wadersoil_wal <- otherrank_fun(r_lcm_wal, r_alc_wal, r_woodop_waders_wal, r_peat_wal, r_saltmarsh_wal, r_paws_wal, r_soiltype_wal, r_pinewood = NA, country = "wal")
gc()
df_otherrank_wadersoil_wal <- otherrank_wadersoil_wal$df_otherrank
df_bioenergyrank_wadersoil_wal <- otherrank_wadersoil_wal$df_bioenergyrank
save(df_otherrank_wadersoil_wal, df_bioenergyrank_wadersoil_wal, file = "rdata/intermediate/df_otherrank_wadersoil_wal_2022.RData")
writeRaster(otherrank_wadersoil_wal$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_wadersoil_wal_2022.tif", overwrite = TRUE)

otherrank_wadersoil_sct <- otherrank_fun(r_lcm_sct, r_alc_sct, r_woodop_waders_sct, r_peat_sct, r_saltmarsh_sct, r_paws_sct, r_soiltype_sct, r_pinewood = r_pinewood_sct, country = "sct")
gc()
df_otherrank_wadersoil_sct <- otherrank_wadersoil_sct$df_otherrank
df_bioenergyrank_wadersoil_sct <- otherrank_wadersoil_sct$df_bioenergyrank
save(df_otherrank_wadersoil_sct, df_bioenergyrank_wadersoil_sct, file = "rdata/intermediate/df_otherrank_wadersoil_sct_2022.RData")
writeRaster(otherrank_wadersoil_sct$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_wadersoil_sct_2022.tif", overwrite = TRUE)

otherrank_wadersoil_eng <- otherrank_fun(r_lcm_eng, r_alc_eng, r_woodop_waders_eng, r_peat_eng, r_saltmarsh_eng, r_paws_eng, r_soiltype_eng, r_pinewood = NA, country = "eng")
gc()
df_otherrank_wadersoil_eng <- otherrank_wadersoil_eng$df_otherrank
df_bioenergyrank_wadersoil_eng <- otherrank_wadersoil_eng$df_bioenergyrank
save(df_otherrank_wadersoil_eng, df_bioenergyrank_wadersoil_eng, file = "rdata/intermediate/df_otherrank_wadersoil_eng_2022.RData")
writeRaster(otherrank_wadersoil_eng$r_otherrank, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_wadersoil_eng_2022.tif", overwrite = TRUE)

rm(otherrank_wadersoil_nir, df_otherrank_wadersoil_nir, df_bioenergyrank_wadersoil_nir,
   otherrank_wadersoil_wal, df_otherrank_wadersoil_wal, df_bioenergyrank_wadersoil_wal)
rm(otherrank_wadersoil_sct, df_otherrank_wadersoil_sct, df_bioenergyrank_wadersoil_sct)
rm(otherrank_wadersoil_eng, df_otherrank_wadersoil_eng, df_bioenergyrank_wadersoil_eng)


## CALCULATE COUNTRY PROPORTIONS --------------
# Load rasters
# Basic
r_otherrank_nir <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_nir_2022.tif")
r_otherrank_wal <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_wal_2022.tif")
r_otherrank_eng <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_eng_2022.tif")
r_otherrank_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_sct_2022.tif")

# Wader mask
r_otherrank_waders_nir <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_waders_nir_2022.tif")
r_otherrank_waders_wal <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_waders_wal_2022.tif")
r_otherrank_waders_eng <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_waders_eng_2022.tif")
r_otherrank_waders_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_waders_sct_2022.tif")

# Wader AND soil mask
r_otherrank_wadersoil_nir <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_wadersoil_nir_2022.tif")
r_otherrank_wadersoil_wal <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_wadersoil_wal_2022.tif")
r_otherrank_wadersoil_eng <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_wadersoil_eng_2022.tif")
r_otherrank_wadersoil_sct <- stack("C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_otherrank_wadersoil_sct_2022.tif")

# # ALC
# load("rdata/raster_lookup_alc.RData")
# 
# d1 <- crosstabDT(stack(r_otherrank_organomin_nir[[1]] > 0, r_alc_nir)); d1/sum(d1)
# d2 <- crosstabDT(stack(r_otherrank_organomin_eng[[1]] > 0, r_alc_eng)); d2/sum(d2)
# d3 <- crosstabDT(stack(r_otherrank_organomin_wal[[1]] > 0, r_alc_wal)); d3/sum(d3)
# d4 <- crosstabDT(stack(r_otherrank_organomin_sct[[1]] > 0, r_alc_sct)); d4/sum(d4)



# Calculate distribution of opportunity area among countries
country_props <- list(wood_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                          n = c(freqDT(r_otherrank_nir[[1]] > 0)$freq[2],
                                                freqDT(r_otherrank_eng[[1]] > 0)$freq[2],
                                                freqDT(r_otherrank_wal[[1]] > 0)$freq[2],
                                                freqDT(r_otherrank_sct[[1]] > 0)$freq[2]),
                                          prop = n / sum(n)),
                      silvop_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                            n = c(freqDT(r_otherrank_nir[[3]] > 0)$freq[2],
                                                  freqDT(r_otherrank_eng[[3]] > 0)$freq[2],
                                                  freqDT(r_otherrank_wal[[3]] > 0)$freq[2],
                                                  freqDT(r_otherrank_sct[[3]] > 0)$freq[2]),
                                            prop = n / sum(n)),
                      silvoa_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                            n = c(freqDT(r_otherrank_nir[[4]] > 0)$freq[2],
                                                  freqDT(r_otherrank_eng[[4]] > 0)$freq[2],
                                                  freqDT(r_otherrank_wal[[4]] > 0)$freq[2],
                                                  freqDT(r_otherrank_sct[[4]] > 0)$freq[2]),
                                            prop = n / sum(n)),
                      woodpa_sng_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                n = c(freqDT(r_otherrank_nir[[5]] > 0)$freq[2],
                                                      freqDT(r_otherrank_eng[[5]] > 0)$freq[2],
                                                      freqDT(r_otherrank_wal[[5]] > 0)$freq[2],
                                                      freqDT(r_otherrank_sct[[5]] > 0)$freq[2]),
                                                prop = n / sum(n)),
                      woodpa_img_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                n = c(freqDT(r_otherrank_nir[[10]] > 0)$freq[2],
                                                      freqDT(r_otherrank_eng[[10]] > 0)$freq[2],
                                                      freqDT(r_otherrank_wal[[10]] > 0)$freq[2],
                                                      freqDT(r_otherrank_sct[[10]] > 0)$freq[2]),
                                                prop = n / sum(n)),
                      grass_grass_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                 n = c(freqDT(r_otherrank_nir[[6]] > 0)$freq[2],
                                                       freqDT(r_otherrank_eng[[6]] > 0)$freq[2],
                                                       freqDT(r_otherrank_wal[[6]] > 0)$freq[2],
                                                       freqDT(r_otherrank_sct[[6]] > 0)$freq[2]),
                                                 prop = n / sum(n)),
                      grass_arable_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                  n = c(freqDT(r_otherrank_nir[[11]] > 0)$freq[2],
                                                        freqDT(r_otherrank_eng[[11]] > 0)$freq[2],
                                                        freqDT(r_otherrank_wal[[11]] > 0)$freq[2],
                                                        freqDT(r_otherrank_sct[[11]] > 0)$freq[2]),
                                                  prop = n / sum(n)),
                      biocrop_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                             n = c(freqDT(r_otherrank_nir[[8]] > 0)$freq[2],
                                                   freqDT(r_otherrank_eng[[8]] > 0)$freq[2],
                                                   freqDT(r_otherrank_wal[[8]] > 0)$freq[2],
                                                   freqDT(r_otherrank_sct[[8]] > 0)$freq[2]),
                                             prop = n / sum(n)),
                      biosrf_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                            n = c(freqDT(r_otherrank_nir[[9]] > 0)$freq[2],
                                                  freqDT(r_otherrank_eng[[9]] > 0)$freq[2],
                                                  freqDT(r_otherrank_wal[[9]] > 0)$freq[2],
                                                  freqDT(r_otherrank_sct[[9]] > 0)$freq[2]),
                                            prop = n / sum(n)))


country_props_waders <- list(wood_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                 n = c(freqDT(r_otherrank_waders_nir[[1]] > 0)$freq[2],
                                                       freqDT(r_otherrank_waders_eng[[1]] > 0)$freq[2],
                                                       freqDT(r_otherrank_waders_wal[[1]] > 0)$freq[2],
                                                       freqDT(r_otherrank_waders_sct[[1]] > 0)$freq[2]),
                                                 prop = n / sum(n)),
                             silvop_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                   n = c(freqDT(r_otherrank_waders_nir[[3]] > 0)$freq[2],
                                                         freqDT(r_otherrank_waders_eng[[3]] > 0)$freq[2],
                                                         freqDT(r_otherrank_waders_wal[[3]] > 0)$freq[2],
                                                         freqDT(r_otherrank_waders_sct[[3]] > 0)$freq[2]),
                                                   prop = n / sum(n)),
                             silvoa_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                   n = c(freqDT(r_otherrank_waders_nir[[4]] > 0)$freq[2],
                                                         freqDT(r_otherrank_waders_eng[[4]] > 0)$freq[2],
                                                         freqDT(r_otherrank_waders_wal[[4]] > 0)$freq[2],
                                                         freqDT(r_otherrank_waders_sct[[4]] > 0)$freq[2]),
                                                   prop = n / sum(n)),
                             woodpa_sng_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                       n = c(freqDT(r_otherrank_waders_nir[[5]] > 0)$freq[2],
                                                             freqDT(r_otherrank_waders_eng[[5]] > 0)$freq[2],
                                                             freqDT(r_otherrank_waders_wal[[5]] > 0)$freq[2],
                                                             freqDT(r_otherrank_waders_sct[[5]] > 0)$freq[2]),
                                                       prop = n / sum(n)),
                             woodpa_img_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                       n = c(freqDT(r_otherrank_waders_nir[[10]] > 0)$freq[2],
                                                             freqDT(r_otherrank_waders_eng[[10]] > 0)$freq[2],
                                                             freqDT(r_otherrank_waders_wal[[10]] > 0)$freq[2],
                                                             freqDT(r_otherrank_waders_sct[[10]] > 0)$freq[2]),
                                                       prop = n / sum(n)),
                             grass_grass_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                        n = c(freqDT(r_otherrank_waders_nir[[6]] > 0)$freq[2],
                                                              freqDT(r_otherrank_waders_eng[[6]] > 0)$freq[2],
                                                              freqDT(r_otherrank_waders_wal[[6]] > 0)$freq[2],
                                                              freqDT(r_otherrank_waders_sct[[6]] > 0)$freq[2]),
                                                        prop = n / sum(n)),
                             grass_arable_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                         n = c(freqDT(r_otherrank_waders_nir[[11]] > 0)$freq[2],
                                                               freqDT(r_otherrank_waders_eng[[11]] > 0)$freq[2],
                                                               freqDT(r_otherrank_waders_wal[[11]] > 0)$freq[2],
                                                               freqDT(r_otherrank_waders_sct[[11]] > 0)$freq[2]),
                                                         prop = n / sum(n)),
                             biocrop_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                    n = c(freqDT(r_otherrank_waders_nir[[8]] > 0)$freq[2],
                                                          freqDT(r_otherrank_waders_eng[[8]] > 0)$freq[2],
                                                          freqDT(r_otherrank_waders_wal[[8]] > 0)$freq[2],
                                                          freqDT(r_otherrank_waders_sct[[8]] > 0)$freq[2]),
                                                    prop = n / sum(n)),
                             biosrf_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                   n = c(freqDT(r_otherrank_waders_nir[[9]] > 0)$freq[2],
                                                         freqDT(r_otherrank_waders_eng[[9]] > 0)$freq[2],
                                                         freqDT(r_otherrank_waders_wal[[9]] > 0)$freq[2],
                                                         freqDT(r_otherrank_waders_sct[[9]] > 0)$freq[2]),
                                                   prop = n / sum(n)))


country_props_wadersoil <- list(wood_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                    n = c(freqDT(r_otherrank_wadersoil_nir[[1]] > 0)$freq[2],
                                                          freqDT(r_otherrank_wadersoil_eng[[1]] > 0)$freq[2],
                                                          freqDT(r_otherrank_wadersoil_wal[[1]] > 0)$freq[2],
                                                          freqDT(r_otherrank_wadersoil_sct[[1]] > 0)$freq[2]),
                                                    prop = n / sum(n)),
                                silvop_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                      n = c(freqDT(r_otherrank_wadersoil_nir[[3]] > 0)$freq[2],
                                                            freqDT(r_otherrank_wadersoil_eng[[3]] > 0)$freq[2],
                                                            freqDT(r_otherrank_wadersoil_wal[[3]] > 0)$freq[2],
                                                            freqDT(r_otherrank_wadersoil_sct[[3]] > 0)$freq[2]),
                                                      prop = n / sum(n)),
                                silvoa_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                      n = c(freqDT(r_otherrank_wadersoil_nir[[4]] > 0)$freq[2],
                                                            freqDT(r_otherrank_wadersoil_eng[[4]] > 0)$freq[2],
                                                            freqDT(r_otherrank_wadersoil_wal[[4]] > 0)$freq[2],
                                                            freqDT(r_otherrank_wadersoil_sct[[4]] > 0)$freq[2]),
                                                      prop = n / sum(n)),
                                woodpa_sng_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                          n = c(freqDT(r_otherrank_wadersoil_nir[[5]] > 0)$freq[2],
                                                                freqDT(r_otherrank_wadersoil_eng[[5]] > 0)$freq[2],
                                                                freqDT(r_otherrank_wadersoil_wal[[5]] > 0)$freq[2],
                                                                freqDT(r_otherrank_wadersoil_sct[[5]] > 0)$freq[2]),
                                                          prop = n / sum(n)),
                                woodpa_img_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                          n = c(freqDT(r_otherrank_wadersoil_nir[[10]] > 0)$freq[2],
                                                                freqDT(r_otherrank_wadersoil_eng[[10]] > 0)$freq[2],
                                                                freqDT(r_otherrank_wadersoil_wal[[10]] > 0)$freq[2],
                                                                freqDT(r_otherrank_wadersoil_sct[[10]] > 0)$freq[2]),
                                                          prop = n / sum(n)),
                                grass_grass_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                           n = c(freqDT(r_otherrank_wadersoil_nir[[6]] > 0)$freq[2],
                                                                 freqDT(r_otherrank_wadersoil_eng[[6]] > 0)$freq[2],
                                                                 freqDT(r_otherrank_wadersoil_wal[[6]] > 0)$freq[2],
                                                                 freqDT(r_otherrank_wadersoil_sct[[6]] > 0)$freq[2]),
                                                           prop = n / sum(n)),
                                grass_arable_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                            n = c(freqDT(r_otherrank_wadersoil_nir[[11]] > 0)$freq[2],
                                                                  freqDT(r_otherrank_wadersoil_eng[[11]] > 0)$freq[2],
                                                                  freqDT(r_otherrank_wadersoil_wal[[11]] > 0)$freq[2],
                                                                  freqDT(r_otherrank_wadersoil_sct[[11]] > 0)$freq[2]),
                                                            prop = n / sum(n)),
                                biocrop_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                       n = c(freqDT(r_otherrank_wadersoil_nir[[8]] > 0)$freq[2],
                                                             freqDT(r_otherrank_wadersoil_eng[[8]] > 0)$freq[2],
                                                             freqDT(r_otherrank_wadersoil_wal[[8]] > 0)$freq[2],
                                                             freqDT(r_otherrank_wadersoil_sct[[8]] > 0)$freq[2]),
                                                       prop = n / sum(n)),
                                biosrf_props = tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                                                      n = c(freqDT(r_otherrank_wadersoil_nir[[9]] > 0)$freq[2],
                                                            freqDT(r_otherrank_wadersoil_eng[[9]] > 0)$freq[2],
                                                            freqDT(r_otherrank_wadersoil_wal[[9]] > 0)$freq[2],
                                                            freqDT(r_otherrank_wadersoil_sct[[9]] > 0)$freq[2]),
                                                      prop = n / sum(n)))

total_props <- tibble(country = c("Northern Ireland", "England", "Wales", "Scotland"),
                      n = c(freqDT(!is.na(r_alc_nir))$freq[2],
                            freqDT(!is.na(r_alc_eng))$freq[2],
                            freqDT(!is.na(r_alc_wal))$freq[2],
                            freqDT(!is.na(r_alc_sct))$freq[2]),
                      prop = n / sum(n))


country_props$total_props <- total_props
country_props_waders$total_props <- total_props
country_props_wadersoil$total_props <- total_props

save(country_props, country_props_waders, country_props_wadersoil, file = "rdata/scenarios/country_props_2022.RData")




