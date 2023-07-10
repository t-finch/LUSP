source("setup.R")

## ATLAS DATA -----------------------------------
# SNipe, CUrlew, L.apwing, RedshanK and OysterCatcher
sn <- read_csv("data/bird_atlas/birdatlas_rf_predictions/00192SNBA20072011_002_0.csv")
cu <- read_csv("data/bird_atlas/birdatlas_rf_predictions/00203CUBA20072011_002_0.csv")
l. <- read_csv("data/bird_atlas/birdatlas_rf_predictions/00171L_BA20072011_002_0.csv")
rk <- read_csv("data/bird_atlas/birdatlas_rf_predictions/00206RKBA20072011_002_0.csv")
oc <- read_csv("data/bird_atlas/birdatlas_rf_predictions/00152OCBA20072011_002_0.csv")

# Combine
data <- bind_rows(sn %>% mutate(spp = "Snipe"),
                  cu %>% mutate(spp = "Curlew"),
                  l. %>% mutate(spp = "Lapwing"),
                  rk %>% mutate(spp = "Redshank"),
                  oc %>% mutate(spp = "Oystercatcher")) %>% 
  # Use median prediction
  select(-predmean) %>% 
  rename(abund = predmedian)



## TETRADS -----------------------------------
# GB - from G Drive
sf_tetrad_gb <- st_read("grids/Grids.gdb", layer = "BNG_Tetrads_2km")
# NI - from G Drive
sf_tetrad_ni <- st_read("grids/Grids.gdb", layer = "tblIre2km")

# Fix tetrad codes in NI
data <- data %>% 
  mutate(tetrad = ifelse(tetrad %in% sf_tetrad_gb$TETRADS, tetrad, substr(tetrad, 2, 5)))




# Woodland opportunity
r_woodop_nir <- raster("rasters/r_woodop_nir.tif")
r_woodop_eng <- raster("rasters/r_woodop_eng.tif")
r_woodop_wal <- raster("rasters/r_woodop_wal.tif")
r_woodop_sct <- raster("rasters/r_woodop_sct.tif")

# Organo-mineral soil
r_soiltype_nir <- raster("rasters/r_soiltype_nir.tif")
r_soiltype_eng <- raster("rasters/r_soiltype_eng.tif")
r_soiltype_wal <- raster("rasters/r_soiltype_wal.tif")
r_soiltype_sct <- raster("rasters/r_soiltype_sct.tif")

# LCM
r_lcm_nir <- raster("rasters/r_lcm_new_nir.tif")
r_lcm_eng <- raster("rasters/r_lcm_new_eng.tif")
r_lcm_wal <- raster("rasters/r_lcm_new_wal.tif")
r_lcm_sct <- raster("rasters/r_lcm_new_sct.tif")

# ALC
r_alc_nir <- raster("rasters/r_alc_nir.tif")
r_alc_eng <- raster("rasters/r_alc_eng.tif")
r_alc_wal <- raster("rasters/r_alc_wal.tif")
r_alc_sct <- raster("rasters/r_alc_sct.tif")
load("rdata/raster_lookup_alc.RData")

r_alc_nir <- r_alc_nir %in% filter(lookup_alc, country %in% "Northern Ireland" & alc %in% c("4A", "4B", "5"))$alc_layer
r_alc_eng <- r_alc_eng %in% filter(lookup_alc, country %in% "England" & alc %in% c("Grade 4", "Grade 5"))$alc_layer
r_alc_wal <- r_alc_wal %in% filter(lookup_alc, country %in% "Wales" & alc %in% c("4", "5"))$alc_layer
r_alc_sct <- r_alc_sct %in% filter(lookup_alc, country %in% "Scotland" & alc %in% c("4.1", "4.2", "5.1", "5.2", "5.3", "6.1", "6.2", "6.3", "7"))$alc_layer


## DERIVE WADER MASK -----------------------------------
high_strata_spp <- data %>% 
  group_by(spp) %>% 
  # Calculate cumsum
  arrange(abund) %>% 
  mutate(cumsum = cumsum(abund)) %>%
  # Identify bottom 10%
  mutate(bottom = cumsum < 0.1 * max(cumsum)) %>%
  # Identify strata - above and below median (excluding bottom 10%)
  mutate(stratum = ifelse(abund < quantile(abund[!bottom], 1/3), "Low", "High")) 


high_strata <- high_strata_spp %>%
  # Count number of species' classed as high per tetrad
  group_by(easting, northing, tetrad) %>%
  summarise(n = sum(stratum == "High")) %>% 
  mutate(stratum = "High")


# Filter out tetrads which are unimportant for all species 
high_strata <- high_strata %>% ungroup() %>% filter(n != 0) 




## RASTERZE WADER TETRADS ------------------------------
r_waders_nir <- stack(fasterize(filter(sf_tetrad_ni, tetrad %in% high_strata$tetrad), r_lcm_nir))
r_waders_eng <- stack(fasterize(filter(sf_tetrad_gb, TETRADS %in% high_strata$tetrad), r_lcm_eng))
r_waders_wal <- stack(fasterize(filter(sf_tetrad_gb, TETRADS %in% high_strata$tetrad), r_lcm_wal))
r_waders_sct <- stack(fasterize(filter(sf_tetrad_gb, TETRADS %in% high_strata$tetrad), r_lcm_sct))


# Get LCM val
v_lcm_nir <- r_lcm_nir[]
v_lcm_eng <- r_lcm_eng[]
v_lcm_wal <- r_lcm_wal[]
v_lcm_sct <- r_lcm_sct[]

# Exclude NA LCM
r_waders_nir[is.na(v_lcm_nir)] <- NA
r_waders_eng[is.na(v_lcm_eng)] <- NA
r_waders_wal[is.na(v_lcm_wal)] <- NA
r_waders_sct[is.na(v_lcm_sct)] <- NA

# Save
writeRaster(r_waders_nir, "rasters/r_waders_nir.tif", overwrite = TRUE)
writeRaster(r_waders_eng, "rasters/r_waders_eng.tif", overwrite = TRUE)
writeRaster(r_waders_wal, "rasters/r_waders_wal.tif", overwrite = TRUE)
writeRaster(r_waders_sct, "rasters/r_waders_sct.tif", overwrite = TRUE)
