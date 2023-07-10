source("setup.R")


## BUILD 25-m HEDGE RASTERS - GB ----------------------------
# Setup RQGIS
# devtools::install_github("jannes-m/RQGIS3")
install_load("RQGIS3")
set_env("C:/Program Files/QGIS 3.6")
# Command Line: set GDAL_DATA=C:/Program Files/QGIS 3.6/share/gdal

# Read hedge vector 
sf_hedges_gb <- st_read("hedges/GB_WLF_V1_0.gdb")
sf_hedges_gb <- st_transform(sf_hedges_gb, as.character(bng))

# Read LCM rasters
r_lcm_eng <- raster("rasters/r_lcm_new_eng.tif")
r_lcm_sct <- raster("rasters/r_lcm_new_sct.tif")
r_lcm_wal <- raster("rasters/r_lcm_new_wal.tif")

# Rasterize
# get_args_man("gdal:rasterize"); get_usage("gdal:rasterize")
r_hedges_wal <- run_qgis(INPUT = sf_hedges_gb, EXTENT = r_lcm_wal,
                         alg = "gdal:rasterize", UNITS = 1, WIDTH = 25, HEIGHT = 25, BURN = 1, OUTPUT = file.path(tempdir(), "temp_out.tif"),
                         load_output = TRUE)
r_hedges_wal[is.na(r_lcm_wal)] <- NA
r_hedges_wal[(!is.na(r_lcm_wal)) & is.na(r_hedges_wal)] <- 0

r_hedges_eng <- run_qgis(INPUT = sf_hedges_gb, EXTENT = r_lcm_eng,
                         alg = "gdal:rasterize", UNITS = 1, WIDTH = 25, HEIGHT = 25, BURN = 1, OUTPUT = file.path(tempdir(), "temp_out.tif"),
                         load_output = TRUE)
r_hedges_eng[is.na(r_lcm_eng)] <- NA
r_hedges_eng[(!is.na(r_lcm_eng)) & is.na(r_hedges_eng)] <- 0

r_hedges_sct <- run_qgis(INPUT = sf_hedges_gb, EXTENT = r_lcm_sct,
                         alg = "gdal:rasterize", UNITS = 1, WIDTH = 25, HEIGHT = 25, BURN = 1, OUTPUT = file.path(tempdir(), "temp_out.tif"),
                         load_output = TRUE)
r_hedges_sct[is.na(r_lcm_sct)] <- NA
r_hedges_sct[(!is.na(r_lcm_sct)) & is.na(r_hedges_sct)] <- 0

# Save
writeRaster(r_hedges_wal, "rasters/r_hedges_wal.tif", overwrite = TRUE)
writeRaster(r_hedges_eng, "rasters/r_hedges_eng.tif", overwrite = TRUE)
writeRaster(r_hedges_sct, "rasters/r_hedges_sct.tif", overwrite = TRUE)



## BUILD 25-m HEDGE RASTERS - NI ----------------------------
# Read raw hedge raster
r_hedges_nir <- raster("hedges/swf_2015_005m_GB023_3035_v012/Data/swf_2015_005m_GB023_3035_v012.tif")

# Read LCM rasters
r_lcm_nir <- raster("rasters/r_lcm_new_nir.tif")

# Crop raster
r_hedges_nir <- crop(r_hedges_nir, projectRaster(r_lcm_nir, crs = crs(r_hedges_nir)))

# If 3 (small wood), ignore
r_hedges_nir[r_hedges_nir == 3] <- 0

# Aggregate (to 25m) and reproject
r_hedges_nir <- aggregate(r_hedges_nir, 5, modal)
r_hedges_nir <- projectRaster(r_hedges_nir, r_lcm_nir, method = "ngb")

# Tidy
r_hedges_nir[is.na(r_lcm_nir)] <- NA
r_hedges_nir[r_hedges_nir == 255] <- 0
r_hedges_nir[r_hedges_nir == 254] <- -1 # Missing data

# Save
writeRaster(r_hedges_nir, "rasters/r_hedges_nir.tif", overwrite = TRUE)




## PROCESS HEDGE RASTERS ----------------------------
# Read hedge rasters
r_hedges_wal <- raster("rasters/r_hedges_wal.tif")
r_hedges_eng <- raster("rasters/r_hedges_eng.tif")
r_hedges_sct <- raster("rasters/r_hedges_sct.tif")
r_hedges_nir <- raster("rasters/r_hedges_nir.tif")

# Read LCM rasters
r_lcm_nir <- raster("rasters/r_lcm_new_nir.tif")
r_lcm_eng <- raster("rasters/r_lcm_new_eng.tif")
r_lcm_sct <- raster("rasters/r_lcm_new_sct.tif")
r_lcm_wal <- raster("rasters/r_lcm_new_wal.tif")

# Identify a.h / i.g 
r_hedges_wal <- stack(r_hedges_wal, r_hedges_wal, r_hedges_wal)
r_hedges_eng <- stack(r_hedges_eng, r_hedges_eng, r_hedges_eng)
r_hedges_sct <- stack(r_hedges_sct, r_hedges_sct, r_hedges_sct)
r_hedges_nir <- stack(r_hedges_nir, r_hedges_nir, r_hedges_nir)

r_hedges_wal[[2]][r_lcm_wal != 3] <- 0
r_hedges_wal[[3]][r_lcm_wal != 4] <- 0
r_hedges_eng[[2]][r_lcm_eng != 3] <- 0
r_hedges_eng[[3]][r_lcm_eng != 4] <- 0
r_hedges_sct[[2]][r_lcm_sct != 3] <- 0
r_hedges_sct[[3]][r_lcm_sct != 4] <- 0
r_hedges_nir[[2]][r_lcm_nir != 3] <- 0
r_hedges_nir[[3]][r_lcm_nir != 4] <- 0

# Calculate cover per 500-m (GB)
r_hedges_wal <- aggregate(r_hedges_wal, 20, sum)
r_hedges_eng <- aggregate(r_hedges_eng, 20, sum)
r_hedges_sct <- aggregate(r_hedges_sct, 20, sum)

# Identify Northern Ireland gaps
r_hedges_nir_gaps <- r_hedges_nir[[1]]
r_hedges_nir_gaps <- r_hedges_nir_gaps == -1
r_hedges_nir_gaps <- aggregate(r_hedges_nir_gaps, 20, sum)
r_hedges_nir_gaps <- r_hedges_nir_gaps != 0
r_hedges_nir <- aggregate(r_hedges_nir, 20, sum)
r_denom_nir <- stack(aggregate(!is.na(r_lcm_nir), 20, sum) / 400,
                     aggregate(r_lcm_nir %in% 3, 20, sum) / 400,
                     aggregate(r_lcm_nir %in% 4, 20, sum) / 400)
r_density_nir <- r_hedges_nir / r_denom_nir

# Function for replacing nearest NA value with nearest != -99 value
replace_NA_nearest_n0 <- function(x){
  na_cells <- which(is.na(x[]))
  n0_cells <- which(x[] != -99)
  
  nas_xy <- cbind(xyFromCell(x, na_cells), val = NA)
  n0s_xy <- cbind(xyFromCell(x, n0_cells), val = values(x)[n0_cells])
  
  n0s_tree <- createTree(coordinates(n0s_xy[, c(1, 2)]))
  nn_ids <- knnLookup(n0s_tree, newdat = coordinates(nas_xy[, c(1, 2)]), k = 5)
  
  x[na_cells] <- rowMeans(cbind(x[x[] != -99][nn_ids[,1]],
                                x[x[] != -99][nn_ids[,2]],
                                x[x[] != -99][nn_ids[,3]],
                                x[x[] != -99][nn_ids[,4]],
                                x[x[] != -99][nn_ids[,5]]))
  
  return(x)
}

# Fill Northern Ireland gaps (nearest density)
r_density_nir[is.na(r_density_nir)] <- -99
r_density_nir[r_hedges_nir_gaps == 1] <- NA
r_density_nir[[1]] <- replace_NA_nearest_n0(r_density_nir[[1]])
r_density_nir[[2]] <- replace_NA_nearest_n0(r_density_nir[[2]])
r_density_nir[[3]] <- replace_NA_nearest_n0(r_density_nir[[3]])
r_density_nir[r_density_nir == -99] <- NA

# Convert density back to length
r_hedges_nir <- r_density_nir * r_denom_nir
r_hedges_nir[r_denom_nir[[1]] != 0 & is.na(r_hedges_nir)] <- 0 



## RESCALE HEDGE COVER ----------------------
# Get estimated baseline hedge length
hedge_length <- read_csv("data/hedges/cs_hedge_length.csv", skip = 1) %>% select(-notes) %>%
  mutate(country = ifelse(country == "Northern Ireland", country, "GB")) %>% 
  group_by(country) %>% 
  summarise(length = sum(length)) %>% 
  ungroup() %>% 
  mutate(pixels = c(cellStats(r_hedges_eng[[1]], sum) + cellStats(r_hedges_sct[[1]], sum) + cellStats(r_hedges_wal[[1]], sum), # 13265839
                    cellStats(r_hedges_nir[[1]], sum))) %>% # 886347.6
  mutate(km_per_pix = length/pixels)


# Rescale hedge length
r_hedges_wal <- r_hedges_wal * hedge_length$km_per_pix[hedge_length$country == "GB"] # 0.0531 / 0.0625 ha = 0.85 km / ha
r_hedges_eng <- r_hedges_eng * hedge_length$km_per_pix[hedge_length$country == "GB"] # 0.0531 / 0.0625 ha = 0.85 km / ha
r_hedges_sct <- r_hedges_sct * hedge_length$km_per_pix[hedge_length$country == "GB"] # 0.0531 / 0.0625 ha = 0.85 km / ha
r_hedges_nir <- r_hedges_nir * hedge_length$km_per_pix[hedge_length$country == "Northern Ireland"] # 0.13 / 0.0625 ha = 2.1 km / ha


# Save
writeRaster(r_hedges_eng, "rasters/r_hedge_length_eng.tif", overwrite = TRUE)
writeRaster(r_hedges_sct, "rasters/r_hedge_length_sct.tif", overwrite = TRUE)
writeRaster(r_hedges_wal, "rasters/r_hedge_length_wal.tif", overwrite = TRUE)
writeRaster(r_hedges_nir, "rasters/r_hedge_length_nir.tif", overwrite = TRUE)
