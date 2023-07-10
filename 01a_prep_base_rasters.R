## INITS -------------------------------
source("setup.R")


## NUTS1 & LCM ----------------------------
# NUTS1 - updated version include coastal areas
sf_nuts1 <- st_read("data/nuts1/NUTS_Level_1_January_2018_Full_Extent_Boundaries_in_the_United_Kingdom.shp") %>% 
  select(nuts118nm) %>% 
  rename("nuts1" = nuts118nm) %>% 
  st_transform(as.character(bng)) 

# Add country column
sf_nuts1 <- sf_nuts1 %>%
  mutate(country = case_when(nuts1 %in% c("Scotland", "Wales", "Northern Ireland") ~ as.character(nuts1),
                             TRUE ~ "England"),
         country = as.factor(country)) 

# Create country polygons
sf_country <- sf_nuts1 %>% 
  group_by(country) %>% 
  summarise()

# Create NUTS1 lookup table
lookup_nuts1 <- tibble(nuts1_layer = 1:n_distinct(sf_nuts1$nuts1),
                       nuts1 = levels(sf_nuts1$nuts1))

# Read LCM
r_lcm_nir <- raster("data/lcm/lcm2015ni25mfinal.tif")
r_lcm_gbr <- raster("data/lcm/lcm2015gb25mfinal.tif")

# LCM lookup table
lookup_lcm <- read_csv("data/lcm/lcm_lookup.csv") %>% select(layer, code) %>% set_names(c("lcm_layer", "lcm"))

# Reclassify unclassified and saltwater -> NA
r_lcm_nir <- reclassify(r_lcm_nir, cbind(0, NA))
r_lcm_nir <- reclassify(r_lcm_nir, cbind(13, NA))
r_lcm_gbr <- reclassify(r_lcm_gbr, cbind(0, NA))
r_lcm_gbr <- reclassify(r_lcm_gbr, cbind(13, NA))

# Crop raster to extent of each UK country
r_lcm_nir <- r_lcm_nir
r_lcm_eng <- r_lcm_gbr %>% crop(as_Spatial(st_transform(sf_country %>% filter(country == "England"), as.character(crs(r_lcm_gbr)))))
r_lcm_sct <- r_lcm_gbr %>% crop(as_Spatial(st_transform(sf_country %>% filter(country == "Scotland"), as.character(crs(r_lcm_gbr)))))
r_lcm_wal <- r_lcm_gbr %>% crop(as_Spatial(st_transform(sf_country %>% filter(country == "Wales"), as.character(crs(r_lcm_gbr)))))

# Rasterize NUTS1 polygons
r_nuts1_nir <- fasterize(st_transform(sf_nuts1, as.character(crs(r_lcm_nir))), field = "nuts1", r_lcm_nir)
r_nuts1_eng <- fasterize(st_transform(sf_nuts1, as.character(crs(r_lcm_eng))), field = "nuts1", r_lcm_eng)
r_nuts1_sct <- fasterize(st_transform(sf_nuts1, as.character(crs(r_lcm_sct))), field = "nuts1", r_lcm_sct)
r_nuts1_wal <- fasterize(st_transform(sf_nuts1, as.character(crs(r_lcm_wal))), field = "nuts1", r_lcm_wal)

# If no LCM, set NUTS1 to 0 (so LCM but no NUTS1 is NA)
r_nuts1_nir[is.na(r_lcm_nir)] <- 0
r_nuts1_eng[is.na(r_lcm_eng)] <- 0
r_nuts1_sct[is.na(r_lcm_sct)] <- 0
r_nuts1_wal[is.na(r_lcm_wal)] <- 0

# Function for replacing nearest NA value with nearest non-0 value
replace_NA_nearest_n0 <- function(x){
  # edge <- boundaries(x, type = "inner", classes = FALSE)
  na_cells <- which(is.na(x[]))
  n0_cells <- which(x[]!= 0)
  
  nas_xy <- cbind(xyFromCell(x, na_cells), val = NA)
  n0s_xy <- cbind(xyFromCell(x, n0_cells), val = values(x)[n0_cells])
  
  n0s_tree <- createTree(coordinates(n0s_xy[, c(1, 2)]))
  nn_ids <- knnLookup(n0s_tree, newdat = coordinates(nas_xy[, c(1, 2)]), k = 1)
  
  length(na_cells)
  length(nn_ids)
  
  x[na_cells] <- x[x[]!=0][nn_ids]
  
  return(x)
}

# Run
r_nuts1_nir <- replace_NA_nearest_n0(r_nuts1_nir)
r_nuts1_eng <- replace_NA_nearest_n0(r_nuts1_eng)
r_nuts1_sct <- replace_NA_nearest_n0(r_nuts1_sct)
r_nuts1_wal <- replace_NA_nearest_n0(r_nuts1_wal)

# Return 0 values to NA
r_nuts1_nir[r_nuts1_nir == 0] <- NA
r_nuts1_eng[r_nuts1_eng == 0] <- NA
r_nuts1_sct[r_nuts1_sct == 0] <- NA
r_nuts1_wal[r_nuts1_wal == 0] <- NA

# Convert non-focal GB countries to NA
r_nuts1_eng[r_nuts1_eng %in% lookup_nuts1$nuts1_layer[lookup_nuts1$nuts1 %in% c("Scotland", "Wales")]] <- NA
r_nuts1_sct[r_nuts1_sct %in% lookup_nuts1$nuts1_layer[lookup_nuts1$nuts1 != "Scotland"]] <- NA
r_nuts1_wal[r_nuts1_wal %in% lookup_nuts1$nuts1_layer[lookup_nuts1$nuts1 != "Wales"]] <- NA

# If out of country, LCM is NA
r_lcm_nir[is.na(r_nuts1_nir)] <- NA
r_lcm_eng[is.na(r_nuts1_eng)] <- NA
r_lcm_sct[is.na(r_nuts1_sct)] <- NA
r_lcm_wal[is.na(r_nuts1_wal)] <- NA

# Trim
r_lcm_nir <- trim(r_lcm_nir)
r_lcm_eng <- trim(r_lcm_eng)
r_lcm_sct <- trim(r_lcm_sct)
r_lcm_wal <- trim(r_lcm_wal)
r_nuts1_nir <- trim(r_nuts1_nir)
r_nuts1_eng <- trim(r_nuts1_eng)
r_nuts1_sct <- trim(r_nuts1_sct)
r_nuts1_wal <- trim(r_nuts1_wal)

# Save
writeRaster(r_lcm_nir, file = "rasters/r_lcm_nir.tif", overwrite = TRUE)
writeRaster(r_lcm_eng, file = "rasters/r_lcm_eng.tif", overwrite = TRUE)
writeRaster(r_lcm_sct, file = "rasters/r_lcm_sct.tif", overwrite = TRUE)
writeRaster(r_lcm_wal, file = "rasters/r_lcm_wal.tif", overwrite = TRUE)
writeRaster(r_nuts1_nir, file = "rasters/r_nuts1_nir.tif", overwrite = TRUE)
writeRaster(r_nuts1_eng, file = "rasters/r_nuts1_eng.tif", overwrite = TRUE)
writeRaster(r_nuts1_sct, file = "rasters/r_nuts1_sct.tif", overwrite = TRUE)
writeRaster(r_nuts1_wal, file = "rasters/r_nuts1_wal.tif", overwrite = TRUE)
save(lookup_nuts1, file = "rdata/raster_lookup_nuts1.RData")
save(lookup_lcm, file = "rdata/raster_lookup_lcm.RData")

# Reload
r_lcm_nir <- raster("rasters/r_lcm_nir.tif")
r_lcm_eng <- raster("rasters/r_lcm_eng.tif")
r_lcm_sct <- raster("rasters/r_lcm_sct.tif")
r_lcm_wal <- raster("rasters/r_lcm_wal.tif")
r_nuts1_nir <- raster("rasters/r_nuts1_nir.tif")
r_nuts1_eng <- raster("rasters/r_nuts1_eng.tif")
r_nuts1_sct <- raster("rasters/r_nuts1_sct.tif")
r_nuts1_wal <- raster("rasters/r_nuts1_wal.tif")
load("rdata/raster_lookup_nuts1.RData")
load("rdata/raster_lookup_lcm.RData")

# Get values into memory
v_lcm_nir <- getValues(r_lcm_nir)
v_lcm_eng <- getValues(r_lcm_eng)
v_lcm_sct <- getValues(r_lcm_sct)
v_lcm_wal <- getValues(r_lcm_wal)


## REFINE LCM -----------------------------
### Woodland habitat
# Shapefiles
# st_layers("priority_habitats/Woodland.gdb")
sf_wood_eng <- st_read("priority_habitats/Woodland.gdb", layer = "Ancient_Woodland_Inventory_v3_3") %>% 
  st_transform(as.character(bng)) %>% 
  select(STATUS) %>% 
  rename('type' = STATUS) %>% 
  mutate(lcm = ifelse(type == "PAWS", 2, 1))
sf_wood_wal <- st_read("priority_habitats/Woodland.gdb", layer = "AncientWoodlandInventory2011") %>% 
  st_transform(as.character(bng)) %>% 
  select(CAT_NAME) %>% 
  rename('type' = CAT_NAME) %>% 
  mutate(lcm = ifelse(type == "Plantation on Ancient Woodland Site", 2, 1))
sf_wood_sct <- st_read("priority_habitats/Woodland.gdb", layer = "FCPRODUCT_S_NWSS") %>% 
  st_transform(as.character(bng)) %>% 
  select(TYPE, DOM_HABITA) %>% 
  rename('type' = TYPE, 'habitat' = DOM_HABITA) %>% 
  filter(type != "Open land habitat") %>% 
  mutate(lcm = ifelse(type == "PAWS" | habitat == "Native pinewood", 2, 1))
sf_wood_nir <- st_read("priority_habitats/Woodland.gdb", layer = "PriorityHabitats_Woodland") %>% 
  st_transform(crs(r_lcm_nir)) %>% 
  select(Hab_Type) %>% 
  rename('type' = Hab_Type) %>% 
  mutate(lcm = ifelse(type == "Woodland", 1, 2)) 

# Rasterize
r_wood_nir <- fasterize(sf_wood_nir, r_lcm_nir, field = 'lcm')
r_wood_eng <- fasterize(sf_wood_eng, r_lcm_eng, field = 'lcm')
r_wood_wal <- fasterize(sf_wood_wal, r_lcm_wal, field = 'lcm')
r_wood_sct <- fasterize(sf_wood_sct, r_lcm_sct, field = 'lcm')

# If is.na(LCM) <- NA
r_wood_nir[is.na(v_lcm_nir)] <- NA
r_wood_eng[is.na(v_lcm_eng)] <- NA
r_wood_sct[is.na(v_lcm_sct)] <- NA
r_wood_wal[is.na(v_lcm_wal)] <- NA 

# Get values
v_wood_nir <- getValues(r_wood_nir)
v_wood_eng <- getValues(r_wood_eng)
v_wood_wal <- getValues(r_wood_wal)
v_wood_sct <- getValues(r_wood_sct)



### National Forest Inventory (GB only)
# Shapefiles
sf_forest_gbr <- st_read("priority_habitats/NFI/NATIONAL_FOREST_INVENTORY_GB_2015.shp") %>% 
  st_transform(as.character(bng)) %>% 
  filter(Category == "Woodland") %>% 
  select(IFT_IOA) %>% 
  rename('type' = IFT_IOA) %>% 
  mutate(lcm = case_when(type %in% c("Broadleaved", "Mixed mainly broadleaved", "Coppice", "Coppice with standards") ~ 1L,
                         type %in% c("Conifer", "Mixed mainly conifer", "Windthrow") ~ 2L,
                         TRUE ~ NA_integer_))
sf_forest_nir <- st_read("priority_habitats/Woodland.gdb", layer = "NIWoodlandBasemapApril2015") %>% 
  st_transform(crs(r_lcm_nir)) %>% 
  select(WOODTYPE) %>% 
  rename('type' = WOODTYPE) %>% 
  mutate(lcm = case_when(type %in% c("BROADLEAF", "MIXED") ~ 1L,
                         type %in% c("CONIFER") ~ 2L,
                         TRUE ~ NA_integer_))

# Rasterize
r_forest_nir <- fasterize(sf_forest_nir, r_lcm_nir, field = 'lcm')
r_forest_eng <- fasterize(sf_forest_gbr, r_lcm_eng, field = 'lcm')
r_forest_wal <- fasterize(sf_forest_gbr, r_lcm_wal, field = 'lcm')
r_forest_sct <- fasterize(sf_forest_gbr, r_lcm_sct, field = 'lcm')

# If is.na(LCM) <- NA
r_forest_nir[is.na(v_lcm_nir)] <- NA
r_forest_eng[is.na(v_lcm_eng)] <- NA
r_forest_sct[is.na(v_lcm_sct)] <- NA
r_forest_wal[is.na(v_lcm_wal)] <- NA 

# Get values
v_forest_nir <- getValues(r_forest_nir)
v_forest_eng <- getValues(r_forest_eng)
v_forest_wal <- getValues(r_forest_wal)
v_forest_sct <- getValues(r_forest_sct)



### Priority Habitats Inventory
phi_lcm_lookup <- read_csv("data/lcm/phi_lcm_lookup.csv")
# st_layers("priority_habitats/UK_PHIs.gdb")
# Northern Ireland
sf_habitats_g_nir <- st_read("priority_habitats/UK_PHIs.gdb", layer = "PriorityHabitats_GrasslandInventory_Update") %>% 
  st_transform(crs(r_lcm_nir)) %>% 
  filter(SurveyType == "Phase 1") %>% 
  mutate(NVC = gsub("\\s", "", NVC)) %>% 
  inner_join(phi_lcm_lookup %>% filter(country == "Northern Ireland") %>% select(-country), by = c("NVC" = "habitat")) %>% 
  select(lcm)
sf_habitats_f_nir <- st_read("priority_habitats/UK_PHIs.gdb", layer = "PriorityHabitats_Fens") %>% 
  st_transform(crs(r_lcm_nir)) %>% 
  mutate(lcm = 8) %>% 
  select(lcm)
sf_habitats_h_nir <- st_read("priority_habitats/UK_PHIs.gdb", layer = "PriorityHabitats_Heathland") %>% 
  st_transform(crs(r_lcm_nir)) %>% 
  mutate(lcm = 9) %>% 
  select(lcm)
# England
sf_habitats_eng <- st_read("priority_habitats/UK_PHIs.gdb", layer = "PHI_England_v1") %>% 
  st_transform(as.character(bng)) %>% 
  full_join(phi_lcm_lookup %>% filter(country == "England") %>% select(-country), by = c("main_habit" = "habitat")) %>% 
  select(lcm)
# Wales
sf_habitats_wal <- st_read("priority_habitats/UK_PHIs.gdb", layer = "GWC_NRW_SensitiveHabitats") %>% 
  st_transform(as.character(bng)) %>% 
  full_join(phi_lcm_lookup %>% filter(country == "Wales") %>% select(-country), by = c("habitat" = "habitat")) %>% 
  select(lcm)
# Scotland
sf_habitats_sct <- st_read("priority_habitats/UK_PHIs.gdb", layer = "HabMos_priority_habitats_v1") %>% 
  st_transform(as.character(bng)) %>% 
  mutate(HAB_CODE = ifelse(substring(HAB_CODE, 1, 2) == "E1", HAB_CODE, substring(HAB_CODE, 1, 2))) %>% 
  full_join(phi_lcm_lookup %>% filter(country == "Scotland") %>% select(-country), by = c("HAB_CODE" = "habitat")) %>% 
  select(lcm)

# Rasterize
r_habitats_nir <- stack(fasterize(sf_habitats_g_nir, r_lcm_nir, field = 'lcm'),
                        fasterize(sf_habitats_f_nir, r_lcm_nir, field = 'lcm'),
                        fasterize(sf_habitats_h_nir, r_lcm_nir, field = 'lcm'))

r_habitats_nir[[1]][] <- ifelse(!is.na(r_habitats_nir[[1]][]), r_habitats_nir[[1]][], ifelse(!is.na(r_habitats_nir[[2]][]), r_habitats_nir[[2]][], r_habitats_nir[[3]][]))
r_habitats_nir <- r_habitats_nir[[1]]

r_habitats_eng <- fasterize(sf_habitats_eng, r_lcm_eng, field = 'lcm')
r_habitats_wal <- fasterize(sf_habitats_wal, r_lcm_wal, field = 'lcm')
r_habitats_sct <- fasterize(sf_habitats_sct, r_lcm_sct, field = 'lcm')

# If is.na(LCM) <- NA
r_habitats_nir[is.na(v_lcm_nir)] <- NA
r_habitats_eng[is.na(v_lcm_eng)] <- NA
r_habitats_sct[is.na(v_lcm_sct)] <- NA
r_habitats_wal[is.na(v_lcm_wal)] <- NA 

# Get values
v_habitats_nir <- getValues(r_habitats_nir)
v_habitats_eng <- getValues(r_habitats_eng)
v_habitats_wal <- getValues(r_habitats_wal)
v_habitats_sct <- getValues(r_habitats_sct)


### Update LCM
v_lcm_new_nir <- case_when(!is.na(v_wood_nir) ~ v_wood_nir, 
                           !is.na(v_forest_nir) & is.na(v_wood_nir) ~ v_forest_nir,
                           !is.na(v_habitats_nir) & is.na(v_wood_nir) & is.na(v_forest_nir) ~ v_habitats_nir,
                           TRUE ~ v_lcm_nir)
v_lcm_new_eng <- case_when(!is.na(v_wood_eng) ~ v_wood_eng, 
                           !is.na(v_forest_eng) & is.na(v_wood_eng) ~ v_forest_eng,
                           !is.na(v_habitats_eng) & is.na(v_wood_eng) & is.na(v_forest_eng) ~ v_habitats_eng,
                           TRUE ~ v_lcm_eng)
v_lcm_new_sct <- case_when(!is.na(v_wood_sct) ~ v_wood_sct, 
                           !is.na(v_forest_sct) & is.na(v_wood_sct) ~ v_forest_sct,
                           !is.na(v_habitats_sct) & is.na(v_wood_sct) & is.na(v_forest_sct) ~ v_habitats_sct,
                           TRUE ~ v_lcm_sct)
v_lcm_new_wal <- case_when(!is.na(v_wood_wal) ~ v_wood_wal, 
                           !is.na(v_forest_wal) & is.na(v_wood_wal) ~ v_forest_wal,
                           !is.na(v_habitats_wal) & is.na(v_wood_wal) & is.na(v_forest_wal) ~ v_habitats_wal,
                           TRUE ~ v_lcm_wal)

r_lcm_new_nir <- r_lcm_nir
r_lcm_new_eng <- r_lcm_eng
r_lcm_new_wal <- r_lcm_wal
r_lcm_new_sct <- r_lcm_sct

r_lcm_new_nir[] <- v_lcm_new_nir
r_lcm_new_eng[] <- v_lcm_new_eng
r_lcm_new_wal[] <- v_lcm_new_wal
r_lcm_new_sct[] <- v_lcm_new_sct

writeRaster(r_lcm_new_nir, file = "rasters/r_lcm_new_nir.tif", overwrite = TRUE)
writeRaster(r_lcm_new_eng, file = "rasters/r_lcm_new_eng.tif", overwrite = TRUE)
writeRaster(r_lcm_new_sct, file = "rasters/r_lcm_new_sct.tif", overwrite = TRUE)
writeRaster(r_lcm_new_wal, file = "rasters/r_lcm_new_wal.tif", overwrite = TRUE)

# Reload
r_lcm_nir <- raster("rasters/r_lcm_new_nir.tif")
r_lcm_eng <- raster("rasters/r_lcm_new_eng.tif")
r_lcm_sct <- raster("rasters/r_lcm_new_sct.tif")
r_lcm_wal <- raster("rasters/r_lcm_new_wal.tif")

# Get values into memory
v_lcm_nir <- getValues(r_lcm_nir)
v_lcm_eng <- getValues(r_lcm_eng)
v_lcm_sct <- getValues(r_lcm_sct)
v_lcm_wal <- getValues(r_lcm_wal)


## AGRICULTURAL LAND CLASSIFICATION -----------------------------
# Read ALC sf
sf_alc_nir <- st_read("data/alc/AFBI_SS/AFBI_Soil_Series_Map_of_Northern_Ireland_RSPB.shp") %>% 
  select(LANDCLASS) %>% 
  rename("alc" = LANDCLASS) %>% 
  st_transform(as.character(bng)) %>% 
  filter(!alc %in% c("NULL", "5D", "5U")) %>% 
  mutate(alc = droplevels(alc))

sf_alc_eng <- st_read("data/alc/NE_AgriculturalLandClassificationProvisionalEngland_SHP_Full/data/Agricultural_Land_Classification_Provisional_England.shp") %>% 
  select(alc_grade) %>% 
  rename("alc" = alc_grade) %>% 
  st_transform(as.character(bng)) %>% 
  filter(!alc %in% c("Urban", "Non Agricultural", "Exclusion")) %>% 
  mutate(alc = droplevels(alc))

sf_alc_sct <- st_read("data/alc/Hutton_LCA250K_OpenData/LCA_250K.shp") %>% 
  select(LCCODE) %>% 
  rename("alc" = LCCODE) %>% 
  mutate(alc = round(alc, 1)) %>% 
  st_transform(as.character(bng)) %>% 
  filter(!alc %in% c(888, 999, 9500)) %>% 
  mutate(alc = as.factor(alc))

sf_alc_wal <- st_read("data/alc/Wales_PredictiveALC/PredictiveALC.shp") %>% 
  select(predictive) %>% 
  rename("alc" = predictive) %>% 
  st_transform(as.character(bng)) %>% 
  filter(!alc %in% c("NA", "U")) %>% 
  mutate(alc = droplevels(alc))

# Fasterise ALC polygons
r_alc_nir <- fasterize(st_transform(sf_alc_nir, as.character(crs(r_lcm_nir))), field = "alc", r_lcm_nir)
r_alc_eng <- fasterize(st_transform(sf_alc_eng, as.character(crs(r_lcm_eng))), field = "alc", r_lcm_eng)
r_alc_sct <- fasterize(st_transform(sf_alc_sct, as.character(crs(r_lcm_sct))), field = "alc", r_lcm_sct)
r_alc_wal <- fasterize(st_transform(sf_alc_wal, as.character(crs(r_lcm_wal))), field = "alc", r_lcm_wal)

# If no LCM then ALC is 0 (NA later)
r_alc_nir[is.na(v_lcm_nir)] <- 0
r_alc_eng[is.na(v_lcm_eng)] <- 0
r_alc_sct[is.na(v_lcm_sct)] <- 0
r_alc_wal[is.na(v_lcm_wal)] <- 0


# Replace NAs with nearest
r_alc_nir <- replace_NA_nearest_n0(r_alc_nir)
r_alc_eng <- replace_NA_nearest_n0(r_alc_eng)
r_alc_sct <- replace_NA_nearest_n0(r_alc_sct)
r_alc_wal <- replace_NA_nearest_n0(r_alc_wal)

# Replace 0s with NA
r_alc_nir[r_alc_nir == 0] <- NA
r_alc_eng[r_alc_eng == 0] <- NA
r_alc_sct[r_alc_sct == 0] <- NA
r_alc_wal[r_alc_wal == 0] <- NA


# Lookup
lookup_alc <- bind_rows(tibble(alc_layer = order(levels(sf_alc_nir$alc)),
                               alc = as.character(levels(sf_alc_nir$alc)),
                               country = "Northern Ireland"),
                        tibble(alc_layer = order(levels(sf_alc_eng$alc)),
                               alc = as.character(levels(sf_alc_eng$alc)),
                               country = "England"),
                        tibble(alc_layer = order(levels(sf_alc_sct$alc)),
                               alc = as.character(levels(sf_alc_sct$alc)),
                               country = "Scotland"),
                        tibble(alc_layer = order(levels(sf_alc_wal$alc)),
                               alc = as.character(levels(sf_alc_wal$alc)),
                               country = "Wales"))

# Save to disk
writeRaster(r_alc_nir, file = "rasters/r_alc_nir.tif", overwrite = TRUE)
writeRaster(r_alc_eng, file = "rasters/r_alc_eng.tif", overwrite = TRUE)
writeRaster(r_alc_sct, file = "rasters/r_alc_sct.tif", overwrite = TRUE)
writeRaster(r_alc_wal, file = "rasters/r_alc_wal.tif", overwrite = TRUE)
save(lookup_alc, file = "rdata/raster_lookup_alc.RData")

# Reload
r_alc_nir <- raster("rasters/r_alc_nir.tif")
r_alc_eng <- raster("rasters/r_alc_eng.tif")
r_alc_sct <- raster("rasters/r_alc_sct.tif")
r_alc_wal <- raster("rasters/r_alc_wal.tif")
load("rdata/raster_lookup_alc.RData")


## HIGH-GRADE SOIL (ENG) ----------------------
r_alc_eng <- raster("rasters/r_alc_eng.tif")

# Read NUTS1 shapefile
sf_nuts1 <- st_read("data/nuts1/NUTS_Level_1_January_2018_Full_Extent_Boundaries_in_the_United_Kingdom.shp") %>% 
  select(nuts118nm) %>% 
  rename("nuts1" = nuts118nm) %>% 
  st_transform(as.character(bng)) 

# Read HWSD raster - from RSPB G Drive. login : https://remote.rspb.org.uk/
r_soil <- raster("G:/GIS_Data/Soils/Harmonised_World_Soil_Database/HWSD_raster.tif") 

# Crop HWSD to NUTS1 extent
r_soil <- r_soil %>% crop(as_Spatial(st_transform(st_buffer(st_as_sfc(st_bbox(sf_nuts1)), dist = 10000), as.character(crs(r_soil)))))

# Load lookup table
lookup_soil <- read_csv("data/soil/lookup_soil.csv", guess_max = 100000) %>% 
  # select(MU_GLOBAL) %>%
  filter(MU_GLOBAL %in% unique(r_soil))

# Reproject tp 25m soil
r_soil_eng <- projectRaster(r_soil, r_alc_eng, method = "ngb")

# If LCM <- NA
r_soil_eng[is.na(r_alc_eng)] <- NA

# If soil ID <= 6 <- NA
r_soil_eng[r_soil_eng <= 6] <- NA


# Crosstabulate LCM/soil areas
df_soil_lcm <- crosstabDT(r_alc_eng, r_soil_eng, useNA = TRUE, long = TRUE)

highgrade <- df_soil_lcm %>% 
  as_tibble() %>% 
  set_names(c("alc", "soil", "freq")) %>% 
  filter(complete.cases(.)) %>%
  # Strip out alc 3
  filter(alc != 3) %>%
  mutate(highgrade = ifelse(alc <= 2, 1, 0)) %>% 
  group_by(highgrade, soil) %>% 
  summarise(freq = sum(freq), .groups = "drop") %>% 
  # Identify most freq ALC per soil
  group_by(soil) %>% 
  filter(freq == max(freq)) %>% 
  select(-freq) %>% 
  ungroup()

r_highgrade_eng <- subs(r_soil_eng, highgrade, by = 2, which = 1)

writeRaster(r_highgrade_eng, "rasters/r_highgrade_eng.tif", overwrite = TRUE)



## ALC ENGLAND - ADD 3b ------------------
# Load original ALC
r_alc_eng <- raster("rasters/r_alc_eng.tif")
load("rdata/raster_lookup_alc.RData")

# Make space for 3b
r_alc_eng <- subs(r_alc_eng, tibble(x = c(1, 2, 3, 4, 5), y = c(1, 2, 3, 5, 6)), by = 1, which = 2)

# Calculate slope angle
r_elev_eng <- raster("rasters/r_elev_eng.tif")
r_slope_eng <- terrain(r_elev_eng, opt = "slope", unit = "degrees")

# Load high-grade
r_highgrade_eng <- raster("rasters/r_highgrade_eng.tif")

# Add 3b (coded as 4) where alc == 3 & slope > 7 OR 
r_alc_eng[r_alc_eng == 3 & (r_slope_eng > 7 | r_highgrade_eng == 0)] <- 4

# Edit lookup
lookup_alc <- lookup_alc %>% 
  mutate(alc_layer = ifelse(country == "England" & alc_layer >= 4, alc_layer + 1, alc_layer),
         alc = ifelse(country == "England" & alc_layer == 3, "Grade 3A", alc)) %>% 
  add_row(alc_layer = 4,
          alc = "Grade 3B",
          country = "England") %>% 
  arrange(country, alc_layer)


# Save
writeRaster(r_alc_eng, file = "rasters/r_alc2_eng.tif", overwrite = TRUE)
save(lookup_alc, file = "rdata/raster_lookup_alc2.RData")




## SALTMARSH POTENTIAL ------------------------
# Sustainable Shores sites
sf_saltmarsh_high <- st_read("data/saltmarsh/SustainableShores_priority_sites.shp") %>% 
  st_transform(as.character(bng)) %>% 
  select(Name) %>% 
  st_buffer(dist = 0) %>% 
  st_cast("MULTIPOLYGON") %>% 
  rename('name' = Name) %>% 
  # Remove if area is exactly 1 or 100 km2 - these area just grid squares where matching with site polygons has failed
  filter(!round(as.numeric(st_area(.))/10000) %in% c(1, 100)) %>% 
  mutate(priority = TRUE)
sf_saltmarsh_low <- st_read("data/saltmarsh/SustainableShores_all_potential_sites.shp") %>% 
  st_transform(as.character(bng)) %>% 
  select(Name) %>% 
  st_buffer(dist = 0) %>% 
  st_cast("MULTIPOLYGON") %>% 
  rename('name' = Name) %>% 
  # Remove if area is exactly 1 or 100 km2 - these area just grid squares where matching with site polygons has failed
  filter(!round(as.numeric(st_area(.))/10000) %in% c(1, 100)) %>% 
  mutate(priority = FALSE)

# Remove from Low priority if in High priority
sf_saltmarsh_low <- sf_saltmarsh_low[!(sf_saltmarsh_low$geometry %in% sf_saltmarsh_high$geometry),]

# Combine low and high priority
sf_saltmarsh <- rbind(sf_saltmarsh_high,
                      sf_saltmarsh_low)

# Build lookup table
lookup_saltmarsh <- sf_saltmarsh %>%
  st_set_geometry(NULL) %>% 
  as.tbl() %>% 
  mutate(saltmarsh_site = as.numeric(name))

# Fasterize
r_saltmarsh_nir <- fasterize(st_transform(sf_saltmarsh, crs(r_lcm_nir)), r_lcm_nir, field = 'name')
r_saltmarsh_eng <- fasterize(sf_saltmarsh, r_lcm_eng, field = 'name')
r_saltmarsh_wal <- fasterize(sf_saltmarsh, r_lcm_wal, field = 'name')
r_saltmarsh_sct <- fasterize(sf_saltmarsh, r_lcm_sct, field = 'name')

# If no LCM <- NA
r_saltmarsh_nir[is.na(v_lcm_nir)] <- NA
r_saltmarsh_eng[is.na(v_lcm_eng)] <- NA
r_saltmarsh_wal[is.na(v_lcm_wal)] <- NA
r_saltmarsh_sct[is.na(v_lcm_sct)] <- NA

# If LCM already saltmarsh/coastal <- NA
r_saltmarsh_nir[v_lcm_nir %in% c(15, 16, 17, 18, 19)] <- NA
r_saltmarsh_eng[v_lcm_eng %in% c(15, 16, 17, 18, 19)] <- NA
r_saltmarsh_wal[v_lcm_wal %in% c(15, 16, 17, 18, 19)] <- NA
r_saltmarsh_sct[v_lcm_sct %in% c(15, 16, 17, 18, 19)] <- NA

# Save
writeRaster(r_saltmarsh_nir, file = "rasters/r_saltmarsh_nir.tif", overwrite = TRUE)
writeRaster(r_saltmarsh_eng, file = "rasters/r_saltmarsh_eng.tif", overwrite = TRUE)
writeRaster(r_saltmarsh_wal, file = "rasters/r_saltmarsh_wal.tif", overwrite = TRUE)
writeRaster(r_saltmarsh_sct, file = "rasters/r_saltmarsh_sct.tif", overwrite = TRUE)
save(lookup_saltmarsh, file = "rdata/raster_lookup_saltmarsh.RData")


## PEAT EXTENT ------------------------
# Read shapefiles
sf_peat_nir <- st_read("data/peat/Peat/Northern_Ireland_peatlands_NoBuffer.shp") %>%
  st_transform(as.character(crs(r_lcm_nir)))
sf_peat_eng <- st_read("data/peat/Peat/Peat_England_combined_v3.shp") %>%
  st_transform(as.character(bng))
sf_peat_sct <- st_read("data/peat/Peat/Peat_Scotland_combined_v3.shp") %>%
  st_transform(as.character(bng))
sf_peat_wal <- st_read("data/peat/Peat/Peat_Wales_combined_v3.shp") %>%
  st_transform(as.character(bng))

# Fasterize
r_peat_nir <- fasterize(sf_peat_nir, r_lcm_nir)
r_peat_eng <- fasterize(sf_peat_eng, r_lcm_eng)
r_peat_sct <- fasterize(sf_peat_sct, r_lcm_sct)
r_peat_wal <- fasterize(sf_peat_wal, r_lcm_wal)

# Non-peat to 0
r_peat_nir[is.na(r_peat_nir)] <- 0
r_peat_eng[is.na(r_peat_eng)] <- 0
r_peat_sct[is.na(r_peat_sct)] <- 0
r_peat_wal[is.na(r_peat_wal)] <- 0

# If is.na(LCM) <- 0
r_peat_nir[is.na(v_lcm_nir)] <- NA
r_peat_eng[is.na(v_lcm_eng)] <- NA
r_peat_sct[is.na(v_lcm_sct)] <- NA
r_peat_wal[is.na(v_lcm_wal)] <- NA

# If earmarked for intertidal habitat <- 0
r_peat_nir[!is.na(r_saltmarsh_nir)] <- 0
r_peat_eng[!is.na(r_saltmarsh_eng)] <- 0
r_peat_sct[!is.na(r_saltmarsh_sct)] <- 0
r_peat_wal[!is.na(r_saltmarsh_wal)] <- 0

# If incompatible LCM <- 0
r_peat_nir[v_lcm_nir %in% c(12, 13, 15, 16, 17, 18, 19, 20, 21)] <- 0
r_peat_eng[v_lcm_eng %in% c(12, 13, 15, 16, 17, 18, 19, 20, 21)] <- 0
r_peat_sct[v_lcm_sct %in% c(12, 13, 15, 16, 17, 18, 19, 20, 21)] <- 0
r_peat_wal[v_lcm_wal %in% c(12, 13, 15, 16, 17, 18, 19, 20, 21)] <- 0

# Save raster
writeRaster(r_peat_nir, file = "rasters/r_peat_nir.tif", overwrite = TRUE)
writeRaster(r_peat_eng, file = "rasters/r_peat_eng.tif", overwrite = TRUE)
writeRaster(r_peat_wal, file = "rasters/r_peat_wal.tif", overwrite = TRUE)
writeRaster(r_peat_sct, file = "rasters/r_peat_sct.tif", overwrite = TRUE)



## ORGANOMINERAL SOIL ------------------------
# Read NUTS1 shapefile
sf_nuts1 <- st_read("data/nuts1/NUTS_Level_1_January_2018_Full_Extent_Boundaries_in_the_United_Kingdom.shp") %>% 
  select(nuts118nm) %>% 
  rename("nuts1" = nuts118nm) %>% 
  st_transform(as.character(bng)) 

# Read HWSD raster - from RSPB G Drive. login : https://remote.rspb.org.uk/
r_soil <- raster("data/soil/HWSD_raster.tif") 

# Crop HWSD to NUTS1 extent
r_soil <- r_soil %>% crop(as_Spatial(st_transform(st_buffer(st_as_sfc(st_bbox(sf_nuts1)), dist = 10000), as.character(crs(r_soil)))))

# Load lookup table
lookup_soil <- read_csv("data/soil/lookup_soil.csv", guess_max = 100000)

# Identify mineral soil vs other (organic / organomineral)
lookup_soil <- lookup_soil %>% 
  select(MU_GLOBAL, SU_SYM90, REF_DEPTH, ISSOIL, SEQ, SHARE, T_OC, T_REF_BULK_DENSITY, T_BULK_DENSITY, T_CLAY, S_OC, S_REF_BULK_DENSITY, S_BULK_DENSITY) %>%
  # Filter only focal soil types
  filter(MU_GLOBAL %in% unique(r_soil)) %>%
  # If topsoils and subsoil missing, removed
  filter(!(is.na(T_OC) & is.na(S_OC))) %>%
  # If subsoil missing and depth <= 30cm...
  mutate(S_OC = ifelse(is.na(S_OC) & REF_DEPTH <= 30, 0, S_OC),
         S_BULK_DENSITY = ifelse(is.na(S_BULK_DENSITY) & REF_DEPTH <= 30, 0, S_BULK_DENSITY)) %>%
  # Recalculate SHARE
  group_by(MU_GLOBAL) %>%
  mutate(share = SHARE / sum(SHARE)) %>%
  ungroup() %>%
  # Calculate total C
  mutate(c_density = (T_OC / 100 * (T_BULK_DENSITY * 10000) * 0.3) + (S_OC / 100 * (S_BULK_DENSITY * 10000) * 0.7),
         c_density30 = (T_OC / 100 * (T_BULK_DENSITY * 10000) * 0.3),
         histosol = substr(SU_SYM90, 1, 2) == "HS") %>%
  arrange(MU_GLOBAL) %>%
  group_by(MU_GLOBAL) %>%
  summarise(histosol = sum(histosol & SEQ == min(SEQ)),
            c_density_dom = c_density[SEQ == min(SEQ)],
            c_density_wtd = sum(c_density * share),
            topsoil_som_dom = T_OC[SEQ == min(SEQ)],
            topsoil_som_wtd = sum(T_OC * share),
            topsoil_clay_dom = T_CLAY[SEQ == min(SEQ)],
            topsoil_clay_wtd = sum(T_CLAY * share)) %>%
  mutate(soiltype_a = ifelse(histosol | c_density_dom > 100, 2, 1),
         soiltype_b = ifelse((topsoil_clay_dom >= 50 & topsoil_som_dom >= 6) | (topsoil_clay_dom < 50 & topsoil_som_dom >= (0.05 * topsoil_clay_dom + 3.5)), 2, 1),
         soiltype_c = ifelse((topsoil_clay_wtd >= 50 & topsoil_som_wtd >= 6) | (topsoil_clay_wtd < 50 & topsoil_som_wtd >= (0.05 * topsoil_clay_wtd + 3.5)), 2, 1))


# Map soiltypes to raster
r_soiltype_a <- subs(r_soil, select(lookup_soil, MU_GLOBAL, soiltype_a), by = 1, which = 2)
r_soiltype_b <- subs(r_soil, select(lookup_soil, MU_GLOBAL, soiltype_b), by = 1, which = 2)
r_soiltype_c <- subs(r_soil, select(lookup_soil, MU_GLOBAL, soiltype_c), by = 1, which = 2)
r_soiltype <- stack(r_soiltype_a, r_soiltype_b, r_soiltype_c)

names(r_soiltype) <- c("Option 1", "Option 2", "Option 3")

plot(r_soiltype, col = brewer.pal(2, "Oranges"), nr = 1, axes = FALSE, legend = FALSE)


# If non-land <- 0 (NA later)
r_soiltype[is.na(fasterize(st_transform(sf_nuts1, as.character(crs(r_soiltype))), r_soiltype[[1]]))] <- 0

# Function for replacing nearest NA value with nearest non-0 value
replace_NA_nearest_n0 <- function(x){
  # edge <- boundaries(x, type = "inner", classes = FALSE)
  na_cells <- which(is.na(x[]))
  n0_cells <- which(x[]!= 0)
  
  nas_xy <- cbind(xyFromCell(x, na_cells), val = NA)
  n0s_xy <- cbind(xyFromCell(x, n0_cells), val = values(x)[n0_cells])
  
  n0s_tree <- createTree(coordinates(n0s_xy[, c(1, 2)]))
  nn_ids <- knnLookup(n0s_tree, newdat = coordinates(nas_xy[, c(1, 2)]), k = 1)
  
  x[na_cells] <- x[x[]!=0][nn_ids]
  
  return(x)
}

# Replace NAs with nearest
r_soiltype[[1]] <- replace_NA_nearest_n0(r_soiltype[[1]])
r_soiltype[[2]] <- replace_NA_nearest_n0(r_soiltype[[2]])
r_soiltype[[3]] <- replace_NA_nearest_n0(r_soiltype[[3]])

# Replace 0s with NA
r_soiltype[r_soiltype == 0] <- NA

# Project to LCM grid
r_soiltype_nir <- projectRaster(r_soiltype, r_lcm_nir, method = "ngb")
r_soiltype_eng <- projectRaster(r_soiltype, r_lcm_eng, method = "ngb")
r_soiltype_wal <- projectRaster(r_soiltype, r_lcm_wal, method = "ngb")
r_soiltype_sct <- projectRaster(r_soiltype, r_lcm_sct, method = "ngb")

r_soiltype_nir3 <- r_soiltype_nir[[3]]
r_soiltype_eng3 <- r_soiltype_eng[[3]]
r_soiltype_sct3 <- r_soiltype_sct[[3]]
r_soiltype_wal3 <- r_soiltype_wal[[3]]

# If no LCM then soiltype is 0 (NA later)
v_lcm_nir <- r_lcm_nir[]
v_lcm_eng <- r_lcm_eng[]
v_lcm_sct <- r_lcm_sct[]
v_lcm_wal <- r_lcm_wal[]

r_soiltype_nir3[is.na(v_lcm_nir)] <- 0
r_soiltype_eng3[is.na(v_lcm_eng)] <- 0
r_soiltype_sct3[is.na(v_lcm_sct)] <- 0
r_soiltype_wal3[is.na(v_lcm_wal)] <- 0

# If built, coastal, rock, saltmarsh <- 0 (NA later)
r_soiltype_nir3[v_lcm_nir %in% c(12, 14, 15, 16, 17, 18, 19, 20, 21)] <- 0
r_soiltype_eng3[v_lcm_eng %in% c(12, 14, 15, 16, 17, 18, 19, 20, 21)] <- 0
r_soiltype_sct3[v_lcm_sct %in% c(12, 14, 15, 16, 17, 18, 19, 20, 21)] <- 0
r_soiltype_wal3[v_lcm_wal %in% c(12, 14, 15, 16, 17, 18, 19, 20, 21)] <- 0

# Function for replacing nearest NA value with nearest non-0 value
replace_NA_nearest_n0 <- function(x){
  # edge <- boundaries(x, type = "inner", classes = FALSE)
  na_cells <- which(is.na(x[]))
  n0_cells <- which(x[]!= 0)
  
  nas_xy <- cbind(xyFromCell(x, na_cells), val = NA)
  n0s_xy <- cbind(xyFromCell(x, n0_cells), val = values(x)[n0_cells])
  
  n0s_tree <- createTree(coordinates(n0s_xy[, c(1, 2)]))
  nn_ids <- knnLookup(n0s_tree, newdat = coordinates(nas_xy[, c(1, 2)]), k = 1)
  
  x[na_cells] <- x[x[]!=0][nn_ids]
  
  return(x)
}

# Replace NAs with nearest
r_soiltype_nir3 <- replace_NA_nearest_n0(r_soiltype_nir3)
r_soiltype_eng3 <- replace_NA_nearest_n0(r_soiltype_eng3)
r_soiltype_sct3 <- replace_NA_nearest_n0(r_soiltype_sct3)
r_soiltype_wal3 <- replace_NA_nearest_n0(r_soiltype_wal3)

# Replace 0s with NA
r_soiltype_nir3[r_soiltype_nir3 == 0] <- NA
r_soiltype_eng3[r_soiltype_eng3 == 0] <- NA
r_soiltype_sct3[r_soiltype_sct3 == 0] <- NA
r_soiltype_wal3[r_soiltype_wal3 == 0] <- NA

# Save
writeRaster(r_soiltype_nir3, file = "rasters/r_soiltype_nir.tif", overwrite = TRUE)
writeRaster(r_soiltype_eng3, file = "rasters/r_soiltype_eng.tif", overwrite = TRUE)
writeRaster(r_soiltype_sct3, file = "rasters/r_soiltype_sct.tif", overwrite = TRUE)
writeRaster(r_soiltype_wal3, file = "rasters/r_soiltype_wal.tif", overwrite = TRUE) 




# WOODLAND OPPORTUNITY MAPS ---------------------------------------------
# Read shapefile
sf_woodop_nir <- st_read("data/woodland/opportunity/Phase2_NorthernIreland_Woodland_Opportunity_v1.shp") %>%
  st_transform(as.character(crs(r_lcm_nir))) 
sf_woodop_eng <- st_read("data/woodland/opportunity/Phase2_England_Woodland_Opportunity_v4.gdb") %>%
  st_transform(as.character(bng)) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")
sf_woodop_wal <- st_read("data/woodland/opportunity/Phase2_Wales_Woodland_Opportunity_v4.shp") %>%
  st_transform(as.character(bng)) 
sf_woodop_sct <- st_read("data/woodland/opportunity/Phase2_Scotland_Woodland_Opportunity_v4.shp") %>%
  st_transform(as.character(bng)) 

# Rasterise
r_woodop_nir <- fasterize(sf_woodop_nir, r_lcm_nir)
r_woodop_eng <- fasterize(sf_woodop_eng, r_lcm_eng)
r_woodop_wal <- fasterize(sf_woodop_wal, r_lcm_wal)
r_woodop_sct <- fasterize(sf_woodop_sct, r_lcm_sct)

# Fix NAs
r_woodop_nir[is.na(v_lcm_nir)] <- NA
r_woodop_eng[is.na(v_lcm_eng)] <- NA
r_woodop_wal[is.na(v_lcm_wal)] <- NA
r_woodop_sct[is.na(v_lcm_sct)] <- NA


df <- crosstab(r_woodop_wal, r_lcm_wal, useNA = TRUE)

# Mask out fixed land covers - woodland, rock, water, coastal, built
r_woodop_nir[v_lcm_nir %in% c(1, 2, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)] <- NA
r_woodop_eng[v_lcm_eng %in% c(1, 2, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)] <- NA
r_woodop_wal[v_lcm_wal %in% c(1, 2, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)] <- NA
r_woodop_sct[v_lcm_sct %in% c(1, 2, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)] <- NA

# Mask out peat
r_woodop_nir[r_peat_nir == 1] <- NA
r_woodop_eng[r_peat_eng == 1] <- NA
r_woodop_wal[r_peat_wal == 1] <- NA
r_woodop_sct[r_peat_sct == 1] <- NA

# Mask out areas earmarked for new saltmarsh
r_woodop_nir[!is.na(r_saltmarsh_nir)] <- NA
r_woodop_eng[!is.na(r_saltmarsh_eng)] <- NA
r_woodop_wal[!is.na(r_saltmarsh_wal)] <- NA
r_woodop_sct[!is.na(r_saltmarsh_sct)] <- NA


# Save
writeRaster(r_woodop_nir, file = "rasters/r_woodop_nir.tif", overwrite = TRUE)
writeRaster(r_woodop_eng, file = "rasters/r_woodop_eng.tif", overwrite = TRUE)
writeRaster(r_woodop_wal, file = "rasters/r_woodop_wal.tif", overwrite = TRUE)
writeRaster(r_woodop_sct, file = "rasters/r_woodop_sct.tif", overwrite = TRUE)



## PAWS -----------------------
# Ancient woodland inventory shapefiles
sf_wood_eng <- st_read("priority_habitats/Woodland.gdb", layer = "Ancient_Woodland_Inventory_v3_3") %>% 
  st_transform(as.character(bng)) %>% 
  select(STATUS) %>% 
  rename('type' = STATUS) %>% 
  mutate(lcm = ifelse(type == "PAWS", 2, 1))
sf_wood_wal <- st_read("priority_habitats/Woodland.gdb", layer = "AncientWoodlandInventory2011") %>% 
  st_transform(as.character(bng)) %>% 
  select(CAT_NAME) %>% 
  rename('type' = CAT_NAME) %>% 
  mutate(lcm = ifelse(type == "Plantation on Ancient Woodland Site", 2, 1))
sf_wood_sct <- st_read("priority_habitats/Woodland.gdb", layer = "FCPRODUCT_S_NWSS") %>% 
  st_transform(as.character(bng)) %>% 
  select(TYPE, DOM_HABITA) %>% 
  rename('type' = TYPE, 'habitat' = DOM_HABITA) %>% 
  filter(type != "Open land habitat") %>% 
  mutate(lcm = ifelse(type == "PAWS" | habitat == "Native pinewood", 2, 1))

# Fasterize
r_paws_eng <- fasterize(sf_wood_eng %>% filter(type == "PAWS"), r_lcm_eng)
r_paws_wal <- fasterize(sf_wood_wal %>% filter(type == "Plantation on Ancient Woodland Site"), r_lcm_wal)
r_paws_sct <- fasterize(sf_wood_sct %>% filter(type == "PAWS"), r_lcm_sct)

# If is.na(LCM) <- NA
r_paws_eng[is.na(v_lcm_eng)] <- NA
r_paws_wal[is.na(v_lcm_wal)] <- NA
r_paws_sct[is.na(v_lcm_sct)] <- NA

# If not coniferous <- NA
r_paws_eng[v_lcm_eng != 2] <- NA
r_paws_wal[v_lcm_wal != 2] <- NA
r_paws_sct[v_lcm_sct != 2] <- NA

# Mask out peat
r_paws_eng[r_peat_eng == 1] <- NA
r_paws_wal[r_peat_wal == 1] <- NA
r_paws_sct[r_peat_sct == 1] <- NA

# Mask out areas earmarked for new saltmarsh
r_paws_eng[!is.na(r_saltmarsh_eng)] <- NA
r_paws_wal[!is.na(r_saltmarsh_wal)] <- NA
r_paws_sct[!is.na(r_saltmarsh_sct)] <- NA

# No PAWS inventory in NI = empty raster
r_paws_nir <- r_lcm_nir
r_paws_nir[] <- NA

# Save PAWS rasters
writeRaster(r_paws_nir, file = "rasters/r_paws_nir.tif", overwrite = TRUE)
writeRaster(r_paws_eng, file = "rasters/r_paws_eng.tif", overwrite = TRUE)
writeRaster(r_paws_wal, file = "rasters/r_paws_wal.tif", overwrite = TRUE)
writeRaster(r_paws_sct, file = "rasters/r_paws_sct.tif", overwrite = TRUE)


## CALEDONIAN PINEWOOD -----------------------
# Load Caledonian Pinewood Inventory
sf_pinewood_sct <- st_read("data/woodland/inventories/Caledonian_Pinewood_Inventory.shp") %>% 
  st_transform(as.character(bng)) %>% 
  select(FEATDESC) %>% 
  rename('type' = FEATDESC) 


# Rasterize
r_pinewood_sct <- fasterize(sf_pinewood_sct %>% filter(type %in% c("Caledonian Pinewood", "Cal Pine Regeneration Zone")), r_lcm_sct, field = 'type', fun = 'first')
r_pinewood_sct <- -r_pinewood_sct + 5 # Cal Pine = 1, regen zone = 2
plot(r_pinewood_sct, xlim = c(280000,320000), ylim = c(780000, 820000))

# If is.na(LCM) <- NA
r_pinewood_sct[is.na(v_lcm_sct)] <- NA

# If already broadleaved woodland <- NA
r_pinewood_sct[v_lcm_sct == 1] <- NA

# If cal pine but not c.w <- regen zone
r_pinewood_sct[r_pinewood_sct[] == 1 & v_lcm_sct != 2] <- 2

# If regen zone and c.w <- PAWS
r_paws_sct[r_pinewood_sct[] == 2 & v_lcm_sct == 2] <- 1

# If native pinewood, not PAWS
r_paws_sct[r_pinewood_sct[] == 1] <- NA

# Save
writeRaster(r_pinewood_sct, file = "rasters/r_pinewood_sct.tif", overwrite = TRUE)
writeRaster(r_paws_sct, file = "rasters/r_paws_sct.tif", overwrite = TRUE)



## NATURAL REGENERATION -----------------------------
r_lcm_nir <- raster("rasters/r_lcm_new_nir.tif")
r_lcm_eng <- raster("rasters/r_lcm_new_eng.tif")
r_lcm_sct <- raster("rasters/r_lcm_new_sct.tif")
r_lcm_wal <- raster("rasters/r_lcm_new_wal.tif")

r_woodop_nir <- raster("rasters/r_woodop_nir.tif")
r_woodop_eng <- raster("rasters/r_woodop_eng.tif")
r_woodop_wal <- raster("rasters/r_woodop_wal.tif")
r_woodop_sct <- raster("rasters/r_woodop_sct.tif")

r_pinewood_sct <- raster("rasters/r_pinewood_sct.tif")

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


# Identify broadleaved woodland (0 if not)
r_b.w_nir <- r_lcm_nir == 1
r_b.w_eng <- r_lcm_eng == 1
r_b.w_wal <- r_lcm_wal == 1
r_b.w_sct <- r_lcm_sct == 1 
r_c.w_sct <- r_pinewood_sct == 1 # Native pinewood

# NA if no woodop - restrict search to save time
r_b.w_nir[is.na(r_woodop_nir) & r_b.w_nir != 1] <- NA
r_b.w_eng[is.na(r_woodop_eng) & r_b.w_eng != 1] <- NA
r_b.w_wal[is.na(r_woodop_wal) & r_b.w_wal != 1] <- NA
r_b.w_sct[is.na(r_woodop_sct) & r_b.w_sct != 1] <- NA
r_c.w_sct[is.na(r_woodop_sct) & r_c.w_sct != 1] <- NA

# Calculate distance to nearest broadleaved woodland
r_natregen_nir <- dist_to_focal(r_b.w_nir)
r_natregen_eng <- dist_to_focal(r_b.w_eng)
r_natregen_wal <- dist_to_focal(r_b.w_wal)
r_natregen_sct <- dist_to_focal(r_b.w_sct)
r_natregen_c.w_sct <- dist_to_focal(r_c.w_sct)

# Exclude dist == 0 (i.e. existing woodland) and > 100 m (too far from seed source)
r_natregen_nir <- r_natregen_nir <= 100 & r_natregen_nir > 0
r_natregen_eng <- r_natregen_eng <= 100 & r_natregen_eng > 0
r_natregen_wal <- r_natregen_wal <= 100 & r_natregen_wal > 0
r_natregen_sct <- r_natregen_sct <= 100 & r_natregen_sct > 0
r_natregen_c.w_sct <- r_natregen_c.w_sct <= 100 & r_natregen_c.w_sct > 0

# 0 if no woodland opportunity
r_natregen_nir[is.na(r_woodop_nir)] <- 0
r_natregen_eng[is.na(r_woodop_eng)] <- 0
r_natregen_wal[is.na(r_woodop_wal)] <- 0
r_natregen_sct[is.na(r_woodop_sct)] <- 0
r_natregen_c.w_sct[is.na(r_natregen_c.w_sct)] <- 0

# If is.na(LCM) <- NA
r_natregen_nir[is.na(r_lcm_nir)] <- NA
r_natregen_eng[is.na(r_lcm_eng)] <- NA
r_natregen_wal[is.na(r_lcm_wal)] <- NA
r_natregen_sct[is.na(r_lcm_sct)] <- NA
r_natregen_c.w_sct[is.na(r_lcm_sct)] <- NA

# Save
writeRaster(r_natregen_nir, file = "rasters/r_natregen_nir.tif", overwrite = TRUE)
writeRaster(r_natregen_eng, file = "rasters/r_natregen_eng.tif", overwrite = TRUE)
writeRaster(r_natregen_wal, file = "rasters/r_natregen_wal.tif", overwrite = TRUE)
writeRaster(r_natregen_sct, file = "rasters/r_natregen_sct.tif", overwrite = TRUE)
writeRaster(r_natregen_c.w_sct, file = "rasters/r_natregen_pinewood_sct.tif", overwrite = TRUE)



## DESIGNATED SITES -----------------------
# Nationally designated sites https://www.eea.europa.eu/data-and-maps/data/nationally-designated-areas-national-cdda-15
natdes_type <- st_read("protected_areas/CDDA_2020_v01_public_EuropeEPSG3035.gdb", "DesignatedArea") %>% as.tbl() %>% 
  # NNRs, SSSIs & ASSI http://dd.eionet.europa.eu/vocabulary/cdda/designationTypeCodeValue/view?page=35#vocabularyConceptResults
  filter(designationTypeCode %in% c("UK01", "UK04", "UK98"))

sf_natdes <- st_read("protected_areas/CDDA_2020_v01_public_EuropeEPSG3035.gdb", "ProtectedSite_Polygon") %>%
  select(cddaId) %>%
  inner_join(natdes_type %>% select(cddaId, designationTypeCode), by = "cddaId") %>% 
  st_transform(as.character(bng))

# N2k sites https://www.eea.europa.eu/data-and-maps/data/natura-11
sf_n2k <- st_read("protected_areas/Natura2000_end2019_epsg3035.shp") %>% 
  filter(MS == "UK") %>% 
  st_transform(as.character(bng))

# Rasterize
r_protected_nir <- fasterize(st_transform(sf_natdes, as.character(crs(r_lcm_nir))), r_lcm_nir, background = 0) +
  fasterize(st_transform(sf_n2k, as.character(crs(r_lcm_nir))), r_lcm_nir, background = 0)
r_protected_eng <- fasterize(sf_natdes, r_lcm_eng, background = 0) + fasterize(sf_n2k, r_lcm_eng, background = 0)
r_protected_sct <- fasterize(sf_natdes, r_lcm_sct, background = 0) + fasterize(sf_n2k, r_lcm_sct, background = 0)
r_protected_wal <- fasterize(sf_natdes, r_lcm_wal, background = 0) + fasterize(sf_n2k, r_lcm_wal, background = 0)

# Replace 2 with 1 (where both N2K & nationally designated)
r_protected_nir[r_protected_nir == 2] <- 1
r_protected_eng[r_protected_eng == 2] <- 1
r_protected_sct[r_protected_sct == 2] <- 1
r_protected_wal[r_protected_wal == 2] <- 1

# NA if no LCM
r_protected_nir[is.na(v_lcm_nir)] <- NA
r_protected_eng[is.na(v_lcm_eng)] <- NA
r_protected_sct[is.na(v_lcm_sct)] <- NA
r_protected_wal[is.na(v_lcm_wal)] <- NA

# Save
writeRaster(r_protected_nir, file = "rasters/r_protected_nir.tif", overwrite = TRUE)
writeRaster(r_protected_eng, file = "rasters/r_protected_eng.tif", overwrite = TRUE)
writeRaster(r_protected_wal, file = "rasters/r_protected_wal.tif", overwrite = TRUE)
writeRaster(r_protected_sct, file = "rasters/r_protected_sct.tif", overwrite = TRUE)



## FEN/BOG -----------------------
# DEM raster
# r_dem <- raster("channels/usgs_dtm_filled.tif")
r_elev <- raster("dtm_channels/usgs_filled_dtm.tif")

# Channels
r_channels <- raster("dtm_channels/channels.tif")

r_channels[is.na(r_channels)] <- 0
r_channels[r_channels != 0] <- 1
r_channels[is.na(r_elev)] <- NA

# Calculate height to nearest stream
height_to_channel_fun <- function(x, dem){
  focal_cells <- which(x[] == 0)
  river_cells <- which(x[] == 1)
  
  focal_xy <- cbind(xyFromCell(x, focal_cells), val = NA)
  river_xy <- cbind(xyFromCell(x, river_cells), val = values(dem)[river_cells])
  nrow(focal_xy)
  
  tree <- createTree(coordinates(river_xy[, c(1, 2)]))
  nn_ids <- knnLookup(tree, newdat = coordinates(focal_xy[, c(1, 2)]), k = 1)
  
  y <- x
  y[river_cells] <- 0
  y[focal_cells] <- dem[focal_cells] - dem[river_cells[nn_ids]]
  
  return(y)
}

r_channelheight <- height_to_channel_fun(r_channels, r_elev)

# Reproject to 25m raster
r_channelheight_nir <- projectRaster(r_channelheight, r_lcm_nir)
r_channelheight_eng <- projectRaster(r_channelheight, r_lcm_eng)
r_channelheight_wal <- projectRaster(r_channelheight, r_lcm_wal)
r_channelheight_sct <- projectRaster(r_channelheight, r_lcm_sct)

# If peat is 0/NA <- -999 (0 is taken!)
r_channelheight_nir[!r_peat_nir %in% 1] <- -999
r_channelheight_eng[!r_peat_eng %in% 1] <- -999
r_channelheight_wal[!r_peat_wal %in% 1] <- -999
r_channelheight_sct[!r_peat_sct %in% 1] <- -999

# Function for replacing nearest NA value with nearest non--999 value
replace_NA_nearest_n0 <- function(x){
  # edge <- boundaries(x, type = "inner", classes = FALSE)
  na_cells <- which(is.na(x[]))
  n0_cells <- which(x[]!= -999)
  
  nas_xy <- cbind(xyFromCell(x, na_cells), val = NA)
  n0s_xy <- cbind(xyFromCell(x, n0_cells), val = values(x)[n0_cells])
  
  n0s_tree <- createTree(coordinates(n0s_xy[, c(1, 2)]))
  nn_ids <- knnLookup(n0s_tree, newdat = coordinates(nas_xy[, c(1, 2)]), k = 1)
  
  length(na_cells)
  length(nn_ids)
  
  x[na_cells] <- x[x[]!=-999][nn_ids]
  
  return(x)
}

r_channelheight_nir <- replace_NA_nearest_n0(r_channelheight_nir)
r_channelheight_eng <- replace_NA_nearest_n0(r_channelheight_eng)
r_channelheight_wal <- replace_NA_nearest_n0(r_channelheight_wal)
r_channelheight_sct <- replace_NA_nearest_n0(r_channelheight_sct)

r_channelheight_nir[r_channelheight_nir == -999] <- NA
r_channelheight_eng[r_channelheight_eng == -999] <- NA
r_channelheight_wal[r_channelheight_wal == -999] <- NA
r_channelheight_sct[r_channelheight_sct == -999] <- NA

# Identify fen/bog
r_fenbog_nir <- reclassify(r_channelheight_nir, matrix(c(-Inf, 1, 8, 1, Inf, 11), ncol = 3, byrow = TRUE))
r_fenbog_eng <- reclassify(r_channelheight_eng, matrix(c(-Inf, 1, 8, 1, Inf, 11), ncol = 3, byrow = TRUE))
r_fenbog_wal <- reclassify(r_channelheight_wal, matrix(c(-Inf, 1, 8, 1, Inf, 11), ncol = 3, byrow = TRUE))
r_fenbog_sct <- reclassify(r_channelheight_sct, matrix(c(-Inf, 1, 8, 1, Inf, 11), ncol = 3, byrow = TRUE))


# Save
writeRaster(r_channelheight_nir, file = "rasters/r_channelheight_nir.tif", overwrite = TRUE)
writeRaster(r_channelheight_eng, file = "rasters/r_channelheight_eng.tif", overwrite = TRUE)
writeRaster(r_channelheight_wal, file = "rasters/r_channelheight_wal.tif", overwrite = TRUE)
writeRaster(r_channelheight_sct, file = "rasters/r_channelheight_sct.tif", overwrite = TRUE)
writeRaster(r_fenbog_nir, file = "rasters/r_fenbog_nir.tif", overwrite = TRUE)
writeRaster(r_fenbog_eng, file = "rasters/r_fenbog_eng.tif", overwrite = TRUE)
writeRaster(r_fenbog_wal, file = "rasters/r_fenbog_wal.tif", overwrite = TRUE)
writeRaster(r_fenbog_sct, file = "rasters/r_fenbog_sct.tif", overwrite = TRUE)



## GRASS/SOIL -------------------------------
# Read NUTS1 shapefile
sf_nuts1 <- st_read("data/nuts1/NUTS_Level_1_January_2018_Full_Extent_Boundaries_in_the_United_Kingdom.shp") %>% 
  select(nuts118nm) %>% 
  rename("nuts1" = nuts118nm) %>% 
  st_transform(as.character(bng)) 

# Read HWSD raster - from RSPB G Drive. login : https://remote.rspb.org.uk/
r_soil <- raster("soil/HWSD_raster.tif") 

# Crop HWSD to NUTS1 extent
r_soil <- r_soil %>% crop(as_Spatial(st_transform(st_buffer(st_as_sfc(st_bbox(sf_nuts1)), dist = 10000), as.character(crs(r_soil)))))

# Load lookup table
lookup_soil <- read_csv("data/soil/lookup_soil.csv", guess_max = 100000) %>% 
  # select(MU_GLOBAL) %>%
  filter(MU_GLOBAL %in% unique(r_soil))
  

# Reproject tp 25m soil
r_soil_nir <- projectRaster(r_soil, r_lcm_nir, method = "ngb")
r_soil_eng <- projectRaster(r_soil, r_lcm_eng, method = "ngb")
r_soil_wal <- projectRaster(r_soil, r_lcm_wal, method = "ngb")
r_soil_sct <- projectRaster(r_soil, r_lcm_sct, method = "ngb")

# If LCM <- NA
r_soil_nir[is.na(v_lcm_nir)] <- NA
r_soil_eng[is.na(v_lcm_eng)] <- NA
r_soil_wal[is.na(v_lcm_wal)] <- NA
r_soil_sct[is.na(v_lcm_sct)] <- NA

# If soil ID <= 6 <- NA
r_soil_nir[r_soil_nir <= 6] <- NA
r_soil_eng[r_soil_eng <= 6] <- NA
r_soil_wal[r_soil_wal <= 6] <- NA
r_soil_sct[r_soil_sct <= 6] <- NA


# Crosstabulate LCM/soil areas
df_soil_lcm <- bind_rows(crosstabDT(r_lcm_nir, r_soil_nir, useNA = TRUE, long = TRUE),
                         crosstabDT(r_lcm_eng, r_soil_eng, useNA = TRUE, long = TRUE),
                         crosstabDT(r_lcm_wal, r_soil_wal, useNA = TRUE, long = TRUE),
                         crosstabDT(r_lcm_sct, r_soil_sct, useNA = TRUE, long = TRUE))

# Calculate proportion of each grassland within each soil type
df_grasssoil <- as.tbl(as.data.frame(df_soil_lcm)) %>% 
  set_names(c('lcm', 'soil', 'n')) %>% 
  filter(!is.na(lcm) & !is.na(soil) & soil > 0) %>% 
  # Seminatural grasslands
  filter(lcm %in% c(5, 6, 7)) %>% 
  group_by(lcm, soil) %>%
  summarise(n = sum(n)) %>%
  group_by(soil) %>% 
  mutate(prob = n / sum(n)) %>%
  # Select most likely grassland type
  filter(prob == max(prob)) %>% 
  ungroup()


# Substitute soil type with 4x grassland probabilities
isFALSE <- function(x)x==F
r_grasssoil_nir <- subsDT(r_soil_nir, df_grasssoil, by = 2, which = 1)
r_grasssoil_eng <- subsDT(r_soil_eng, df_grasssoil, by = 2, which = 1)
r_grasssoil_wal <- subsDT(r_soil_wal, df_grasssoil, by = 2, which = 1)
r_grasssoil_sct <- subsDT(r_soil_sct, df_grasssoil, by = 2, which = 1)

# Replace NA with nearest non 0
r_grasssoil_nir[is.na(r_grasssoil_nir)] <- 0
r_grasssoil_eng[is.na(r_grasssoil_eng)] <- 0
r_grasssoil_wal[is.na(r_grasssoil_wal)] <- 0
r_grasssoil_sct[is.na(r_grasssoil_sct)] <- 0

r_grasssoil_nir[!is.na(v_lcm_nir) & r_grasssoil_nir[] == 0] <- NA
r_grasssoil_eng[!is.na(v_lcm_eng) & r_grasssoil_eng[] == 0] <- NA
r_grasssoil_wal[!is.na(v_lcm_wal) & r_grasssoil_wal[] == 0] <- NA
r_grasssoil_sct[!is.na(v_lcm_sct) & r_grasssoil_sct[] == 0] <- NA

# Function for replacing nearest NA value with nearest non-0 value
replace_NA_nearest_n0 <- function(x){
  # edge <- boundaries(x, type = "inner", classes = FALSE)
  na_cells <- which(is.na(x[]))
  n0_cells <- which(x[]!= 0)
  
  nas_xy <- cbind(xyFromCell(x, na_cells), val = NA)
  n0s_xy <- cbind(xyFromCell(x, n0_cells), val = values(x)[n0_cells])
  
  n0s_tree <- createTree(coordinates(n0s_xy[, c(1, 2)]))
  nn_ids <- knnLookup(n0s_tree, newdat = coordinates(nas_xy[, c(1, 2)]), k = 1)
  
  length(na_cells)
  length(nn_ids)
  
  x[na_cells] <- x[x[]!=0][nn_ids]
  
  return(x)
}
r_grasssoil_nir <- replace_NA_nearest_n0(r_grasssoil_nir)
r_grasssoil_eng <- replace_NA_nearest_n0(r_grasssoil_eng)
r_grasssoil_wal <- replace_NA_nearest_n0(r_grasssoil_wal)
r_grasssoil_sct <- replace_NA_nearest_n0(r_grasssoil_sct)

r_grasssoil_nir[is.na(v_lcm_nir)] <- NA
r_grasssoil_eng[is.na(v_lcm_eng)] <- NA
r_grasssoil_wal[is.na(v_lcm_wal)] <- NA
r_grasssoil_sct[is.na(v_lcm_sct)] <- NA


# Save soil rasters
writeRaster(r_grasssoil_nir, file = "rasters/r_grasssoil_nir.tif", overwrite = TRUE)
writeRaster(r_grasssoil_eng, file = "rasters/r_grasssoil_eng.tif", overwrite = TRUE)
writeRaster(r_grasssoil_wal, file = "rasters/r_grasssoil_wal.tif", overwrite = TRUE)
writeRaster(r_grasssoil_sct, file = "rasters/r_grasssoil_sct.tif", overwrite = TRUE)





## YIELD CLASS FROM ESC -------------------------------
# Species, in order, are: AH (ash), BE (beech), BPO (black poplar), DF (Douglas fir), PBI (downy birch), SBI (silver birch), SOK (sessile oak), SP (Scots pine), SS (Sitka spruce), SY (sycamore) 

## Sample coordinates from climate strata
# NUTS1 shapefile 
sf_nuts1 <- st_read("data/nuts1/NUTS_Level_1_January_2018_Full_Extent_Boundaries_in_the_United_Kingdom.shp") %>% 
  select(nuts118nm) %>% 
  rename("nuts1" = nuts118nm) %>% 
  st_transform(as.character(bng))

# Avg monthly temperature
r_temp <- stack(as.list(list.files("worldclim/tavg", full.names = TRUE)))

# Total annual precip
r_prec <- stack(as.list(list.files("worldclim/prec", full.names = TRUE)))

# Crop, mask and mean
r_temp <- r_temp %>% 
  crop(as_Spatial(st_transform(st_buffer(st_as_sfc(st_bbox(sf_nuts1)), dist = 10000), as.character(crs(r_temp))))) %>% 
  mask(as_Spatial(st_transform(st_buffer(sf_nuts1, dist = 10000), as.character(crs(r_temp))))) %>% 
  mean() # Average of monthly means
r_prec <- r_prec %>% 
  crop(as_Spatial(st_transform(st_buffer(st_as_sfc(st_bbox(sf_nuts1)), dist = 10000), as.character(crs(r_prec))))) %>% 
  mask(as_Spatial(st_transform(st_buffer(sf_nuts1, dist = 10000), as.character(crs(r_prec))))) %>% 
  sum() # Sum of monthly means

# Round temp to nearest C, truncate coldest and hottest categories
r_temp <- round(r_temp) 
r_temp[r_temp <= 4] <- 4
r_temp[r_temp >= 12] <- 11

# Get upr/lwr 50% precip for each temp class
r_precbin <- r_prec
r_precbin[] <- bind_cols(r_temp %>% 
                           as.data.frame() %>% 
                           as.tbl(),
                         r_prec %>% 
                           as.data.frame() %>% 
                           as.tbl()) %>% 
  set_names(c("temp", "prec")) %>% 
  group_by(temp) %>%
  mutate(prec_bin = case_when(prec < quantile(prec, 1/3, na.rm = TRUE) ~ 0L,
                              prec >= quantile(prec, 1/3, na.rm = TRUE) & prec < quantile(prec, 2/3, na.rm = TRUE) ~ 1L,
                              prec >= quantile(prec, 2/3, na.rm = TRUE) ~ 2L,
                              TRUE ~ NA_integer_)) %>% 
  ungroup() %$% prec_bin 

# Combine to get UNIQUE temp/prec categories
r_climate <- (r_temp - 4) * 3 + r_precbin
plot(r_climate)
length(unique(r_climate))
freq(r_climate)

# Project to BNG (nearest neighbour)
r_climate_bng <- projectRaster(r_climate, crs = bng, method = "ngb")


# 20 samples per climate strata
set.seed(1)
sample_20 <- sampleStratified(r_climate_bng, size = 20, xy = TRUE) %>% 
  as.data.frame() %>% 
  as.tbl() %>% 
  select(x, y, layer) %>% 
  mutate(x = round(x),
         y = round(y)) %>% 
  rename("climate" = layer) 

# Write CSVs
for(i in 1:n_distinct(sample_20$climate)){
  climate <- unique(sample_20$climate)[i]
  sample_20 %>% 
    filter(climate == unique(climate)[i]) %>% 
    select(-climate) %>% 
    crossing(species = c("SS", "DF", "SP", "SBI", "SOK")) %>% 
    write_csv(paste0("data_out/esc/esc_climate", climate, ".csv"))
  sample_20 %>% 
    filter(climate == unique(climate)[i]) %>% 
    select(-climate) %>% 
    crossing(species = c("BE", "SY", "PBI", "AH", "BPO")) %>% 
    write_csv(paste0("data_out/esc/esc2_climate", climate, ".csv"))
}

# Go to http://www.forestdss.org.uk/geoforestdss/ & "Download ESC site data" for each csv file


## Import from ESC
# Create empty list
climate_yc1 <- as.list(rep(NA, 24))
climate_yc2 <- as.list(rep(NA, 24))

# Loop through CSVs
for(i in 1:length(climate_yc1)){
  climate_yc1[[i]] <- read.csv(paste0("data/woodland/esc/esc_climate", i-1, ".csv")) %>%
    as.tbl() %>%
    mutate(climate = i-1)
}
for(i in 1:length(climate_yc2)){
  climate_yc2[[i]] <- read.csv(paste0("data/woodland/esc/esc2_climate", i-1, ".csv")) %>%
    as.tbl() %>%
    mutate(climate = i-1)
}

# Bind rows 
climate_yc <- bind_rows(climate_yc1, 
                        climate_yc2) 


## New species
files <- list.files("data_out/esc", pattern = "esc2", full.names = TRUE)
new_species <- as.list(rep(NA, length(files)))
for(i in 1:length(files)){
  new_species[[i]] <- read.csv(files[i])
}

new_species <- bind_rows(new_species) %>% 
  as.tbl() %>% 
  select(-species) %>% 
  unique() %>% 
  crossing(species = c("WH", "RC"))

# write new spp
for(i in 1:ceiling(nrow(new_species)/100)){
  write_csv(filter(new_species[((i-1)*100+1):(i*100),], !is.na(species)), paste0("data_out/esc/esc3_climate", i, ".csv"))
}

# Read new spp
climate_yc3 <- as.list(rep(NA, ceiling(nrow(new_species)/100)))
for(i in 1:ceiling(nrow(new_species)/100)){
  climate_yc3[[i]] <- read.csv(paste0("data/woodland/esc/esc3_climate", i, ".csv")) %>%
    as.tbl() 
  }

climate_yc <- bind_rows(climate_yc3) %>% 
  left_join(climate_yc %>% 
              select(x, y, climate) %>% 
              unique(),
            by = c("x", "y")) %>% 
  bind_rows(climate_yc)


climate_yc %>%
  filter(!is.na(cyc)) %>%
  filter(species == "RC") %>%
  lm(cyc ~ as.factor(climate), data = .) %>%
  summary()
  # plot()

# Calculate species-strata mean CYC (climatic yield class)
mean_cyc <- climate_yc %>% 
  filter(!is.na(cyc)) %>% 
  group_by(climate, species) %>% 
  summarise(cyc = mean(cyc)) %>%  
  ungroup() %>% 
  spread(species, cyc)


# Map to raster
r_cyc <- subs(r_climate_bng, mean_cyc, by = 1, which = c(2:13))


## Fasterize to each country
# 10km buffer around NUTS1 regions to ensure all coastal LCM pixels captured
r_nuts1_10k <- sf_nuts1 %>% 
  summarise() %>% 
  st_buffer(dist = 10000) %>% 
  fasterize(., r_cyc[[1]])

# Function for replacing nearest NA value with nearest non-0 value
replace_NA_nearest_n0 <- function(x){
  # edge <- boundaries(x, type = "inner", classes = FALSE)
  na_cells <- which(is.na(x[]))
  n0_cells <- which(x[]!= 0)
  
  nas_xy <- cbind(xyFromCell(x, na_cells), val = NA)
  n0s_xy <- cbind(xyFromCell(x, n0_cells), val = values(x)[n0_cells])
  
  n0s_tree <- createTree(coordinates(n0s_xy[, c(1, 2)]))
  nn_ids <- knnLookup(n0s_tree, newdat = coordinates(nas_xy[, c(1, 2)]), k = 1)
  
  length(na_cells)
  length(nn_ids)
  
  x[na_cells] <- x[x[]!=0][nn_ids]
  
  return(x)
}

# Replace coastal NAs with nearest non-0
for(i in 1:nlayers(r_cyc)){
  r_cyc[[i]][is.na(r_nuts1_10k)] <- 0
  r_cyc[[i]] <- replace_NA_nearest_n0(r_cyc[[i]])
  
}

# LCM rasters
r_lcm_nir <- raster("rasters/r_lcm_new_nir.tif")
r_lcm_eng <- raster("rasters/r_lcm_new_eng.tif")
r_lcm_sct <- raster("rasters/r_lcm_new_sct.tif")
r_lcm_wal <- raster("rasters/r_lcm_new_wal.tif")

# Reproject to each country
r_cyc_nir <- projectRaster(r_cyc, r_lcm_nir, method = "ngb")
r_cyc_eng <- projectRaster(r_cyc, r_lcm_eng, method = "ngb")
r_cyc_sct <- projectRaster(r_cyc, r_lcm_sct, method = "ngb")
r_cyc_wal <- projectRaster(r_cyc, r_lcm_wal, method = "ngb")

# Get LCM values
v_lcm_nir <- r_lcm_nir[]
v_lcm_eng <- r_lcm_eng[]
v_lcm_sct <- r_lcm_sct[]
v_lcm_wal <- r_lcm_wal[]

# If is.na(lcm) <- NA
for(i in 1:nlayers(r_cyc)){
  r_cyc_nir[[i]][is.na(v_lcm_nir)] <- NA
  r_cyc_eng[[i]][is.na(v_lcm_eng)] <- NA
  r_cyc_sct[[i]][is.na(v_lcm_sct)] <- NA
  r_cyc_wal[[i]][is.na(v_lcm_wal)] <- NA
}

# Save
writeRaster(r_cyc_nir, "rasters/r_cyc_nir.tif", overwrite = TRUE)
writeRaster(r_cyc_eng, "rasters/r_cyc_eng.tif", overwrite = TRUE)
writeRaster(r_cyc_sct, "rasters/r_cyc_sct.tif", overwrite = TRUE)
writeRaster(r_cyc_wal, "rasters/r_cyc_wal.tif", overwrite = TRUE)

## ALTITUDE -------------------------
# Read elevation & precip rasters
r_elev <- raster("dtm_channels/usgs_filled_dtm.tif")

# Replace NA elev with 0 (i.e coast)
r_elev[is.na(r_elev)] <- 0

# Project to 4 country rasters 
r_elev_nir <- projectRaster(r_elev, r_lcm_nir)
r_elev_eng <- projectRaster(r_elev, r_lcm_eng)
r_elev_sct <- projectRaster(r_elev, r_lcm_sct)
r_elev_wal <- projectRaster(r_elev, r_lcm_wal)

r_elev_nir[is.na(r_lcm_nir)] <- NA
r_elev_eng[is.na(r_lcm_eng)] <- NA
r_elev_sct[is.na(r_lcm_sct)] <- NA
r_elev_wal[is.na(r_lcm_wal)] <- NA

r_elev_nir[is.na(r_elev_nir) & !is.na(r_lcm_nir)] <- 0
r_elev_eng[is.na(r_elev_eng) & !is.na(r_lcm_eng)] <- 0
r_elev_sct[is.na(r_elev_sct) & !is.na(r_lcm_sct)] <- 0
r_elev_wal[is.na(r_elev_wal) & !is.na(r_lcm_wal)] <- 0

# Save
writeRaster(r_elev_nir, file = "rasters/r_elev_nir.tif", overwrite = TRUE)
writeRaster(r_elev_eng, file = "rasters/r_elev_eng.tif", overwrite = TRUE)
writeRaster(r_elev_wal, file = "rasters/r_elev_wal.tif", overwrite = TRUE)
writeRaster(r_elev_sct, file = "rasters/r_elev_sct.tif", overwrite = TRUE)



# ISLAND / MAINLAND --------------------
# Read island/mainland shapefile (created manually in QGIS from NUTS1 file, using Multi-Singlepart tool)
sf_islands <- st_read("data/nuts1/islands.shp") %>% 
  st_transform(bng)

# Rasterize
r_island_nir <- fasterize(st_transform(sf_islands, crs(r_lcm_nir)), r_lcm_nir, field = "island")
r_island_eng <- fasterize(sf_islands, r_lcm_eng, field = "island")
r_island_sct <- fasterize(sf_islands, r_lcm_sct, field = "island")
r_island_wal <- fasterize(sf_islands, r_lcm_wal, field = "island")

# + 1 so island = 2, mainland = 1
r_island_nir <- r_island_nir + 1
r_island_eng <- r_island_eng + 1
r_island_sct <- r_island_sct + 1
r_island_wal <- r_island_wal + 1

# If LCM is NA <- 1 (for now - NA later)
r_island_nir[is.na(r_lcm_nir)] <- 0
r_island_eng[is.na(r_lcm_eng)] <- 0
r_island_sct[is.na(r_lcm_sct)] <- 0
r_island_wal[is.na(r_lcm_wal)] <- 0

# Function for replacing nearest NA value with nearest non-0 value
replace_NA_nearest_n0 <- function(x){
  # edge <- boundaries(x, type = "inner", classes = FALSE)
  na_cells <- which(is.na(x[]))
  n0_cells <- which(x[]!= 0)
  
  nas_xy <- cbind(xyFromCell(x, na_cells), val = NA)
  n0s_xy <- cbind(xyFromCell(x, n0_cells), val = values(x)[n0_cells])
  
  n0s_tree <- createTree(coordinates(n0s_xy[, c(1, 2)]))
  nn_ids <- knnLookup(n0s_tree, newdat = coordinates(nas_xy[, c(1, 2)]), k = 1)
  
  length(na_cells)
  length(nn_ids)
  
  x[na_cells] <- x[x[]!=0][nn_ids]
  
  return(x)
}

# Replace NA value with nearest non-0
r_island_nir <- replace_NA_nearest_n0(r_island_nir)
r_island_eng <- replace_NA_nearest_n0(r_island_eng)
r_island_sct <- replace_NA_nearest_n0(r_island_sct)
r_island_wal <- replace_NA_nearest_n0(r_island_wal)

# If LCM is NA <- NA
r_island_nir[is.na(r_lcm_nir)] <- NA
r_island_eng[is.na(r_lcm_eng)] <- NA
r_island_sct[is.na(r_lcm_sct)] <- NA
r_island_wal[is.na(r_lcm_wal)] <- NA

# - 1 so island = 1, mainland = 0
r_island_nir <- r_island_nir - 1
r_island_eng <- r_island_eng - 1
r_island_sct <- r_island_sct - 1
r_island_wal <- r_island_wal - 1

# Save
writeRaster(r_island_nir, file = "rasters/r_island_nir.tif", overwrite = TRUE)
writeRaster(r_island_eng, file = "rasters/r_island_eng.tif", overwrite = TRUE)
writeRaster(r_island_wal, file = "rasters/r_island_wal.tif", overwrite = TRUE)
writeRaster(r_island_sct, file = "rasters/r_island_sct.tif", overwrite = TRUE)