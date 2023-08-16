## INITS -------------------------------
source("setup.R")
library(rasterDT)
isFALSE <- function(x){!x}


## SUSTAINABLE SHORES PRIORITY SITE RASTERS -----------------------------
r_saltmarsh_nir <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_nir.tif")
r_saltmarsh_eng <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_eng.tif")
r_saltmarsh_wal <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_wal.tif")
r_saltmarsh_sct <- raster("C:/Users/tomfinch/Documents/bigdata/rasters/r_saltmarsh_sct.tif")



## RESTORATION ORDER ---------------------------
# Contains lookup_saltmarsh, with priority/non-priority
load("rdata/raster_lookup_saltmarsh.RData")

# Rank according to priority, calculate cumulative area
set.seed(1)
df_saltmarshrank <- bind_rows(freqDT(r_saltmarsh_nir),
                              freqDT(r_saltmarsh_eng),
                              freqDT(r_saltmarsh_wal),
                              freqDT(r_saltmarsh_sct)) %>% 
  as.tbl() %>% 
  filter(!is.na(ID)) %>% 
  rename('site' = ID) %>% 
  group_by(site) %>% 
  summarise(area = sum(freq * 0.0625)) %>% 
  # Get names/priority status
  left_join(lookup_saltmarsh, by = c("site" = "saltmarsh_site")) %>%
  # Randomise order
  sample_frac(1) %>%
  # Then get priority sites top 
  arrange(-priority) %>% 
  # Calculate cumulative area
  mutate(cum_area = cumsum(area),
         rank = 1:length(cum_area)) %>% 
  select(-name)


# Map rank back to raster
r_saltmarshrank_nir <- subsDT(r_saltmarsh_nir, select(df_saltmarshrank, site, rank))
r_saltmarshrank_eng <- subsDT(r_saltmarsh_eng, select(df_saltmarshrank, site, rank))
r_saltmarshrank_wal <- subsDT(r_saltmarsh_wal, select(df_saltmarshrank, site, rank))
r_saltmarshrank_sct <- subsDT(r_saltmarsh_sct, select(df_saltmarshrank, site, rank))



## SAVE -----------------------------------
# Df
save(df_saltmarshrank, file = "rdata/intermediate/df_saltmarshrank.RData")

# Rasters
writeRaster(r_saltmarshrank_nir, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_saltmarshrank_nir.tif", overwrite = TRUE)
writeRaster(r_saltmarshrank_eng, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_saltmarshrank_eng.tif", overwrite = TRUE)
writeRaster(r_saltmarshrank_wal, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_saltmarshrank_wal.tif", overwrite = TRUE)
writeRaster(r_saltmarshrank_sct, "C:/Users/tomfinch/Documents/bigdata/rasters/intermediate/r_saltmarshrank_sct.tif", overwrite = TRUE)
