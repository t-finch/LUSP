# LOAD PACKAGES ############################
# install.packages("install.load")
library(install.load)

# Install / load all packages from CRAN
install_load(
          # Tidyvsere
          "tidyverse",
          "magrittr",
          
          # Plotting
          "cowplot",
          # "ggrepel",
          # "leaflet",
          "RColorBrewer",
          # "proto",
          # "gridExtra",
          "viridis",
          # "mapview",
          "ggspatial",
          
          # Spatial
          "raster",
          "rasterDT",
          "sp",
          "sf",
          "rgdal",
          "maptools",
          "mapproj",
          "geosphere",
          "rgeos",
          # "spdplyr",
          "fasterize",
          "SearchTrees",
          "exactextractr",
          
          # Stats, etc.
          "lme4",
          # "MuMIn",
          "MASS",
          # "vegan",
          # "flexclust",
          # "matrixStats",
          # "Hmisc",
          # "alabama",
          
          # Other
          "beepr",
          # "doParallel",
          # "foreach",
          # "furrr",
          # "tictoc",
          "mgcv"
          
          # Markdown, knitr
          # "rmarkdown",
          # "formatR",
          # "knitr"
)

rasterOptions(tmpdir=paste0("C:/Users/tomfinch/Temporary R/", round(runif(1, 0, 1000)), "/"))




# SET OPTIONS  ############################
options(scipen = 1000000)
windowsFonts(TNR = windowsFont("Times New Roman"),
             A = windowsFont("Arial"),
             C = windowsFont("Calibri"))


# DEFINE CONSTANTS  ############################
# Define coordinate reference systems
bng   = CRS('+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs')
# mrc   = CRS('+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs')
# wgs84 = CRS('+proj=longlat +datum=WGS84')


# CUSTOM GGPLOT THEME  ############################
theme_tom <- function(base_size = 8, base_family = "") {
          # Starts with theme_grey and then modify some parts
          theme_grey(base_size = base_size, base_family = base_family) %+replace%
                    theme(
                              # theme_bw() stuff:
                              legend.key        = element_rect(colour = "grey80"),
                              panel.background  = element_rect(fill = "white", colour = NA),
                              panel.border      = element_rect(fill = NA, colour = "grey50"),
                              # panel.grid.major  = element_line(colour = "grey90", size = 0.2),
                              # panel.grid.minor  = element_line(colour = "grey90", size = 0.5),
                              panel.grid.major  = element_blank(),
                              panel.grid.minor  = element_blank(),
                              strip.background  = element_rect(fill = "grey80", colour = "grey50", size = 0.2),
                              # Custom stuff
                              axis.text         = element_text(size = 8),
                              axis.ticks        = element_line(colour = "black", size = 0.2),
                              plot.title        = element_text(size = 12, hjust = 0, face = "bold", margin=margin(b = 3, unit = "pt")),
                              legend.text       = element_text(size = 7),
                              legend.key.size   = unit(3, "mm"),
                              strip.text        = element_text(size = 8),
                              legend.background = element_rect(colour = 1, fill = scales::alpha("white", 1), size = 0.25)
                    )
}

# Set as default
theme_set(theme_tom())

# ggplot legend position shortcut
gglegend.position <- function(op, ...){
          switch(op,
                 'topleft' = theme(legend.position = c(0,1), legend.justification = c(0,1), ...),
                 'topright' = theme(legend.position = c(1,1), legend.justification = c(1,1), ...),
                 'topmid' = theme(legend.position = c(0.5,1), legend.justification = c(0.5,1), ...),
                 'lowleft' = theme(legend.position = c(0,0), legend.justification = c(0,0), ...),
                 'lowright' = theme(legend.position = c(1,0), legend.justification = c(1,0), ...),
                 'lowmid' = theme(legend.position = c(0.5,0), legend.justification = c(0.5,0), ...),
                 'midmid' = theme(legend.position = c(0.5,0.5), legend.justification = c(0.5,0.5), ...))
          }


# DEFINE CUSTOM FUNCTIONS ############################
# To avoid clashes
select <- dplyr:::select
set_names <- magrittr::set_names

# geomean <- function(x, na.rm = T, trim = 0, ...) {
#           # x[x == 0] <- min(x[x != 0]) * 0.001
#           exp(mean(log(x, ...), na.rm = na.rm, trim = trim, ...))
# }

# To make rasterDT::subsDT work
isFALSE <- function(x){!x}

