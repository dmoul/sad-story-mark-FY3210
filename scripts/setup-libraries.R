# setup-libraries.R

library(here)
library(tidyverse)
library(lubridate)
library(scales)
library(units)
library(janitor)
library(glue)
library(sf)
library(terra)
library(tidyterra)
library(elevatr)    # elevation raster via get_elev_raster()
# library(geodata)    # elevation raster via elevation_30s()
library(tidycensus)
library(tigris)     # county boundaries
# library(nlme)
library(stars)
library(patchwork)
library(osmdata)    # open street maps; might be faster to use osmextract::oe_get() https://docs.ropensci.org/osmextract/
library(gt)
library(ggbump)     # geom_bump()
library(ggmosaic)   # geom_mosaic()
library(broman)     # spell_out()

options(tigris_use_cache = TRUE)

theme_set(theme_light() + 
            theme(panel.border = element_blank(),
                  panel.grid = element_blank(),
                  plot.title = element_text(size = rel(2.0),
                                            face = "bold"),
                  plot.title.position = "plot"
            )
)
