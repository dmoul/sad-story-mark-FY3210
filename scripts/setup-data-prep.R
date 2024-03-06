# setup-data-prep.R

my_caption <- "Daniel Moul. Survey mark data: ngs.noaa.gov"

prj <- "epsg:4269" # NAD83

my_counties <- c("Orange", "Chatham", "Durham", "Wake")
n_counties <- length(my_counties)


###### prerequisite directories and shapefiles

if(!dir.exists(here("data"))) dir.create("data")
if(!dir.exists(here("data/raw"))) dir.create("data/raw")
if(!dir.exists(here("data/processed"))) dir.create("data/processed")

if(!file.exists(here("data/raw/shapefiles/NC/NC.shp"))) {
  if(!file.exists(here("data/raw/shapefiles/NC.zip"))) {
    stop("Missing data/raw/shapefiles/NC.zip")
  } else {
    my_dir <- here("data/raw/shapefiles/")
    system(paste0("cd ", my_dir, 
                  " && mkdir ./NC", 
                  " && cd ./NC", 
                  " && unzip ../NC.zip")
           )
  }
}

if(!file.exists(here("data/raw/shapefiles/Major_Hydrography_Streams_Rivers/Major_Hydrography_(Streams_Rivers).shp"))) {
  if(!file.exists(here("data/raw/shapefiles/Major_Hydrography_Streams_Rivers.zip"))) {
    stop("Missing data/raw/shapefiles/Major_Hydrography_Streams_Rivers.zip")
  } else {
    my_dir <- here("data/raw/shapefiles/")
    system(paste0("cd ", my_dir, 
                  " && mkdir ./Major_Hydrography_Streams_Rivers", 
                  " && cd ./Major_Hydrography_Streams_Rivers", 
                  " && unzip ../Major_Hydrography_Streams_Rivers.zip"))
  }
}

if(!file.exists(here("data/raw/shapefiles/Major_Hydrography_Waterbodies/Major_Hydrography_(Waterbodies).shp"))) {
  if(!file.exists(here("data/raw/shapefiles/Major_Hydrography_Waterbodies.zip"))) {
    stop("Missing data/raw/shapefiles/Major_Hydrography_Waterbodies.zip")
  } else {
    my_dir <- here("data/raw/shapefiles/")
    system(paste0("cd ", my_dir, 
                  " && mkdir ./Major_Hydrography_Waterbodies", 
                  " && cd ./Major_Hydrography_Waterbodies", 
                  " && unzip ../Major_Hydrography_Waterbodies"))
  }
}

###### county borders (and some related data) from US Census TIGRIS database

# census_api_key("PUT YOUR KEY HERE") # run this one time before using tidycensus for the first time
nc_counties_all_tigris <- tigris::counties("NC") |> # aland and awater in m^2
  clean_names()

# v2020 <- load_variables(2022, "acs5", cache = TRUE)

nc_county_all_pop2022 <- get_acs(geography = "county", 
                   state = "37",
                   variables = "B01003_001", 
                   year = 2022,
                   geometry = TRUE,
                   cache_table = TRUE) |>
  clean_names() |>
  mutate(county = str_extract(name, "^.+(?= County)")) |>
  select(geoid, county, variable, pop = estimate, geometry) |>
  inner_join(nc_counties_all_tigris |>
               st_drop_geometry() |>
               select(geoid, county = name, aland, awater),
             by = join_by(geoid, county)
             )

nc_counties <- nc_county_all_pop2022 %>%
  filter(county %in% my_counties)

nc_counties_border_union <- st_union(nc_counties)


###### survey mark data from NGS
last_cond_levels <- rev(c("MONUMENTED", "GOOD", "POOR", "MARK NOT FOUND", "SEE DESCRIPTION"))
# Note: The small number of "FIRST OBSERVED" values was used only in the 1980s and seems to be 
#       synonymous with "MONUMENTED". So I combine them into "MONUMENTED" below.
stability_levels <- rev(c("A", "B", "C", "D", "UNK"))

nc_marks_all <- st_read(here("data/raw/shapefiles/NC/")) |>
  remove_empty(which = "cols") |>
  st_transform(crs = prj) |>
  clean_names() |>
  mutate(county = str_to_title(county),
         across(c(ends_with("_ht"), starts_with(c("n_", "dec_", "_epoch", "ecef")), 
                  first_recv, model_grav), 
                as.numeric),
         first_recv = ifelse(str_length(first_recv) == 4, # year only?
                            paste0(first_recv, "0101"),
                            first_recv),
         first_recv = ymd(first_recv, quiet = TRUE),
         last_recv = ifelse(str_length(last_recv) == 4, # year only?
                            paste0(last_recv, "0101"),
                            last_recv),
         last_recv = ymd(last_recv, quiet = TRUE),
         ortho_ht = ifelse(pid != "EZ2890", # bad data point
                           ortho_ht,
                           231),
         last_cond = if_else(last_cond == "FIRST OBSERVED",
                             "MONUMENTED", last_cond),
         last_cond = factor(last_cond, levels = last_cond_levels),
         stability = factor(stability, levels = stability_levels),
         sat_use = factor(sat_use, levels = c("Y", "N", "UNK"))
  ) |>
  select(-c(ht_mod, cors_id, pacs_sacs, starts_with("n_acc_"), pos_order, vert_order, vert_class, ecef_x, ecef_y, ecef_z,
            starts_with(c("spc_", "utm_")))) |> # move the following above the select once it works
  mutate(first_placed = as.numeric(str_extract(stamping, "((18)|(19)|(20))([0-9]){2,2}$|(?<=/)[9][0-9]")),
         first_placed = if_else(first_placed < 100, # only two digits
                                first_placed + 1900,
                                first_placed),
         first_placed = if_else(between(first_placed, 1800, 2023),
                                        first_placed,
                                        NA)
         ) |>
  mutate(first_check = first_placed - year(first_recv))

nc_marks_counties <- nc_marks_all |>
  filter(county %in% my_counties) |>
  filter(ortho_ht > 0,
         !is.na(last_recv),
         !is.na(last_recby),
         last_recby != "NONE")

n_marks_counties <- nrow(nc_marks_counties)
n_marks_placed_recoverd_same_year <- nc_marks_counties |>
  filter(first_check == 0) |>
  nrow()
pct_placed_recoverd_same_year <- n_marks_placed_recoverd_same_year / n_marks_counties

###### elevation color scale from tidyterra package

# from help for scale_fill_hypso_c()
data("hypsometric_tints_db")

pal_dem_screen <- hypsometric_tints_db |>
  filter(pal == "dem_screen")

my_range <- c(40, 350) # good for four counties in NC
my_values <- scales::rescale(pal_dem_screen$limit, range = my_range)

###### elevation data from elevatr::get_elev_raster() 
# gets data from the [Amazon Web Services Terrain Tiles](https://registry.opendata.aws/terrain-tiles/) 
# and the [Open Topography global datasets API](https://opentopography.org/developers/).

fname <- here("data/processed/elev_nc_counties.GTiff")

if(!file.exists(fname)) {
  elevation <- get_elev_raster(nc_counties, z = 9)
  
  elev_nc_counties <-  mask(crop(rast(elevation), nc_counties_border_union),
                            as_spatvector(nc_counties_border_union)
  )
  
  terra::writeRaster(elev_nc_counties, fname,
                     overwrite = TRUE,
                     filetype = "GTiff")
  
} else {
  
  elev_nc_counties <- terra::rast(fname)
  
}

names(elev_nc_counties) <- "height"


### Water features from NC OneMap

nc_rivers <- st_read(here("data/raw/shapefiles/Major_Hydrography_Streams_Rivers/")) |>
  st_transform(crs = prj) |>
  st_join(nc_counties,
          left = FALSE)

nc_waterbodies <- st_read(here("data/raw/shapefiles/Major_Hydrography_Waterbodies/")) |>
  st_transform(crs = prj) |>
  st_join(nc_counties,
          left = FALSE)


### Streets from Open Street Map

#| label: get-osm-streets

fname <- "./data/processed/osm_nc_counties.rds"

if(!file.exists(fname)) {
  
  my_bbox <- st_bbox(nc_counties_border_union)
  
  streets <- my_bbox |>
    opq(timeout = 50) %>% # https://github.com/ropensci/osmdata/issues/200
    add_osm_feature(key = "highway", 
                    value = c("motorway", "trunk", "primary", 
                              "secondary", "tertiary", "road")) %>%
    osmdata_sf()
  
  write_rds(streets, fname)
  
} else {
  
  streets <- read_rds(fname)
  
}

### Contributors who recovered marks

# Contributor abbreviations and organization names are listed at
# https://www.ngs.noaa.gov/cgi-bin/get_contrib2.prl

contrib_all <- read_fwf("./data/raw/contributors-2024-02-03.txt",
                        comment = "#",
                        fwf_widths(c(8, NA), c("org_abbr", "organization"))
)

contrib <- nc_marks_counties |>
  st_drop_geometry() |>
  count(last_recby) |>
  filter(last_recby != "NONE") |>
  inner_join(
    contrib_all,
    join_by(last_recby == org_abbr)
  ) |>
  rename(last_recby_org = organization) |>
  arrange(last_recby)

n_org <- nrow(contrib)


### Local gravity including elevation above sea level

r_e_m = 6371.009 * 1000 # radius of earth in meters
# g_alt = 9.80665 * (r_e_m / (r_e_m + 500))^2
# g_alt_mGal = g_alt / .00001

# International Gravity Formula (IGF)
# local_g <- 9.780327 * (1 + 0.0053024 * sin(lat * pi / 180))^2 - 0.0000058 * sin(2 * lat)^2 - 3.0863^(-6) * elevation
# then divide by 10e-6 to convert units to mGal

data_for_plot_gravity <- nc_marks_counties |>
  select(ortho_ht, dec_lat, model_grav, county) |>
  mutate(gravity_theoretical_simple = 9.80665 * (r_e_m / (r_e_m + ortho_ht))^2 / 0.00001,
         gravity_delta_simple = model_grav - gravity_theoretical_simple,
         gravity_pct_delta_simple = model_grav / gravity_theoretical_simple,
         # gravity_theoretical_igf = (9.780327 * (1 + 0.0053024 * sin(dec_lat * pi / 180))^2 - 0.0000058 * sin(2 * dec_lat)^2 - 3.0863^(-6) * ortho_ht) / 10e-6,
         # gravity_delta_igf = model_grav - gravity_theoretical_igf,
         # gravity_pct_delta_igf = model_grav / gravity_theoretical_igf,
         gravity_theoretical_welmec = (9.780327 * (1 + 0.0053024 * sin(dec_lat * pi / 180)^2 - 0.0000058 * sin(2 * dec_lat)^2) - 0.000003085 * ortho_ht),
         gravity_theoretical_welmec_mgal = gravity_theoretical_welmec / 1.0e-5,
         gravity_delta_theoretical_welmec_mgal = model_grav - gravity_theoretical_welmec_mgal,
         gravity_pct_deltal_welmec_mgal = model_grav / gravity_theoretical_welmec_mgal
         )

gravity_delta_theoretical_welmec_0_90 <- tibble(
  lat_degree = 0:90,
  grav = 9.780327 * (1 + 0.0053024 * sin(lat_degree * pi / 180)^2 - 0.0000058 * sin(2 * lat_degree)^2) # at sea level, in ms^-2
)

dta_gravity_theoretical_simple <- data_for_plot_gravity |>
  filter(ortho_ht > 0) |>
  filter(ortho_ht == max(ortho_ht, na.rm = TRUE) | ortho_ht == min(ortho_ht, na.rm = TRUE)) |>
  distinct(ortho_ht, .keep_all = TRUE) |>
  rename(model_grav_orig = model_grav,
         model_grav = gravity_theoretical_simple) # to simplify plotting them together

# dta_gravity_theoretical_igf <- data_for_plot_gravity |>
#   filter(ortho_ht > 0) |>
#   filter(ortho_ht == max(ortho_ht, na.rm = TRUE) | ortho_ht == min(ortho_ht, na.rm = TRUE)) |>
#   distinct(ortho_ht, .keep_all = TRUE) |>
#   rename(model_grav_orig = model_grav,
#          model_grav = gravity_theoretical_igf) # to simplify plotting them together

dta_gravity_theoretical_welmec <- data_for_plot_gravity |>
  filter(ortho_ht > 0) |>
  filter(ortho_ht == max(ortho_ht, na.rm = TRUE) | ortho_ht == min(ortho_ht, na.rm = TRUE)) |>
  distinct(ortho_ht, .keep_all = TRUE) |>
  rename(model_grav_orig = model_grav,
         model_grav = gravity_theoretical_welmec) # to simplify plotting them together

dta_gravity_theoretical_welmec_mgal <- dta_gravity_theoretical_welmec |>
  mutate(model_grav = model_grav / 1.0e-5)


###### gravity anomaly models

# Brouguer
# WGM2012_20240225024751.zip downloaded from https://bgi.obs-mip.fr/data-products/outils/wgm2012-maps-visualizationextraction/

grav_tmp <- read_fwf(here("data/map/WGM2012_20240225025821/plot_bouguer_20240225025821.txt"),
                     skip = 5)
colnames(grav_tmp) <- c("lon", "lat", "mgal")

bouguer <- st_as_sf(grav_tmp,
                    coords = c("lon", "lat"),
                    crs = "WGS84") |>
  st_transform(prj)

bouguer_nc_state <- st_intersection(bouguer,
                                    st_union(nc_counties_all_tigris))

bouguer_nc_counties <- st_intersection(bouguer, 
                                       st_buffer(nc_counties_border_union,
                                                 10000)
)

# Isostatic
# WGM2012_20240225031912.zip downloaded from https://bgi.obs-mip.fr/data-products/outils/wgm2012-maps-visualizationextraction/

grav_tmp <- read_fwf(here("data/map/WGM2012_20240225031912/plot_isostatic_20240225031912.txt"),
                     skip = 5)
colnames(grav_tmp) <- c("lon", "lat", "mgal")

isostatic <- st_as_sf(grav_tmp,
                      coords = c("lon", "lat"),
                      crs = "WGS84") |>
  st_transform(prj)

isostatic_nc_counties <- st_intersection(isostatic, 
                                         st_buffer(nc_counties_border_union,
                                                   10000)
)

# Free air
# WGM2012_20240225040612.zip downloaded from https://bgi.obs-mip.fr/data-products/outils/wgm2012-maps-visualizationextraction/

grav_tmp <- read_fwf(here("data/map/WGM2012_20240225040612/plot_freeair_20240225040612.txt"),
                     skip = 5)
colnames(grav_tmp) <- c("lon", "lat", "mgal")

free_air <- st_as_sf(grav_tmp,
                     coords = c("lon", "lat"),
                     crs = "WGS84") |>
  st_transform(prj)

free_air_nc_counties <- st_intersection(free_air, 
                                        st_buffer(nc_counties_border_union,
                                                  10000)
)

