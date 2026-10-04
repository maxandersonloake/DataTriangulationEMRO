# =============================================================================
# 2_2_PrepareBoundaries_EMRO.R
#
# One-off preparation of the EMRO-approved admin-1 boundary layers used by the
# Data visualisation tab's maps, from the raw shapefiles supplied by WHO EMRO
# (kept in Data/emro_boundaries_src/):
#
#   pak_admbnda_adm1_ocha_pco_gaul_20171122.shp  Pakistan provinces (6 features)
#   som_admin1_em.shp                            Somalia's 18 regions (EMRO version)
#   EMRO_Disputed_areas.shp                      disputed-area polygons (6)
#
# The supplied .shp files carry geometry only -- no .dbf attribute table and
# no .prj -- so (a) the CRS is assumed to be WGS84 lon/lat (the coordinates
# are plainly degrees), and (b) each feature is NAMED here by matching it
# against an existing, named boundary layer: whichever existing province /
# region it overlaps most is its name.
#
# Pakistan: the EMRO file has no Azad Kashmir or Gilgit Baltistan polygons
# (the EMRO convention treats that area as disputed -- it is covered by the
# disputed-areas layer instead), but the dashboard still reports data for
# both, so they are carried over from the existing Data/pak_admin_boundaries/
# pak_admin1.geojson, clipped so they don't overlap the new provinces. FATA is
# merged into Khyber Pakhtunkhwa (the two have been one province since 2018,
# and the dashboard's "KP" already means both).
#
# Outputs (committed; read at app start-up by app.R):
#   Data/pak_admin_boundaries/pak_admin1_emro.geojson   7 provinces (adm1_name)
#   Data/som_admin_boundaries/som_admin1_emro.geojson   18 regions  (adm1_name)
#   Data/emro_disputed_areas.geojson                    6 disputed polygons
#
# Run from the project root:  Rscript 2_2_PrepareBoundaries_EMRO.R
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})
sf_use_s2(FALSE)

# The supplied shapefiles have no .shx index (nor .dbf/.prj) alongside them;
# GDAL can rebuild the index on the fly, but only if told it may.
Sys.setenv(SHAPE_RESTORE_SHX = "YES")

SRC_DIR <- "Data/emro_boundaries_src"

read_shp <- function(name) {
  x <- sf::st_read(file.path(SRC_DIR, name), quiet = TRUE)
  sf::st_crs(x) <- 4326
  sf::st_make_valid(sf::st_zm(x))
}

# Name each feature of `new` by the `name_col` value of whichever feature of
# `ref` it overlaps most (by area).
name_by_overlap <- function(new, ref, name_col) {
  ref <- sf::st_make_valid(ref[, name_col])
  vapply(seq_len(nrow(new)), function(i) {
    ov <- suppressWarnings(sf::st_intersection(new[i, ], ref))
    if (nrow(ov) == 0) return(NA_character_)
    as.character(ov[[name_col]][which.max(as.numeric(sf::st_area(ov)))])
  }, character(1))
}

# ---- Pakistan -------------------------------------------------------------
pak_new <- read_shp("pak_admbnda_adm1_ocha_pco_gaul_20171122.shp")
pak_old <- sf::st_make_valid(sf::st_read("Data/pak_admin_boundaries/pak_admin1.geojson", quiet = TRUE))

pak_new$adm1_name <- name_by_overlap(pak_new, pak_old, "adm1_name")
message("Pakistan features named by overlap: ", paste(pak_new$adm1_name, collapse = ", "))
stopifnot(!anyNA(pak_new$adm1_name))

pak_new <- pak_new %>%
  group_by(adm1_name) %>%                     # merges FATA into Khyber Pakhtunkhwa
  summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  sf::st_make_valid()

# Azad Kashmir + Gilgit Baltistan carried over from the existing layer,
# minus anything the new provinces already cover.
new_union <- sf::st_union(pak_new)
carry <- pak_old[pak_old$adm1_name %in% c("Azad Kashmir", "Gilgit Baltistan"), "adm1_name"]
carry <- sf::st_make_valid(suppressWarnings(sf::st_difference(carry, new_union)))
carry <- carry[, "adm1_name"]
message("Carried-over Azad Kashmir / Gilgit Baltistan: ", paste(carry$adm1_name, collapse = ", "))

pak_out <- bind_rows(pak_new, carry) %>%
  arrange(adm1_name) %>%
  sf::st_make_valid()
stopifnot(nrow(pak_out) == 7, !anyDuplicated(pak_out$adm1_name))

# ---- Somalia --------------------------------------------------------------
som_new <- read_shp("som_admin1_em.shp")
som_ref <- sf::st_make_valid(sf::st_read("Data/som_admin_boundaries/som_admin1.geojson", quiet = TRUE))
som_new$adm1_name <- name_by_overlap(som_new, som_ref, "adm1_name")
message("Somalia regions named by overlap: ", paste(som_new$adm1_name, collapse = ", "))
stopifnot(!anyNA(som_new$adm1_name), !anyDuplicated(som_new$adm1_name), nrow(som_new) == 18)
som_out <- som_new[order(som_new$adm1_name), "adm1_name"]

# ---- Disputed areas -------------------------------------------------------
dis <- read_shp("EMRO_Disputed_areas.shp")
dis$id <- seq_len(nrow(dis))
dis_out <- dis[, "id"]

# make_valid() can leave GEOMETRYCOLLECTIONs (polygon + stray line/point
# slivers); keep only the polygon parts so leaflet can draw every feature.
as_multipolygon <- function(x) {
  g <- lapply(sf::st_geometry(x), function(geom) {
    if (inherits(geom, "GEOMETRYCOLLECTION")) {
      geom <- sf::st_collection_extract(sf::st_sfc(geom, crs = 4326), "POLYGON")[[1]]
    }
    sf::st_cast(sf::st_sfc(geom, crs = 4326), "MULTIPOLYGON")[[1]]
  })
  sf::st_geometry(x) <- sf::st_sfc(g, crs = 4326)
  x
}
# The GeoJSON is written to 5 decimal places; snap to that grid first and
# re-clean, so rounding can't turn a sliver into a stray line on write.
snap_clean <- function(x) {
  sf::st_geometry(x) <- sf::st_set_precision(sf::st_geometry(x), 1e5)
  x <- sf::st_make_valid(x)
  as_multipolygon(x)
}
pak_out <- snap_clean(pak_out)
som_out <- snap_clean(som_out)
dis_out <- snap_clean(dis_out)
message("Geometry types written: ", paste(unique(c(as.character(sf::st_geometry_type(pak_out)), as.character(sf::st_geometry_type(som_out)), as.character(sf::st_geometry_type(dis_out)))), collapse = ", "))

# ---- Write ----------------------------------------------------------------
write_geojson <- function(x, path) {
  if (file.exists(path)) file.remove(path)
  sf::st_write(x, path, driver = "GeoJSON", quiet = TRUE,
               layer_options = "COORDINATE_PRECISION=5")
  message("Wrote ", path, " (", round(file.size(path) / 1024), " KB, ", nrow(x), " features)")
}
write_geojson(pak_out, "Data/pak_admin_boundaries/pak_admin1_emro.geojson")
write_geojson(som_out, "Data/som_admin_boundaries/som_admin1_emro.geojson")
write_geojson(dis_out, "Data/emro_disputed_areas.geojson")
