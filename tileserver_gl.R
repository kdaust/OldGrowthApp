
library(sf)
library(analogsea)
Sys.setenv(DO_PAT = "b02741f9a02e7386464b5dd7a2dbea6d890c3f8051fd32fd239e17590a0039bf")
library(bccciss)
library(ssh)

out_dir <- "./data-raw/shp"
shp_name <- "Defer.shp"
layer <- "Defer"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dat <- st_read("./VectorData/Cutblocks.gpkg")
dat <- st_transform(dat,4326)
colnames(dat)[1] <- "ID"
dat$PolyID <- seq_along(dat$ID)
st_write(dat,dsn = out_dir,layer = "Cutblocks", driver = "ESRI Shapefile")

# Digital Ocean provisioning - Setup your SSH keys in your accounts before running these.
tileserver <- setup_docklet(size = "s-1vcpu-2gb")
# Or Reuse an existing droplet
tileserver <- droplets()[["TyrannicalAccessibility"]]
# About 5-6h

remote_shp_tiles_new <- function(droplet, ..., source_dir, remote_dir = "/tmp/shp", skip_upload = FALSE) {
  
  layers <- list.files(source_dir, "\\.shp$", ignore.case = TRUE)

  names(layers) <- tools::file_path_sans_ext(layers)
  
  if (!skip_upload == TRUE) {
    analogsea::droplet_ssh(droplet, paste("mkdir -p", remote_dir))
    analogsea::droplet_upload(droplet, local = list.files(source_dir, full.names = TRUE), remote = remote_dir)
    geojsons <- character()
    for (i in 1:length(layers)) {
      geojson <- shQuote(file.path(remote_dir, paste0(names(layers[i]), ".geojson")))
      shp <- shQuote(file.path(remote_dir, layers[i]))
      analogsea::droplet_ssh(droplet, paste("ogr2ogr -f GeoJSON", geojson, shp))
      geojsons <- c(geojsons, geojson)
    }
    analogsea::droplet_ssh(droplet, paste("ls -alh", remote_dir,"| grep geojson"))
  } else {
    geojsons <- shQuote(file.path(remote_dir, paste0(names(layers), ".geojson")))
  }
  
  base_cmd <- "tippecanoe -o /mapdata/defertiles.mbtiles"
  cmd <- paste(base_cmd, paste(..., collapse = " "), paste(geojsons, collapse = " "))
  analogsea::droplet_ssh(droplet, cmd)
  analogsea::droplet_ssh(droplet, "ls -alh /mapdata | grep mbtiles")
  
  return(invisible(droplet))
  
}

remote_shp_tiles(tileserver,
                 "-z18 --simplification=10 --force --coalesce-densest-as-needed --extend-zooms-if-still-dropping --detect-shared-borders",
                 source_dir = out_dir, skip_upload = F)
launch_tileserver(tileserver, config = "./config/tileserver/config.json")

