#' Clip and Move a Raster File
#'
#' Clips a raster to a polygon bounding box, optionally masks to polygon, and saves the 
#' output in the same format as the input.
#'
#' @param input_path Character. Path to the input raster file.
#' @param polygon An `sf` or `SpatVector` object to be used for clipping and masking.
#' @param out_path Character. Path to save the processed raster.
#' @return Character. The path to the saved output raster.
#' @author David LeBauer
clip_and_move_raster_files <- function(input_path, polygon, out_path, mask = TRUE, overwrite = TRUE) {
  rast_in <- terra::rast(input_path)
  # check that input file exists
  if (!file.exists(input_path)) {
    PEcAn.logger::logger.error("Input raster file does not exist: ", input_path)
  }
  # check that polygon is valid
  if (!inherits(polygon, c("sf", "SpatVector"))) {
    PEcAn.logger::logger.error("Polygon must be an sf object or SpatVector")
  }
  if (inherits(polygon, "sf")) {
    # Convert sf object to SpatVector
    polygon <- terra::vect(polygon)
  }
  # Reproject polygon to raster CRS, convert to SpatVector
  polygon_proj <- sf::st_transform(polygon, crs = terra::crs(rast_in))
  polygon_vect <- terra::vect(polygon_proj)
  rast_crop <- terra::crop(rast_in, polygon_vect)
  
  if (mask) {
    rast_to_write <- terra::mask(rast_crop, polygon_vect)
  } else {
    rast_to_write <- rast_crop
  }
  filetype <- terra::filetype(rast_in)
  gdal_opts <- terra::gdal(rast_in)
  
  terra::writeRaster(
    rast_to_write,
    filename = out_path, 
    overwrite = overwrite,
    filetype = filetype,
    gdal = gdal_opts
  )
  return(out_path)
}