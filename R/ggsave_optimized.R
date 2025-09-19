#' Save Optimized Image Formats
#'
#' Saves a ggplot object to a file, supporting both vector and raster formats.
#' Raster formats are processed with `magick` for additional optimizations.
#'
#' @param filename Character. Path to save the plot, including file extension.
#' @param plot ggplot object. The plot to save. Defaults to the last plot.
#' @param width Numeric. Width of the plot in specified units. Default is 7.
#' @param height Numeric. Height of the plot in specified units. Default is 5.
#' @param units Character. Units for width and height ("in", "cm", "mm"). Default is "in".
#' @param dpi Numeric. Resolution for raster formats. Default is 96.
#' @param quality Numeric. JPEG/WebP quality (1-100). Default is 80.
#' @param bg Character. Background color. Default is "white".
#' @param ... Additional arguments passed to `ggplot2::ggsave`.
#'
#' @return Invisibly returns the filename of the saved plot.
#' @examples
#' \dontrun{
#' ggsave_optimized("plot.png", plot = my_plot, width = 8, height = 6)
#' }
#' @export
ggsave_optimized <- function(
    filename,
    plot = ggplot2::last_plot(),
    width = 7,
    height = 5,
    units = "in",
    dpi = 96,
    quality = 80,
    bg = "white",
    ...) {
  ext <- tolower(tools::file_ext(filename))

  # Use ggsave for vector formats and raster formats other than WebP
  if (ext %in% c("svg", "pdf", "eps", "png", "jpg", "jpeg")) {
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      bg = bg,
      ...
    )
    # Optimize PNGs with magick
    if (ext == "png") {
      img <- magick::image_read(filename)
      img <- magick::image_quantize(img, max = 256)
      magick::image_write(img, path = filename, format = "png", compression_level = 9)
    }
    return(invisible(filename))
  }

  # Use magick for WebP (not natively supported by ggsave)
  if (ext == "webp") {
    tmp <- withr::local_tempfile(fileext = ".png")
    ggplot2::ggsave(
      filename = tmp,
      plot = plot,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      bg = bg,
      ...
    )
    img <- magick::image_read(tmp)
    magick::image_write(img, path = filename, format = "webp", quality = quality)
    unlink(tmp)
    return(invisible(filename))
  }

  stop("Unsupported file extension: ", ext)
}
