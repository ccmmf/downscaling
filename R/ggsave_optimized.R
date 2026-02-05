#' Save Web Optimized Image Formats
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
#' @param use_cairo Logical. Use Cairo/svglite for vector devices if available. Default TRUE.
#' @param use_ragg Logical. Use ragg devices for raster if available. Default TRUE.
#' @param png_quantize Logical. Quantize PNG colors via magick to reduce size. Default TRUE.
#' @param png_max Integer. Max colors for PNG quantization. Default 256.
#' @param webp_lossless Logical. Encode WebP losslessly (overrides quality). Default FALSE.
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
    use_cairo = TRUE,
    use_ragg = TRUE,
    png_quantize = TRUE,
    png_max = 256,
    webp_lossless = FALSE,
    ...) {
  ext <- tolower(tools::file_ext(filename))
  # Choose device where applicable
  dev <- NULL
  if (ext %in% c("pdf", "svg", "eps")) {
    if (isTRUE(use_cairo) && isTRUE(grDevices::capabilities()["cairo"])) {
      if (ext == "pdf") dev <- grDevices::cairo_pdf
      if (ext == "svg") {
        if (requireNamespace("svglite", quietly = TRUE)) {
          dev <- svglite::svglite
        } else {
          dev <- grDevices::svg
        }
      }
      if (ext == "eps") dev <- grDevices::cairo_ps
    } else {
      if (ext == "svg") dev <- grDevices::svg
    }
  } else if (ext %in% c("png", "jpg", "jpeg")) {
    if (isTRUE(use_ragg) && requireNamespace("ragg", quietly = TRUE)) {
      if (ext == "png") dev <- ragg::agg_png
      if (ext %in% c("jpg", "jpeg")) dev <- ragg::agg_jpeg
    }
  }

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
      device = dev,
      limitsize = FALSE,
      ...
    )
    # Optimize PNGs with magick
    if (ext == "png") {
      if (isTRUE(png_quantize)) {
        img <- magick::image_read(filename)
        img <- magick::image_quantize(img, max = png_max)
        magick::image_write(img, path = filename, format = "png", compression_level = 9)
      }
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
      device = if (isTRUE(use_ragg) && requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL,
      limitsize = FALSE,
      ...
    )
    img <- magick::image_read(tmp)
    if (isTRUE(webp_lossless)) {
      magick::image_write(img, path = filename, format = "webp", quality = 100)
    } else {
      magick::image_write(img, path = filename, format = "webp", quality = quality)
    }
    unlink(tmp)
    return(invisible(filename))
  }

  # Support TIFF via magick
  if (ext %in% c("tif", "tiff")) {
    tmp <- withr::local_tempfile(fileext = ".png")
    ggplot2::ggsave(
      filename = tmp,
      plot = plot,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      bg = bg,
      device = if (isTRUE(use_ragg) && requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL,
      limitsize = FALSE,
      ...
    )
    img <- magick::image_read(tmp)
    magick::image_write(img, path = filename, format = "tiff")
    unlink(tmp)
    return(invisible(filename))
  }

  stop("Unsupported file extension: ", ext)
}
