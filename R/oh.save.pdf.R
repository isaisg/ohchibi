#' Function to save objects in high resolution. To be used with illustrator
#'
#' Pass any type of figure.
#' @keywords save,figure,illustrator,vectorized
#' @export
#' @examples
#' oh.save.pdf()

oh.save.pdf <- function(p = NULL,outname = NULL,outdir = "figures/",
                        width = 20, height = 15, family = "Arial", fallback_resolution = 1200,
                        antialias = "default",pointsize = 12){
  dir.create(outdir, showWarnings = FALSE)
  myfilename <- paste(outdir, outname, sep = "/")
  cairo_pdf(filename = myfilename, onefile = FALSE, fallback_resolution = fallback_resolution,
          width = width, height = height, family = family, antialias = antialias,pointsize = pointsize)
  print(p)
  dev.off()

}
