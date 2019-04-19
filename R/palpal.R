#' List of available color-blind friendly palettes
#' export
palpal_palettes <- list(wong = c("#000000", "#e69f00", "#56b4e9", "#009e73",
                        "#f0e442", "#0072b2", "#d55e00", "#cc79a7"),
                        tol = c("#332288", "#AA4499", "#88CCEE", "#882255", "#44AA99",
                                "#CC6677","#117733", "#DDCC77", "#999933"),
                        mkweb = c("#009292", "#920000", "#490092", "#db6d00", "#24ff24",
                          "#ffff6d", "#000000", "#006ddb", "#004949","#924900",
                          "#ff6db6", "#6db6ff","#b66dff", "#ffb6db","#b6dbff"))
#' Palette generator
#' Discrete only
#' @param n Number of colors, default all
#' @param name Name of palette: \code{wong} or \code{mkweb}
#' @param type Only "discrete" is enabled for the now
#' @return vector of colors
#' @export
#' @examples
#' palpal("wong")
#' palpal("mkweb")
#' \dontrun{ggplot(data=mtcars, aes(x=mpg, y=disp, color=factor(cyl))) +
#' geom_point() + scale_color_manual(values=palpal("mkweb"))}

palpal <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- palpal_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" & n > length(pal)) {
    stop(paste("Sorry, the", name, "palette only contains", length(pal), "colors."))
  }

  #return_pal <- switch(type,
  #            continuous = grDevices::colorRampPalette(pal)(n),
  #            discrete = pal[1:n]
  #)

  return_pal <- pal[1:n]

  structure(return_pal, class = "palette", name = name)
}
