scatterWithSmooth <- function(data, mapping, ...) {
  ggally_smooth_loess(data, mapping,
                      colour = "#737373",
                      shape = 1, ...)
}
