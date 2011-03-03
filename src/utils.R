SavePlot <- function(filename,
                     plot = last_plot(),
                     width = 640, height = 480) {
  filename <- file.path("graphs", str_c(filename, ".png"))
  Cairo(filename, width = width, height = height)
  print(plot)
  dev.off()
}
