

# preprocess length data
#' Title
#'
#' @param tbhdat input data
#' @param Scientificname chr string of species to plot
#'
#' @import  dplyr
#' @import ggplot2
#' @importFrom tidyr uncount
#' @importFrom gridExtra tableGrob
#' @import patchwork
#'
#'
#' @return A plot
#' @export
#'
#' @examples
#' tbhdat <- read.csv('data/example_len_dat.csv', stringsAsFactors = F)
#' sumplot(tbhdat, 'Lagodon rhomboides')

sumplot <- function(tbhdat, Scientificname) {
  Plot <- tbhdat %>%
    filter(Scientificname == 'Lagodon rhomboides') %>%
    ggplot() +
    geom_histogram(aes(x=sl), color = "black", fill = NA, binwidth = 10) +
    labs(x = "Standard length (mm)", y = "Frequency") +
    facet_wrap(~ Gear, ncol = 1) +
    theme_classic() +
    theme(panel.border = element_rect(color = "black", fill = NA)) +
    theme(strip.background = element_rect(fill = "gray90"))

  #get length summary data
  summary <- tbhdat %>%
    filter(Scientificname == 'Lagodon rhomboides') %>%
    group_by(Gear) %>%
    summarise(min_sl = min(sl),
              max_sl = max(sl),
              mean_sl = round(mean(sl), digits = 2),
              sd_sl = round(sd(sl), digits = 2),
              Num_lengths = n()) %>%
    ungroup()

  # combine plot and summary table
  Plot / gridExtra::tableGrob(summary, rows = NULL) +
    plot_layout(heights = c(2,0.8)) +
    plot_annotation(title = 'Lagodon rhomboides', theme= theme(plot.title = element_text(face = "italic")))
}
