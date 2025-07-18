########################################################################################
########################################################################################
########################################################################################
########################################################################################

#' @title Classification by species and year
#' @description Calculate and plot ToPE and ToEE classification results for all species and year combinations within a dataset.
#'
#' @param all_curves Output data frame from community().
#' @param plot If T, outputs a plot.
#'
#' @returns An edited version of the data frame output from community() designed for plotting classification results by species and year.
#' @export
#'
#' @examples
#' # Set seed
#' set.seed(123)
#'
#' # Create test dataset 1
#' year = seq(1,30,1)
#' event = round(rnorm(30, 100, 5))- seq(1,30,1)
#' env = (rnorm(30, 15, 0.5))
#' species = 1
#' sp1 = data.frame(year, species, event, env)
#'
#' # Create test dataset 2
#' year = seq(1,30,1)
#' event = round(rnorm(30, 100, 5))
#' env = (rnorm(30, 15, 0.5)) + seq(0.1,3,0.1)
#' species = 2
#' sp2 = data.frame(year, species, event, env)
#'
#' # combine
#' dataset = as.data.frame(rbind(sp1, sp2))
#'
#' # Calculate empirical time of phenological emergence (ToPE)
#' com_res = community(dataset)
#'
#' # Run and plot classification by species and year
#' class_by_species(com_res)


# Raster plot, classifications by species and year

class_by_species = function(all_curves, plot = T){

  # Reformat to matrix
  curve_matrix = all_curves %>% dplyr::select(species, year, class, class_c)

  # Factorize final class
  curve_matrix$final = as.numeric(factor(curve_matrix$class, levels = c('no signal', 'shifting', 'decoupling', 'combination')))

  # Calculate year final classification reached
  lines_matrix = curve_matrix %>% # group_by(species) %>%
    dplyr::filter(class_c == final)
  points_matrix = lines_matrix %>% dplyr::group_by(species) %>% dplyr::filter(year == min(year))

  # grab year final class is established
  year_fc = dplyr::select(points_matrix, species, year) %>% dplyr::mutate(year_fc = year) %>%
    dplyr::select(species, year_fc)

  # Sort
  curve_matrix = dplyr::left_join(curve_matrix, year_fc) %>%
    dplyr::arrange(ifelse(class != 'no signal', as.numeric(final), 5), year_fc, species, year) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(order = match(species, unique(species)))

  # Check if plotting
  if(plot == T){

    # colour scale
    cscale = c(hcl(h=240, 0, 95, alpha = 0.8), # No signal
               hcl(h=60, 100, 100, alpha = 0.8), # Combination
               hcl(h=12, 200, 100, alpha = 0.8), # decouple
               hcl(h=120, 100, 100, alpha = 0.8))

    # Plot p-value matrix
    plot = ggplot2::ggplot(curve_matrix, ggplot2::aes(x = year, y = order)) +
      ggplot2::geom_raster(ggplot2::aes(fill = as.factor(class_c))) +
      ggplot2::scale_fill_manual(values = cscale[c(1,4,3,2)], labels = c('No Signal', 'Shift', 'Decouple', 'Combination')) +
      ggplot2::theme_classic() + ggplot2::labs(x = 'Year', y = 'Species', fill = 'Classification') +
      ggplot2::scale_y_continuous(breaks = unique(curve_matrix$order), labels = unique(curve_matrix$species), position = 'right') +
      ggplot2::theme(legend.position = 'left') +
      ggplot2::geom_line(data = dplyr::filter(curve_matrix, (class_c == final) & (class != 'no signal')), ggplot2::aes(x = year, y = order, group = species), inherit.aes = F) +
      ggplot2::geom_point(data = dplyr::filter(curve_matrix, class != 'no signal'), ggplot2::aes(x = year_fc, y = order, group = species), inherit.aes = F)

    print(plot)

  } # End plot if

  # Return curve matrix
  return(curve_matrix)

}
