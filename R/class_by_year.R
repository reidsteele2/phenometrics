########################################################################################
########################################################################################
########################################################################################
########################################################################################

#' @title Classification by year
#' @description Calculate and plot summarized ToPE and ToEE classification results across all species in a dataset for each year.
#'
#' @param all_curves Output data frame from community().
#' @param plot If T, outputs a plot.
#'
#' @returns An edited version of the data frame output from community() designed for plotting summarized classification results by year.
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
#' class_by_year(com_res)

# Area curve plot, classification proportion by year

class_by_year = function(all_curves, plot = T){

  # Calculate final metrics
  conc_curve = all_curves %>% #group_by(species) %>%
    dplyr::group_by(year) %>% # Switch to get years
    dplyr::summarize(n_em = sum(emerged), # Number of emerged (shift) species
              n_dc = sum(decoupled), # Number of decoupled species
              n_cb = sum(combination), # Number of combined species
              n_shift = sum(shift), # Number of shifted, non combined species
              n_decouple = sum(decouple), # Number of decoupled, non combined species
              n_ncc = sum(unaffected), # Number of species with no climate change signal
              sp = length(unique(species))) # Total number of species

  # Remove NA years
  conc_curve = conc_curve[is.na(conc_curve$n_em) == F,]

  # Proportional  version
  conc_curve_p = conc_curve

  # Rename n_year
  colnames(conc_curve_p)[colnames(conc_curve_p) == 'n_years'] = 'year'

  # Divide by species to get proportions
  conc_curve_p[,-1] = conc_curve_p[,-1]/conc_curve$sp

  # Create climate change column
  conc_curve_p$n_cc = 1-conc_curve_p$n_ncc

  # Plotting
  if(plot == T){

    # Try ggplot
    conc_curve_l = dplyr::select(conc_curve_p, year, n_shift, n_decouple, n_cb, n_ncc, n_cc)

    # Rename columns
    colnames(conc_curve_l) = c('year', 'shift', 'decouple', 'combination', 'no signal', 'affected')

    # Pivot longer
    conc_curve_area = dplyr::select(conc_curve_l, year, shift, decouple, combination, `no signal`) %>%
      tidyr::pivot_longer(cols = c('shift', 'decouple', 'combination', 'no signal'),
                   names_to = 'classification', values_to = 'n_species') %>%
      dplyr::mutate(classification = factor(classification, levels = c('no signal', 'combination', 'decouple', 'shift')))

    # colour scale
    cscale = c(hcl(h=240, 0, 95, alpha = 0.8), # No signal
               hcl(h=60, 100, 100, alpha = 0.8), # Combination
               hcl(h=12, 200, 100, alpha = 0.8), # decouple
               hcl(h=120, 100, 100, alpha = 0.8))

    # Plot area curve
    plot = ggplot2::ggplot(conc_curve_area, ggplot2::aes(x = year, y = n_species, fill = classification)) +
      ggplot2::geom_area() + ggplot2::geom_line(data = conc_curve_p, ggplot2::aes(x = year, y = n_cc, fill = NULL), lwd = 1) +
      ggplot2::scale_fill_manual(values = cscale, name = "Classification") + # Shift
      ggplot2::theme_classic() + ggplot2::ylim(c(0,1)) + ggplot2::xlab('Year') + ggplot2::ylab('Proportion of Species')

    print(plot)

  }

  # Return
  return(conc_curve_p)

}
