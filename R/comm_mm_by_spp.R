########################################################################################
########################################################################################
########################################################################################
########################################################################################

# Community mismatch indicators

#' @title Community Mismatch by Species and Year
#' @description Summarizes and plots output from comm_mismatch() by species and year.
#'
#' @param output Output from comm_mismatch().
#' @param plot Output data frame from community().
#'
#' @returns A sorted and summarized data frame generated from the output of comm_musmatch().
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
#' sp1 = data.frame(year, species, event)
#'
#' # Create test dataset 2
#' year = seq(1,30,1)
#' event = round(rnorm(30, 100, 5))
#' species = 2
#' sp2 = data.frame(year, species, event)
#'
#' # Create test dataset 3
#' year = seq(1,30,1)
#' event = round(rnorm(30, 95, 5))- seq(1,15,1)
#' species = 3
#' sp3 = data.frame(year, species, event)
#'
#' # combine
#' dataset = as.data.frame(rbind(sp1, sp2, sp3))
#'
#' # Calculate empirical time of phenological emergence (ToPE)
#' mm_output = comm_mismatch(dataset)
#'
#' # Calculate and plot mismatch by species and year
#' comm_mm_by_spp(mm_output)

# Mismatch by year and species

comm_mm_by_spp = function(output, plot = T){

  # Summarize community output
  mm_summ = dplyr::group_by(output, sp1, year) %>%
    dplyr::filter(is.na(emerged) == F) %>%
    dplyr::summarize(mismatch = sum(emerged)/length(unique(sp2)))

  # Plot result
  if(plot == T){

    # Order species by mismatch ratio in final year, descending
    sp_order = dplyr::filter(mm_summ, year == max(year)) %>%
      dplyr::arrange(desc(mismatch))

    # Calculate species order for plotting
    order = match(mm_summ$sp1, unique(sp_order$sp1))

    # Plot mismatch by species matrix
    plot = ggplot2::ggplot(mm_summ, ggplot2::aes(x = year, y = order)) +
      ggplot2::geom_raster(ggplot2::aes(fill = mismatch)) +
      ggplot2::scale_fill_continuous(type = 'viridis') +
      ggplot2::theme_classic() + ggplot2::labs(x = 'Year', y = 'Species', fill = 'Proportion Mismatched') +
      ggplot2::scale_y_continuous(breaks = unique(order), labels = unique(mm_summ$sp1), position = 'right') +
      ggplot2::theme(legend.position = 'left')

    print(plot)

  } # End plot if

  return(mm_summ)

} # End comm_mm_ind function
