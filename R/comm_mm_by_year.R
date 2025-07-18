########################################################################################
########################################################################################
########################################################################################
########################################################################################

# Community mismatch indicators

#' @title Community Mismatch by Year
#' @description Summarizes and plots mismatch proportion from comm_mismatch() by year.
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

# Community mismatch proportion by year
comm_mm_by_year = function(output, plot = T){

  # Summarize community output
  mm_yr = dplyr::group_by(output, year) %>%
    dplyr::filter(is.na(emerged) == F) %>%
    dplyr::summarize(mismatch = sum(emerged)/dplyr::n())

  # Plot result
  if(plot == T){

    # Plot p-value matrix
    plot = ggplot2::ggplot(mm_yr, ggplot2::aes(x = year, y = mismatch)) +
      ggplot2::geom_line(lwd = 1) + ggplot2::theme_classic() +
      ggplot2::labs(x = 'Year', y = 'Proportion of Mismatched Species Pairs') +
      ggplot2::ylim(c(0,1))


    print(plot)

  } # End plot if

  # Return result
  return(mm_yr)

} # End comm_mm_ind function
