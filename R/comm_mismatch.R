########################################################################################
########################################################################################
########################################################################################
########################################################################################


#' @title Community ToM Metrics
#' @description Calculate ToM metrics for all relevant species pairs in a dataset
#'
#' @param data A data frame containing the time series to test for ToM. Must contain year (column named 'year'), timing of phenological event of interest, in Julian day (column named 'event'), and unique identifier for each individual time series for which ToPE and ToEE should be calculated (column named 'species').
#' @param alt Alternative hypothesis for emergence testing. Set to 'two.sided' by default, indicating checking for sp1 event timing to to be either greater than or less than sp2.
#' @param method Methodology used for emergence calculations. Set to 'empirical' for empirical testing (default), or 'statistical' for statistical testing using the Kolmogorov-Smirnov test.
#' @param emt Number of consecutive years of positive test results required to define emergence.
#' @param base_y Number of years at the start of the time series to compare to ensure testing is appropraite. Species pairs will only be tested if the first base_y years event values are within 1 standard deviation.
#' @param quants Quantiles used for empirical testing. Unused if method = 'statistical'.
#' @param alpha Alpha value used to determine significance for statistical testing. Unused if method = 'empirical'.
#' @param ks_t Proportion of significant KS Tests required to define a positive test result. Unused if method = 'empirical'.
#' @param nboot Number of boostrapped KS tests to run. Unused if method = 'empirical'.
#' @param unemergence If F, all years after first emergence are set to emerged. If T, calculation for each individual year is returned.
#'
#' @returns A data frame of species pairs with test result and emergence status for each individual year. Designed to feed into comm_mm_by_spp() and comm_mm_by_year() functions.
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
#' comm_mismatch(dataset)


# Community-level indicators

# Community match-mismatch
comm_mismatch = function(data,
                         alt = 'two.sided',     # Alternative hypothesis for ks test: greater, less, 2 sided
                         method = 'empirical', # Empirical or statistical
                         emt = 5, # Emergence threshold (number of years for emergence)
                         base_y = 5, # Number of years from the start of the time series to compare to ensure testing is appropriate
                         quants = c(0.025, 0.975), # quantiles for empirical testing
                         alpha = 0.05, # Significance threshold for ks test
                         ks_t = 0.6, # KS test threshold
                         nboot = 100, # Number of bootstraps for ks testing
                         unemergence = F){ # if F, all years after first emergence are set to emergence){

  # Pull out species names
  spp_list = unique(data$species)

  # Create data frame to hold test pairs
  test_list = expand.grid(spp_list, spp_list); colnames(test_list) = c('sp1', 'sp2')

  # Remove self-tests
  test_list = dplyr::filter(test_list, test_list$sp1 != test_list$sp2)

  # Output container object
  output = NULL

  # Run tests
  for(i in 1:length(spp_list)){

    # Gather species
    sp1 = spp_list[i]

    # Filter out other species
    tests = test_list[test_list$sp1 == sp1,]

    # sp1 data
    sp1_data = dplyr::filter(data, species == sp1)


    # Filter out single sample years %>%
    if(method == 'statistical'){
            sp1_data = sp1_data %>%
              dplyr::filter(n() > 1) %>%
              dplyr::group_by(year) %>%
              dplyr::ungroup()
    }

    # Loop through tests
    for(j in 1:nrow(tests)){

      # sp2 data
      sp2_data = dplyr::filter(data, species == tests[j, 'sp2'])

      # Filter out single sample years %>%
      if(method == 'statistical'){
        sp2_data = sp2_data %>%
          dplyr::filter(n() > 1) %>%
          dplyr::group_by(year) %>%
          dplyr::ungroup()
      }

      # Gather first years
      sp1_minyear = min(sp1_data$year)
      sp2_minyear = min(sp2_data$year)

      # Calculate minimum combined year
      minyear = max(c(sp1_minyear, sp2_minyear))

      # Calculate mean and sd of first 5 years
      sp2_ori = dplyr::filter(sp2_data, year <= minyear+base_y-1) %>%
        dplyr::summarize(mean = mean(event), sd = sd(event))
      sp1_ori = dplyr::filter(sp1_data, year <= minyear+base_y-1) %>%
        dplyr::summarize(mean = mean(event), sd = sd(event))

      # Calculate upper and lower limit
      sp1_lims = c(sp1_ori$mean - sp1_ori$sd, sp1_ori$mean + sp1_ori$sd)

      # Determine whether test is appropriate
      if((sp2_ori$mean >= min(sp1_lims)) & (sp2_ori$mean <= max(sp1_lims))){

        if(method == 'empirical'){

          # Run mismatch test
          mm = emp_tom(sp1_data, sp2_data, alt = alt, emt = emt, plot = F, unemergence = unemergence,
                           quants = quants)

        }

        if(method == 'statistical'){

          # Run mismatch test
          mm = ks_tom(sp1_data, sp2_data, alt = alt, emt = emt, plot = F, unemergence = unemergence,
                           alpha = alpha, ks_t = ks_t, nboot = nboot)

        }

        # Enter results
        output = rbind(output, data.frame(sp1 = sp1, sp2 = tests[j, 'sp2'], year = mm$year, p = mm$p, emerged = mm$emerged))

      } # end test check if

    } # End j loop

  } # End i loop

  # Return output
  return(output)

} # End community mismatch function
