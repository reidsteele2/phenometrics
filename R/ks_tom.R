########################################################################################
########################################################################################
########################################################################################
########################################################################################



#' @title Statistical Time of Mismatch (ToM)
#' @description Calculate Time of Mismatch (ToM) between two time series use statistical methodology.
#'
#' @param sp1 A data frame containing the time series to test for ToM. Must contain year (column named 'year'), timing of phenological event of interest, in Julian day (column named 'event').
#' @param sp2 A data frame containing the time series against which sp1 is compared for ToM. Must contain year (column named 'year'), timing of phenological event of interest, in Julian day (column named 'event').
#' @param alt Alternative hypothesis for emergence testing ("two.sided", "less", or "greater"). Set to 'two.sided' by default, indicating checking for sp1 event timing to to be either greater than or less than sp2.
#' @param emt Number of consecutive years of positive test results required to define emergence.
#' @param alpha Significance value for KS test.
#' @param ks_t Proportion of significant bootstrap KS tests required for a positive test result
#' @param nboot Number of KS test bootstraps samples.
#' @param plot If TRUE, will generate a plot of test result against year.
#' @param unemergence If F, all years after first emergence are set to emerged. If T, calculation for each individual year is returned.
#' @param model Type of model used for detrending. 'linear' or 'curvilinear'. 'linear' uses a simple linear model, 'curvilinear' adds an x^2 term.
#' @param ... Additional arguments to feed to `lm()`
#'
#' @returns A data frame containing year, test result (p, binary. 1 = threshold exceeded), and emergence status (binary, 1 = emerged)
#' @export
#'
#' @examples
#' # Set seed
#' set.seed(123)
#'
#' # Create test dataset 1
#' year = rep(seq(1,30,1), 10)
#' event = round(rnorm(300, 100, 5))- rep(seq(1,30,1), 10)
#' species = 1
#' sp1 = data.frame(year, species, event); sp1 = sp1[order(sp1$year),]
#'
#' # Create test dataset 2
#' year = rep(seq(1,30,1), 10)
#' event = round(rnorm(300, 100, 5))
#' species = 2
#' sp2 = data.frame(year, species, event); sp2 = sp2[order(sp2$year),]
#'
#' # Calculate empirical time of phenological emergence (ToPE)
#' ks_tom(sp1, sp2)



# Calculate time of mismatch using KS Test
ks_tom = function(sp1,     # Input data for species 1
                  sp2,     # Input data for species 2
                  alt = 'two.sided',     # Alternative hypothesis for ks test: greater, less, 2 sided
                  emt = 5, # Emergence threshold (number of years for emergence)
                  alpha = 0.05, # Significance threshold for ks test
                  ks_t = 0.6, # KS test threshold
                  nboot = 100, # Number of bootstraps for ks testing
                  plot = T, # Plot results?
                  unemergence = F, # if F, all years after first emergence are set to emergence
                  model = 'linear', # model used for detrending. 'linear' or 'curvilinear'.
                  ... # Additional arguments to feed to lm()
){

  # Add error messages for invalid selections
  if(!alt %in% c("two.sided", "less", "greater")){stop('alt setting not recognized')}
  if(!model %in% c("linear", "curvilinear")){stop('model setting not recognized')}

  # gather year range
  sp1_yr = range(sp1$year)
  sp2_yr = range(sp2$year)

  # Total range
  yrange = c(max(c(sp1_yr[1], sp2_yr[1])), min(sp1_yr[2], sp2_yr[2]))

  # Filter to same years
  sp1 = as.data.frame(dplyr::filter(sp1, (year >= min(yrange)) &  (year <= max(yrange))))
  sp2 = as.data.frame(dplyr::filter(sp2, (year >= min(yrange)) &  (year <= max(yrange))))

  # Fit linear models (event timing)
  if(model == 'linear'){

    lm_ev_sp1 = lm(event ~ year, data = sp1, ...)
    lm_ev_sp2 = lm(event ~ year, data = sp2, ...)

  } else if(model == 'curvilinear'){

    lm_ev_sp1 = lm(event ~ year + I(year^2), data = sp1, ...)
    lm_ev_sp2 = lm(event ~ year + I(year^2), data = sp2, ...)

  }

  # Collect baselines
  base_sp1 = predict(lm_ev_sp1, newdata = data.frame(year = min(sp1_yr)))
  base_sp2 = predict(lm_ev_sp2, newdata = data.frame(year = min(sp2_yr)))

  # Calculate adjustment
  adj_sp1 = predict(lm_ev_sp1, newdata = sp1) - base_sp1
  adj_sp1tosp2 = predict(lm_ev_sp2, newdata = sp1) - base_sp2

  # Swap Species
  sp1_mm = sp1
  sp1_mm$event = sp1_mm$event - adj_sp1 + adj_sp1tosp2

  # Run ks tests

  # p value container
  ks_p = rep(NA, length(unique(sp1$year)))

  # Cycle years and perform KS test
  for(i in 1:length(unique(sp1$year))){

    # Run KS test
    ks_r = ks(sp1_mm[sp1_mm$year==unique(sp1_mm$year)[i], 'event'],
              sp1[sp1$year==unique(sp1$year)[i], 'event'],
              alt = alt, alpha = alpha, nboot = nboot)

    # Fill ks_p
    ks_p[i] = ks_r

  } # End KS test loop

  # Calculate emergence
  emerged = data.table::frollapply(ks_p, N = emt, FUN = function(x){all(x>=ks_t)}, align = 'left')

  # Set all to 1 after emergence
  if(unemergence == F){

    # Loop through emerged
    for(i in 1:length(emerged)){

      # If emerged is NA, leave it
      if(is.na(emerged[i])){

        emerged[i] = NA

      } else {

        # If previous value of emerged is 1, make all subsequent values 1
        if((is.na(data.table::shift(emerged)[i]) == F) & (data.table::shift(emerged)[i]==1)){

          emerged[i] = 1

        } # End second if

      } # End else

    } # End loop

  } # End unemergence if

  # Create data frame
  ks_p_df = cbind.data.frame(year = unique(sp1$year), p = ks_p, emerged)

  # Plot if requested
  if(plot == T){

    # Plot p-value curve
    plot(ks_p ~ unique(sp1$year), type = 'l', pch = 16, lwd = 2,
         main = paste(unique(sp1$species), 'x', unique(sp2$species)),
         ylab = 'Proportion of KS Tests Significant', xlab = 'Year')
    abline(h = ks_t, lty = 'dashed', col = 'blue') # Add significance threshold line
    abline(v = ks_p_df[min(which(emerged == 1)),'year'], lwd = 2, col = 'red') # Add Emerged Year
    text(y = c(-0.5, 0.5), x = ks_p_df[min(which(emerged == 1)),'year'] + 4, label = paste('TOM:', ks_p_df[min(which(emerged == 1)),'year']), col = 'red')

  } # End plot if

  # Return data frame
  return(ks_p_df)

} # end KS mismatch function
