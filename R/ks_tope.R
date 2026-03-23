########################################################################################
########################################################################################
########################################################################################
########################################################################################
#' @title Statistical Time of Phenological Emergence (ToPE)
#'
#' @description Calculate Time of Phenological Emergence (ToPE) for a single phenology time series using statistical methodology.
#'
#' @param data A data frame containing the time series to test for ToEE. Must contain year (column named 'year') and timing of phenological event of interest, in Julian day (column named 'event').
#' @param emt Number of consecutive years of positive test results required to define emergence.
#' @param alpha Significance value for KS test.
#' @param ks_t Proportion of significant bootstrap KS tests required for a positive test result
#' @param nboot Number of KS test bootstraps samples.
#' @param plot If TRUE, will generate a plot of test result against year.
#' @param max_y Moving year window used to generate detrended counterfactual. If 0, moving window is deactivated, else length of moving year window.
#' @param alt Alternative hypothesis for emergence testing ("two.sided", "less", or "greater"). Set to 'greater' by default, indicating checking for an decreasing trend in the time series of event ~ year.
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
#' # Create test dataset
#' year = rep(seq(1,30,1), 10)
#' event = round(rnorm(300, 100, 5))- rep(seq(1,30,1), 10)
#' dataset = data.frame(year, event); dataset = dataset[order(dataset$year),]
#'
#' # Calculate statistical time of emergence
#' ks_tope(dataset)



# Calculate time of emergence using KS Test
ks_tope = function(data,     # Input data
                   emt = 5,  # Emergence threshold (number of years for emergence)
                   alpha = 0.05, # Significance threshold for ks test
                   ks_t = 0.6, # KS test threshold
                   nboot = 100, # Number of bootstraps for ks testing
                   plot = T, # Plot results?
                   max_y = 0, # Maximum year window, 0 = deactivated
                   alt = 'greater', # KS Test sidedness
                   unemergence = F, # if F, all years after first emergence are set to emergence
                   model = 'linear', # model used for detrending. 'linear' or 'curvilinear'.
                   ... # Additional arguments to feed to lm()
){

  # Add error messages for invalid selections
  if(!alt %in% c("two.sided", "less", "greater")){stop('alt setting not recognized')}
  if(!model %in% c("linear", "curvilinear")){stop('model setting not recognized')}

  # Final year in first max_y set
  iyear = min(data$year) + max_y - 1

  # Use all years if may_y is deactivated or there are fewer than max_y years
  if((iyear >= max(data$year)) | (max_y == 0)){

    # Warn if baseline is moving but time series is too short
    if((max_y > 0) & (iyear >= max(data$year))){warning('Moving window is equal to or greater than time series length. Double-check max_y parameter.')}

    # Fit linear model (event timing)
    if(model == 'linear'){

      lm_ev = lm(event ~ year, data = data, ...)

    } else if(model == 'curvilinear'){

      lm_ev = lm(event ~ year + I(year^2), data = data, ...)

    }

    # prediction values at data points
    abs_preds = predict(lm_ev, newdata = data)

    # calculate baseline value
    baseline_val = predict(lm_ev, newdata = data.frame(year = min(data$year)))

    # Calculate adjustment
    adj = abs_preds - baseline_val

    # Detrend
    data_d = data
    data_d$event = data_d$event-adj

  } else {

    # Save years
    years = unique(data$year)

    # Run initial years
    data_f = dplyr::filter(data, year <= iyear)

    # Fit linear model (event timing)
    if(model == 'linear'){

      lm_ev = lm(event ~ year, data = data_f, ...)

    } else if(model == 'curvilinear'){

      lm_ev = lm(event ~ year + I(year^2), data = data_f, ...)

    }

    # calculate baseline value
    baseline_val = predict(lm_ev, newdata = data.frame(year = min(data_f$year)))

    # create adjustment container data frame
    slopes = data.frame(year = unique(data$year), adj = NA)

    # Calculate adjustment for years before moving window
    slopes[slopes$year<=iyear,'adj'] = predict(lm_ev, slopes[slopes$year<=iyear,])-baseline_val

    # Loop through remaining years
    for(i in which(is.na(slopes$adj))){

      # filter data to relevant years
      data_f = dplyr::filter(data, year %in% seq(slopes$year[i] - (max_y - 1), slopes$year[i], 1))

      # Fit linear model (event timing)
      if(model == 'linear'){

        lm_ev = lm(event ~ year, data = data_f, ...)

      } else if(model == 'curvilinear'){

        lm_ev = lm(event ~ year + I(year^2), data = data_f, ...)

      }

      # prediction values at data points
      abs_preds = predict(lm_ev,newdata = data.frame(year = max(data_f$year)))

      # calculate baseline value
      baseline_val = predict(lm_ev, newdata = data.frame(year = min(data_f$year)))

      # Enter adjustment
      slopes$adj[i] = abs_preds-baseline_val

    } # End loop through slopes

    # Generate match index
    ind = match(data$year, years)

    # Calculate adjustment
    adj = slopes$adj[ind]

    # Detrend
    data_d = data
    data_d$event = data_d$event-adj

  } # End else

  # Run ks tests

  # p value container
  ks_p = rep(NA, length(unique(data$year)))

  # Cycle years and perform KS test
  for(i in 1:length(unique(data$year))){

    # Run KS test
    ks_r = ks(data[data$year==unique(data$year)[i], 'event'],
              data_d[data_d$year==unique(data$year)[i], 'event'],
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
  ks_p_df = cbind.data.frame(year = unique(data$year), p = ks_p, emerged)

  # Plot if requested
  if(plot == T){

    # Plot p-value curve
    plot(ks_p ~ unique(data$year), type = 'l', pch = 16, lwd = 2,
         main = unique(data$species),
         ylab = 'Proportion of KS Tests Significant', xlab = 'Year')
    abline(h = ks_t, lty = 'dashed', col = 'blue') # Add significance threshold line
    abline(v = ks_p_df[min(which(emerged == 1)),'year'], lwd = 2, col = 'red') # Add Emerged Year
    text(y = 0.5, x = ks_p_df[min(which(emerged == 1)),'year'] + 4, label = paste('TOE:', ks_p_df[min(which(emerged == 1)),'year']), col = 'red')

  } # End plot if

  # Return data frame
  return(ks_p_df)

} # end KS emergence function
