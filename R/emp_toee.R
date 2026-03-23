########################################################################################
########################################################################################
########################################################################################
########################################################################################


#' @title Empirical Time of Environmental Emergence (ToEE)
#' @description Calculate Time of Environmental Emergence (ToEE) for a single phenology time series
#'
#' @param data A data frame containing the time series to test for ToEE. Must contain year (column named 'year') and environmental condition of interest (column named 'env').
#' @param emt Number of consecutive years of positive test results required to define emergence.
#' @param plot If TRUE, will generate a plot of test result against year.
#' @param max_y Length of baseline window used to generate detrended counterfactual. If 0 uses all years, else uses maximum of max_y years.
#' @param alt Alternative hypothesis for emergence testing ("two.sided", "less", or "greater"). Set to 'less' by default, indicating checking for an increasing trend in the time series of env ~ year.
#' @param quants Quantiles of the detrended data used to determine the emergence threshold
#' @param unemergence If F, all years after first emergence are set to emerged. If T, calculation for each individual year is returned.
#' @param model Type of model used for detrending. 'linear' or 'curvilinear'. 'linear' uses a simple linear model, 'curvilinear' adds an x^2 term.
#' @param baseline Static or moving baseline used to generate detrended counterfactual. If 'static', same baseline is used for all years tested. If 'moving', baseline moves with test year.
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
#' year = seq(1,30,1)
#' env = (rnorm(30, 15, 0.5)) + seq(0.1,3,0.1)
#' dataset = data.frame(year, env)
#'
#' # Calculate empirical time of phenological emergence (ToPE)
#' emp_toee(dataset)



emp_toee = function(data,    # Input data
                    emt = 5,  # Emergence threshold (number of years for emergence)
                    plot = T, # Plot results?
                    max_y = 0, # Maximum year window, 0 = deactivated
                    alt = 'less', # KS Test sidedness
                    quants = c(0.25, 0.75),
                    unemergence = F, # if F, all years after first emergence are set to emergence
                    model = 'linear', # model used for detrending. 'linear' or 'curvilinear'.
                    baseline = 'static', # Static or moving baseline. 'static' or 'moving'.
                    ... # Additional arguments to feed to lm()
){

  # Add error messages for invalid selections
  if(!alt %in% c("two.sided", "less", "greater")){stop('alt setting not recognized')}
  if(!model %in% c("linear", "curvilinear")){stop('model setting not recognized')}
  if(!baseline %in% c("static", "moving")){stop('baseline setting not recognized')}

  # Final year in first max_y set
  iyear = min(data$year) + max_y - 1

  # Use all years if may_y is deactivated or there are fewer than max_y years
  if((iyear >= max(data$year)) | (baseline == 'static')){

    # Warn if baseline is moving
    if(baseline == 'moving'){warning('Moving window is equal to or greater than time series length. Double-check max_y parameter.')}

    # If max_y is set, filter years
    if(max_y > 0){

      # Filter to years less than or equal to iyear
      data_base = data[data$year <= iyear,]

    } else {

      # Take all years
      data_base = data

    } # End max_y if/else

    # Fit linear model (environment)
    if(model == 'linear'){

      lm_ev = lm(env ~ year, data = data_base, ...)

    } else if(model == 'curvilinear'){

      lm_ev = lm(env ~ year + I(year^2), data = data_base, ...)

    }

    # prediction values at data points
    abs_preds = predict(lm_ev, newdata = data_base)

    # calculate baseline value
    baseline_val = predict(lm_ev, newdata = data.frame(year = min(data_base$year)))

    # Calculate adjustment
    adj = abs_preds - baseline_val

    # Detrend
    data_d = data_base
    data_d$env = data_d$env-adj

  } else if(baseline == 'moving'){

    # Save years
    years = unique(data$year)

    # set data for baseline
    data_base = data

    # Run initial years
    data_f = dplyr::filter(data, year <= iyear)

    # Fit linear model (environment)
    if(model == 'linear'){

      lm_ev = lm(env ~ year, data = data_f, ...)

    } else if(model == 'curvilinear'){

      lm_ev = lm(env ~ year + I(year^2), data = data_f, ...)

    }

    # calculate baseline value
    baseline_val = predict(lm_ev, newdata = data.frame(year = min(data_f$year)))

    # create adjustment container data frame
    slopes = data.frame(year = unique(data_base$year), adj = NA)

    # Calculate adjustment for years before moving window
    slopes[slopes$year<=iyear,'adj'] = predict(lm_ev, slopes[slopes$year<=iyear,])-baseline_val

    # Loop through remaining years
    for(i in which(is.na(slopes$adj))){

      # filter data to relevant years
      data_f = dplyr::filter(data, year %in% seq(slopes$year[i] - (max_y - 1), slopes$year[i], 1))

      # Fit linear model (environment)
      if(model == 'linear'){

        lm_ev = lm(env ~ year, data = data_f, ...)

      } else if(model == 'curvilinear'){

        lm_ev = lm(env ~ year + I(year^2), data = data_f, ...)

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
    data_d = data_base
    data_d$env = data_d$env-adj

  } # End else

  # If baseline is static, calculate emergence together
  if(baseline == 'static'){

    # Calculate annual means of data and detrended data
    data_m = dplyr::group_by(data, year) %>% dplyr::summarize(env = mean(env))
    data_d_m = dplyr::group_by(data_d, year) %>% dplyr::summarize(env = mean(env))

    # Calculate quantiles of interest
    for(i in 1:length(quants)){thresh = c(quantile(data_d_m$env, min(quants)), quantile(data_d_m$env, max(quants)))}

    # If alt = greater, check if the minimum threshold is greater than each individual year
    if(alt == 'greater'){ks_p = ifelse(min(thresh) > data_m$env, 1, 0)}

    # If alt = lesser, check if the maximum threshold is less than each individual year
    if(alt == 'less'){ks_p = ifelse(max(thresh) < data_m$env, 1, 0)}

    # if alt = two.sided, check both
    if(alt == 'two.sided'){ks_p = ifelse((min(thresh) > data_m$env)|(max(thresh) < data_m$env), 1, 0)}

    # If baseline is moving, calculate thresholds for each window
  } else if (baseline == 'moving'){

    # initialize ks_p
    ks_p = rep(NA, length(unique(data$year)))

    # loop through years starting highest year less than or equal to iyear
    for(y in unique(data$year)[unique(data$year) >= max(unique(data$year)[unique(data$year) <= iyear])]){

      # Calculate minimum year
      minyear = y - max_y + 1

      # Calculate annual means of data and detrended data
      data_m = dplyr::group_by(data[data$year %in% (minyear:y),], year) %>% dplyr::summarize(env = mean(env))
      data_d_m = dplyr::group_by(data_d[data_d$year %in% (minyear:y),], year) %>% dplyr::summarize(env = mean(env))

      # Calculate quantiles of interest
      for(i in 1:length(quants)){thresh = c(quantile(data_d_m$env, min(quants)), quantile(data_d_m$env, max(quants)))}

      # Calculate index for filling test values
      test_ind = which((unique(data$year) <= y) & (is.na(ks_p)))

      # If alt = greater, check if the minimum threshold is greater than each individual year
      if(alt == 'greater'){ks_p[test_ind] = ifelse(min(thresh) > data_m[data_m$year == unique(data$year)[test_ind],'env'], 1, 0)}

      # If alt = lesser, check if the maximum threshold is less than each individual year
      if(alt == 'less'){ks_p[test_ind] = ifelse(max(thresh) < data_m[data_m$year == unique(data$year)[test_ind],'env'], 1, 0)}

      # if alt = two.sided, check both
      if(alt == 'two.sided'){ks_p[test_ind] = ifelse((min(thresh) > data_m[data_m$year == unique(data$year)[test_ind],'env'])|
                                                       (max(thresh) < data_m[data_m$year == unique(data$year)[test_ind],'env']), 1, 0)}

    } # End iyear for loop

  } # End moving baseline if

  # Calculate emergence
  emerged = data.table::frollapply(ks_p, N = emt, FUN = function(x){all(x>=0.5)}, align = 'left')

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
         ylab = 'Threshold Value Exceeded', xlab = 'Year')
    abline(v = ks_p_df[min(which(emerged == 1)),'year'], lwd = 2, col = 'red') # Add Emerged Year
    text(y = 0.5, x = ks_p_df[min(which(emerged == 1)),'year'] + 4, label = paste('TOE:', ks_p_df[min(which(emerged == 1)),'year']), col = 'red')

  } # End plot if

  # Return data frame
  return(ks_p_df)

} # end emp_decoupling function
