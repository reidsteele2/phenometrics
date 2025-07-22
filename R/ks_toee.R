########################################################################################
########################################################################################
########################################################################################
########################################################################################
#' @title Statistical Time of Environmental Emergence (ToPE)
#'
#' @description Calculate Time of Environmental Emergence (ToEE) for a single phenology time series using statistical methodology.
#'
#' @param data A data frame containing the time series to test for ToEE. Must contain year (column named 'year') and environmental condition of interest (column named 'env').
#' @param emt Number of consecutive years of positive test results required to define emergence.
#' @param alpha Significance value for KS test.
#' @param ks_t Proportion of significant bootstrap KS tests required for a positive test result
#' @param nboot Number of KS test bootstraps samples.
#' @param plot If TRUE, will generate a plot of test result against year.
#' @param max_y Moving year window used to generate detrended counterfactual. If 0, moving window is deactivated, else length of moving year window.
#' @param alt Alternative hypothesis for emergence testing. Set to 'greater' by default, indicating checking for an decreasing trend in the time series of event ~ year.
#' @param unemergence If F, all years after first emergence are set to emerged. If T, calculation for each individual year is returned.
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
#' env = (rnorm(300, 15, 0.5)) + rep(seq(0.1,3,0.1),10)
#' dataset = data.frame(year, env); dataset = dataset[order(dataset$year),]
#'
#' # Calculate statistical time of emergence
#' ks_toee(dataset)



# Calculate time of decoupling using KS Test
ks_toee = function(data,    # Input data
                   emt = 5, # Emergence threshold (number of years for emergence)
                   alpha = 0.05, # Significance threshold for ks test
                   ks_t = 0.6, # KS test threshold
                   nboot = 100, # Number of bootstraps for ks testing
                   plot = T, # Plot results?
                   max_y = 0, # Maximum year window, 0 = deactivated
                   alt = 'less', # KS Test sidedness
                   unemergence = F # if F, all years after first emergence are set to emergence
){

  # Final year in first max_y set
  iyear = min(data$year) + max_y - 1

  # Use all years if may_y is deactivated or there are fewer than max_y years
  if((iyear >= max(data$year)) | (max_y == 0)){

    # Fit linear model (arrival)
    lm_ev = lm(env ~ year, data = data)

    # Pull out slope
    lm_ev_summ = summary(lm_ev)
    lm_ev_b = lm_ev_summ$coefficients['year','Estimate']

    # # Plot LM
    # plot(env ~ year, data = data, pch = 16)
    # abline(lm_ev, col = 'blue', lwd = 2)

    # Gather years
    years = unique(data$year)

    # Adjust year to order
    years_0 = years-min(years)

    # Generate match index
    ind = match(data$year, years)

    # Calculate adjustment
    adj = (ind-1)*lm_ev_b

    # Detrend
    data_d = data
    data_d$env = data_d$env-adj

    # Fit linear model (arrival)
    lm_ev_d = lm(env ~ year, data = data_d)

    # Pull out slope
    lm_ev_summ_d = summary(lm_ev_d)
    lm_ev_b_d = lm_ev_summ_d$coefficients['year','Estimate']

    # # Plot LM
    # plot(env ~ year, data = data_d, pch = 16)
    # abline(lm_ev_d, col = 'blue', lwd = 2)

  } else {

    # Run initial years
    data_f = dplyr::filter(data, year <= iyear)

    # Fit linear model (arrival)
    lm_ev = lm(env ~ year, data = data_f)

    # Pull out slope
    lm_ev_summ = summary(lm_ev)
    lm_ev_b = lm_ev_summ$coefficients['year','Estimate']

    # Gather years
    years = unique(data$year)

    # Adjust year to order
    years_0 = years-min(years)

    # create slope container data frame
    slopes = data.frame(year = years, ind = years_0, adj = NA)

    # Add relevant adjustments
    slopes$adj = ifelse(slopes$year <= iyear, slopes$ind*lm_ev_b, NA)

    # Loop through remaining years
    for(i in which(is.na(slopes$adj))){

      # filter data to relevant years
      data_f = dplyr::filter(data, year %in% seq(slopes$year[i] - (max_y - 1), slopes$year[i], 1))

      # Fit linear model (arrival)
      lm_ev = lm(env ~ year, data = data_f)

      # Pull out slope
      lm_ev_summ = summary(lm_ev)
      lm_ev_b = lm_ev_summ$coefficients['year','Estimate']

      # Enter adjustment
      slopes$adj[i] = lm_ev_b * (max_y-1)

    } # End loop through slopes

    # Generate match index
    ind = match(data$year, years)

    # Calculate adjustment
    adj = slopes$adj[ind]

    # Detrend
    data_d = data
    data_d$env = data_d$env-adj

    # Fit linear model (arrival)
    lm_ev_d = lm(env ~ year, data = data_d)

    # Pull out slope
    lm_ev_summ_d = summary(lm_ev_d)
    lm_ev_b_d = lm_ev_summ_d$coefficients['year','Estimate']

  }

  # Run ks tests

  # p value container
  ks_p = rep(NA, length(unique(data$year)))

  # Cycle years and perform KS test
  for(i in 1:length(unique(data$year))){

    # Run KS test
    ks_r = ks(data[data$year==unique(data$year)[i], 'env'],
              data_d[data_d$year==unique(data$year)[i], 'env'],
              alt = alt, alpha = alpha, nboot = nboot)

    # Fill ks_p
    ks_p[i] = ks_r

  } # End KS test loop

  # Calculate emergence
  emerged = data.table::frollapply(ks_p, n = emt, function(x){all(x>=ks_t)}, align = 'left')

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
    text(y = 0.5, x = ks_p_df[min(which(emerged == 1)),'year'] + 4, label = paste('TOD:', ks_p_df[min(which(emerged == 1)),'year']), col = 'red')

  } # End plot if

  # Return data frame
  return(ks_p_df)

} # end KS decoupling function
