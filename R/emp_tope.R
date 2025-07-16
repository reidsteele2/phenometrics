########################################################################################
########################################################################################
########################################################################################
########################################################################################



# Empirical emergence tests

# Calculate time of emergence using Empirical Test
emp_tope = function(data,     # Input data
                    emt = 5,  # Emergence threshold (number of years for emergence)
                    plot = T, # Plot results?
                    max_y = 0, # Maximum year window, 0 = deactivated
                    alt = 'greater', # KS Test sidedness
                    quants = c(0.025, 0.975),
                    unemergence = F # if F, all years after first emergence are set to emergence
){

  # Final year in first max_y set
  iyear = min(data$year) + max_y - 1

  # Use all years if may_y is deactivated or there are fewer than max_y years
  if((iyear >= max(data$year)) | (max_y == 0)){

    # Fit linear model (arrival)
    lm_ev = lm(event ~ year, data = data)

    # Pull out slope
    lm_ev_summ = summary(lm_ev)
    lm_ev_b = lm_ev_summ$coefficients['year','Estimate']

    # # Plot LM
    # plot(event ~ year, data = data, pch = 16)
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
    data_d$event = data_d$event-adj

  } else {

    # Run initial years
    data_f = filter(data, year <= iyear)

    # Fit linear model (arrival)
    lm_ev = lm(event ~ year, data = data_f)

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
      data_f = filter(data, year %in% seq(slopes$year[i] - (max_y - 1), slopes$year[i], 1))

      # Fit linear model (arrival)
      lm_ev = lm(event ~ year, data = data_f)

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
    data_d$event = data_d$event-adj

  } # End else

  # Calculate annual means of data and detrended data
  data_m = group_by(data, year) %>% summarize(event = mean(event))
  data_d_m = group_by(data_d, year) %>% summarize(event = mean(event))

  # Calculate quantiles of interest
  for(i in 1:length(quants)){thresh = c(quantile(data_d_m$event, min(quants)), quantile(data_d_m$event, max(quants)))}

  # If alt = greater, check if the minimum threshold is greater than each individual year
  if(alt == 'greater'){ks_p = ifelse(min(thresh) > data_m$event, 1, 0)}

  # If alt = lesser, check if the maximum threshold is less than each individual year
  if(alt == 'less'){ks_p = ifelse(max(thresh) < data_m$event, 1, 0)}

  # if alt = two.sided, check both
  if(alt == 'two.sided'){ks_p = ifelse((min(thresh) > data_m$event)|(max(thresh) < data_m$event), 1, 0)}

  # Calculate emergence
  emerged = frollapply(ks_p, n = emt, function(x){all(x>=0.5)}, align = 'left')

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

} # end emp_emergence function
