########################################################################################
########################################################################################
########################################################################################
########################################################################################

# Simulate time series function

#' @title Phenology Time Series Simulation
#' @description Simulates future phenology timing and temperature time series for all species in a dataset, assuming a constant continued linear trend. Spread is simulated from the detrended original time series. It is recommended to average the result of many iterations of this function to account for random sampling.
#'
#'
#' @param data A data frame containing a phenological time series for a single species, which will be simulated forward. Must contain year (column named 'year'), timing of phenological event of interest, in Julian day (column named 'event'), environmental condition of interest (column named 'env'), and unique identifier for each individual time series for which ToPE and ToEE should be calculated (column named 'species').
#' @param simyears Future years to simulate.
#'
#' @returns A data frame containing the original data, as well as future simulated years.
#' @export
#'
#' @examples
#' # Set seed
#' set.seed(123)
#'
#' # Create test dataset
#' year = rep(seq(1,30,1), 10)
#' event = round(rnorm(300, 100, 5))- rep(seq(1,30,1), 10)
#' species = 1
#' env = (rnorm(300, 15, 0.5))
#' dataset = data.frame(year, species, event, env); dataset = dataset[order(dataset$year),]
#'
#' # Years to Simulate (10 more years)
#' years_sim = max(year+1):max(year+10)
#'
#' # Run simulation
#' sim_ts(dataset, years_sim)



sim_ts = function(data, simyears){

  # Simulating forwards
  data_sim = data

  for(i in 1:length(unique(data$species))){

    spp = unique(data$species)[i]

    sppdata = dplyr::filter(data, species == spp)

    # Fit linear model (arrival)
    lm_ev = lm(event ~ year, data = sppdata)

    # Pull out slope
    lm_ev_summ = summary(lm_ev)
    lm_ev_b = lm_ev_summ$coefficients['year','Estimate']

    # # # Plot LM
    # plot(event ~ year, data = sppdata, pch = 16)
    # abline(lm_ev, col = 'blue', lwd = 2)

    # Gather years
    years = unique(sppdata$year)

    # Adjust year to order
    years_0 = years-min(years)

    # Generate match index
    ind = match(sppdata$year, years)

    # Calculate adjustment
    adj = (ind-1)*lm_ev_b

    # Detrend
    data_d = sppdata
    data_d$event = data_d$event-adj

    # Fit linear model (temperature)
    lm_env = lm(env ~ year, data = sppdata)

    # Pull out slope
    lm_env_summ = summary(lm_env)
    lm_env_b = lm_env_summ$coefficients['year','Estimate']

    # # # Plot LM
    # plot(env ~ year, data = sppdata, pch = 16)
    # abline(lm_env, col = 'blue', lwd = 2)

    # Gather years
    years = unique(sppdata$year)

    # Adjust year to order
    years_0 = years-min(years)

    # Generate match index
    ind = match(sppdata$year, years)

    # Calculate adjustment
    adj = (ind-1)*lm_env_b

    # Detrend
    data_d$env = data_d$env-adj

    # Calculate median sample size
    mss = as.numeric(dplyr::group_by(data_d, year) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::summarize(median(n)))

    # Loop through years to add
    for(j in 1:length(simyears)){

      # Randomly sample rows from detrended data (without replacement)
      data_add = data_d[sample(1:nrow(data_d), mss, replace = F),]

      # Rename year
      data_add$year = simyears[j]

      # Adjust event to match trend
      data_add$event = data_add$event + ((simyears[j] - min(years))*lm_ev_b)

      # Adjust environment to match trend
      data_add$env = data_add$env + ((simyears[j] - min(years))*lm_env_b)

      # fill result
      data_sim = rbind(data_sim, data_add)

    } # end testing j loop

  } # end testing i loop

  # Return data with simulated years appended
  return(data_sim)

} # End sim_ts function

