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

  # Check for species column, make one if it does not exist
  if(!('species' %in% colnames(data))){data$species = 1}

  # Error if no event or env column
  if((!('event' %in% colnames(data))) & (!('env' %in% colnames(data)))){
    stop('no event or env column to simulate')
  }

  # Loop through species
  for(i in 1:length(unique(data$species))){

    # species name
    spp = unique(data$species)[i]

    # filter to species
    sppdata = dplyr::filter(data, species == spp)

    # Check if event exists
    if('event' %in% colnames(data)){

      # Fit linear model (arrival)
      lm_ev = lm(event ~ year, data = sppdata)

      # Pull out slope
      lm_ev_summ = summary(lm_ev)
      lm_ev_b = lm_ev_summ$coefficients['year','Estimate']

      # prediction values at data points
      abs_preds = predict(lm_ev, newdata = data)

      # calculate baseline value
      baseline_val = predict(lm_ev, newdata = data.frame(year = min(data$year)))

      # Calculate adjustment
      adj = abs_preds - baseline_val

      # Detrend
      data_d = data
      data_d$event = data_d$event-adj

    } # End event check if

    # Check if env exists
    if('env' %in% colnames(data)){

      # Fit linear model (temperature)
      lm_env = lm(env ~ year, data = sppdata)

      # Pull out slope
      lm_env_summ = summary(lm_env)
      lm_env_b = lm_env_summ$coefficients['year','Estimate']

      # prediction values at data points
      abs_preds = predict(lm_env, newdata = data)

      # calculate baseline value
      baseline_val = predict(lm_env, newdata = data.frame(year = min(data$year)))

      # Calculate adjustment
      adj = abs_preds - baseline_val

      # Detrend
      data_d = data
      data_d$env = data_d$env-adj

    } # End env check if

    # Calculate median sample size
    mss = as.numeric(dplyr::group_by(data_d, year) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::summarize(median(n)))

    # Gather minimum year
    minyear = min(data$year)

    # Loop through years to add
    for(j in 1:length(simyears)){

      # Randomly sample rows from detrended data (without replacement)
      data_add = data_d[sample(1:nrow(data_d), mss, replace = F),]

      # Rename year
      data_add$year = simyears[j]

      # Adjust event to match trend
      if('event' %in% colnames(data)){
        data_add$event = data_add$event + ((simyears[j] - minyear)*lm_ev_b)
      }

      # Adjust environment to match trend
      if('env' %in% colnames(data)){
        data_add$env = data_add$env + ((simyears[j] - minyear)*lm_env_b)
      }

      # fill result
      data_sim = rbind(data_sim, data_add)

    } # end testing j loop

  } # end testing i loop

  # Return data with simulated years appended
  return(data_sim)

} # End sim_ts function

