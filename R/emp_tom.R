########################################################################################
########################################################################################
########################################################################################
########################################################################################



#' @title Empirical Time of Mismatch (ToM)
#' @description Calculate Time of Mismatch (ToM) between two time series
#'
#' @param sp1 A data frame containing the time series to test for ToM. Must contain year (column named 'year'), timing of phenological event of interest, in Julian day (column named 'event').
#' @param sp2 A data frame containing the time series against which sp1 is compared for ToM. Must contain year (column named 'year'), timing of phenological event of interest, in Julian day (column named 'event').
#' @param emt Number of consecutive years of positive test results required to define emergence.
#' @param plot If TRUE, will generate a plot of test result against year.
#' @param max_y Moving year window used to generate counterfactual. If 0, moving window is deactivated, else length of moving year window.
#' @param alt Alternative hypothesis for emergence testing. Set to 'two.sided' by default, indicating checking for sp1 event timing to to be either greater than or less than sp2.
#' @param quants Quantiles of the counterfactual data used to determine the emergence threshold
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
#' year = seq(1,30,1)
#' event = round(rnorm(30, 100, 5))- seq(1,30,1)
#' species = 1
#' sp1 = data.frame(year, species, event)
#'
#' # Create test dataset
#' year = seq(1,30,1)
#' event = round(rnorm(30, 100, 5))
#' species = 2
#' sp2 = data.frame(year, species, event)
#'
#' # Calculate empirical time of phenological emergence (ToPE)
#' emp_tom(sp1, sp2)



# Calculate time of mismatch using Empirical Test
emp_tom = function(sp1,     # Input data for species 1 (test species)
                   sp2,     # Input data for species 2 (comparison species)
                   emt = 5,  # Emergence threshold (number of years for emergence)
                   plot = T, # Plot results?
                   max_y = 0, # Maximum year window, 0 = deactivated
                   alt = 'two.sided', # KS Test sidedness
                   quants = c(0.025, 0.975),
                   unemergence = F # if F, all years after first emergence are set to emergence
){

  # gather year range
  sp1_yr = range(sp1$year)
  sp2_yr = range(sp2$year)

  # Total range
  yrange = c(max(c(sp1_yr[1], sp2_yr[1])), min(sp1_yr[2], sp2_yr[2]))

  # Filter to same years
  sp1 = as.data.frame(dplyr::filter(sp1, (year >= min(yrange)) &  (year <= max(yrange))))
  sp2 = as.data.frame(dplyr::filter(sp2, (year >= min(yrange)) &  (year <= max(yrange))))

  # Fit linear model (arrival)
  lm_ev_sp1 = lm(event ~ year, data = sp1)

  # Pull out slope
  lm_ev_summ_sp1 = summary(lm_ev_sp1)
  lm_ev_b_sp1 = lm_ev_summ_sp1$coefficients['year','Estimate']

  # # Plot LM
  # plot(event ~ year, data = sp1, pch = 16)
  # abline(lm_ev, col = 'blue', lwd = 2)

  # Fit linear model (arrival)
  lm_ev_sp2 = lm(event ~ year, data = sp2)

  # Pull out slope
  lm_ev_summ_sp2 = summary(lm_ev_sp2)
  lm_ev_b_sp2 = lm_ev_summ_sp2$coefficients['year','Estimate']

  # # Plot LM
  # plot(event ~ year, data = sp2, pch = 16)
  # abline(lm_ev_d, col = 'blue', lwd = 2)



  # Gather years
  years = seq(yrange[1], yrange[2], 1)

  # Adjust year to order
  years_0 = years-min(years)

  # Generate match index
  ind_sp1 = match(sp1$year, years)
  ind_sp2 = match(sp2$year, years)

  # Calculate adjustment
  adj_sp1 = (ind_sp1-1)*(lm_ev_b_sp1)
  adj_sp1tosp2 = (ind_sp1-1)*(lm_ev_b_sp2)
  # adj_sp2 = (ind_sp2-1)*(lm_ev_b_sp2)
  # adj_sp2tosp1 = (ind_sp2-1)*(lm_ev_b_sp1)

  # Swap Species trends
  sp1_mm = sp1
  sp1_mm$event = sp1_mm$event - adj_sp1 + adj_sp1tosp2
  # sp2_mm = sp2
  # sp2_mm$event = sp2_mm$event - adj_sp2 + adj_sp2tosp1

  # Calculate annual means of data and retrended data
  sp1_mean = dplyr::group_by(sp1, year) %>% dplyr::summarize(event = mean(event))
  sp1_mm_mean = dplyr::group_by(sp1_mm, year) %>% dplyr::summarize(event = mean(event))

  # Calculate quantiles of interest
  for(i in 1:length(quants)){thresh = c(quantile(sp1_mm_mean$event, min(quants)), quantile(sp1_mm_mean$event, max(quants)))}

  # If alt = greater, check if the minimum threshold is greater than each individual year
  if(alt == 'greater'){ks_p = ifelse(min(thresh) > sp1_mean$event, 1, 0)}

  # If alt = lesser, check if the maximum threshold is less than each individual year
  if(alt == 'less'){ks_p = ifelse(max(thresh) < sp1_mean$event, 1, 0)}

  # if alt = two.sided, check both
  if(alt == 'two.sided'){ks_p = ifelse((min(thresh) > sp1_mean$event)|(max(thresh) < sp1_mean$event), 1, 0)}

  # Calculate emergence
  emerged = data.table::frollapply(ks_p, n = emt, function(x){all(x>=0.5)}, align = 'left')

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
         ylab = 'Threshold Value Exceeded', xlab = 'Year')
    abline(v = ks_p_df[min(which(emerged == 1)),'year'], lwd = 2, col = 'red') # Add Emerged Year
    text(y = 0.5, x = ks_p_df[min(which(emerged == 1)),'year'] + 4, label = paste('TOM:', ks_p_df[min(which(emerged == 1)),'year']), col = 'red')

  } # End plot if

  # Return data frame
  return(ks_p_df)

} # end emp_mismatch function
