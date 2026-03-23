########################################################################################
########################################################################################
########################################################################################
########################################################################################



#' @title Community ToPE and ToEE Metrics
#' @description Calculate ToPE and ToEE metrics for all species in a dataset
#'
#' @param data A data frame containing the time series to test for ToEE and ToPE. Must contain year (column named 'year'), timing of phenological event of interest, in Julian day (column named 'event'), environmental condition of interest (column named 'env'), and unique identifier for each individual time series for which ToPE and ToEE should be calculated (column named 'species').
#' @param em_alt Alternative hypothesis for ToPE testing ("two.sided", "less", or "greater"). Set to 'greater' by default, indicating checking for an decreasing trend in the time series of event ~ year.
#' @param dc_alt Alternative hypothesis for ToEE emergence testing ("two.sided", "less", or "greater"). Set to 'less' by default, indicating checking for an increasing trend in the time series of env ~ year.
#' @param method Methodology used for emergence calculations. Set to 'empirical' for empirical testing (default), or 'statistical' for statistical testing using the Kolmogorov-Smirnov test.
#' @param ... Additional arguments to feed to ToPE/ToEE functions (`ks_tope`, `ks_toee`, `emp_tope`, `emp_toee`)

#'
#' @returns A data frame containing classification results for each species and year determined by ToPE and ToEE test results. Designed to feed into class_by_year() and class_by_species() functions.
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
#' sp1 = data.frame(year, species, event, env)
#'
#' # Create test dataset 2
#' year = seq(1,30,1)
#' event = round(rnorm(30, 100, 5))
#' env = (rnorm(30, 15, 0.5)) + seq(0.1,3,0.1)
#' species = 2
#' sp2 = data.frame(year, species, event, env)
#'
#' # combine
#' dataset = as.data.frame(rbind(sp1, sp2))
#'
#' # Calculate empirical time of phenological emergence (ToPE)
#' community(dataset)



# Function to calculate ToPE and ToEE metrics across a community
community = function(data,
                     em_alt = 'greater', dc_alt = 'less', # KS Direction parameters
                     method = 'empirical', # empirical or statistical
                     ... # Additional arguments to feed to emergence functions
){

  # Emergence - species curves
  em_curve = NULL
  dc_curve = NULL

  # Loop through species
  for(i in 1:length(unique(data$species))){

    # Load in data
    sp = dplyr::filter(data, species == unique(data$species)[i])

    # Empirical/statistical
    if(method == 'statistical'){

      # Run individual curves
      em = cbind(species = unique(data$species)[i],
                 ks_tope(sp, plot = F, alt = em_alt, ...))
      dc = cbind(species = unique(data$species)[i],
                 ks_toee(sp, plot = F, alt = dc_alt, ...))

    }

    # Empirical/statistical
    if(method == 'empirical'){

      # Run individual curves
      em = cbind(species = unique(data$species)[i],
                 emp_tope(sp, plot = F, alt = em_alt, ...))
      dc = cbind(species = unique(data$species)[i],
                 emp_toee(sp, plot = F, alt = dc_alt, ...))

    }

    # Calculate classification identifier
    class_val = max(em$emerged, na.rm = T) + (max(dc$emerged, na.rm = T) * 2)
    if(class_val == -Inf){class_val = 0}

    # Calculate class in text
    class = switch(as.character(class_val),
                   '0' = 'no signal',
                   '1' = 'shifting',
                   '2' = 'decoupling',
                   '3' = 'combination')

    # Run curves
    em_curve = rbind(em_curve, em)
    dc_curve = rbind(dc_curve, cbind(dc, class))

  } # End species loop

  # Rename emerged coulumn to decoupled in dc_curve column
  colnames(dc_curve)[which(colnames(dc_curve) =='emerged')] = 'decoupled'

  # Join data frames
  all_curves = dplyr::left_join(em_curve, dc_curve, by = c('species', 'year'))

  # Add combination curve
  all_curves = all_curves %>% dplyr::mutate(combination = ifelse((emerged == 1) & (decoupled == 1), 1, 0)) %>%
    dplyr::mutate(shift = ifelse(combination == 1, 0, emerged), # Remove combined shifts
           decouple = ifelse(combination == 1, 0, decoupled), # Remove combined decouples
           unaffected = ifelse(shift+decouple+combination == 0, 1, 0)) # Add no climate change column

  # Condense all curves
  all_curves$class_c = all_curves$unaffected
  all_curves$class_c = ifelse(all_curves$shift == 1, 2, all_curves$class_c)
  all_curves$class_c = ifelse(all_curves$decouple == 1, 3, all_curves$class_c)
  all_curves$class_c = ifelse(all_curves$combination == 1, 4, all_curves$class_c)

  # Return
  return(all_curves)

} # End function
