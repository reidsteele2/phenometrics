########################################################################################
########################################################################################
########################################################################################
########################################################################################


#' Internal function for running bootstrapped KS tests
#'
#' @param data1 test dataset
#' @param data2 control dataset
#' @param alt alternative hypothesis for KS test
#' @param nboot number of bootstrap samples
#' @param alpha significance value
#'
#' @returns internal output for statistical emergence tests
#' @export
#'
#' @examples
#' 1+1

# Run KS testing
ks = function(data1, data2, alt, nboot = 100, alpha = 0.05){

  # Results carrier object
  ks_sig = rep(NA, nboot)

  # Bootstrap
  for(i in 1:nboot){

    # Create bootstrap sample for test dataset
    data_b = sample(data2, length(data2), replace = T)

    # Run KS test
    ks = ks.test(data1,
                 data_b,
                 alternative = alt)

    # Fill ks test significance
    ks_sig[i] = ks$p.value < alpha

  } # end bootstrap loop

  # Return proportion significant
  ks_out = sum(ks_sig) / nboot

  # return result
  return(ks_out)

}
