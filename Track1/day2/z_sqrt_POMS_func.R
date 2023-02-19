
#' Normalize skewed data by calculating square root of POMS-transformed data & getting z-scores for all values
#'
#' @param vector a vector containing values you want to transform
#' @param sample_min sample size minimum or smallest possible value (e.g. 0 for RTs)
#' @param sample_max sample size maximum or largest possible value
#' 
#' @return returns sqrt(POMS)-transformed values


z_sqrt_POMS <- function(vector, sample_min = NA, sample_max = NA) {
    
    # Normalize skewed data
    
    # For outlier exclusion as described in Cousineau & Chartier, 2010
    # I found this approach in this paper, where it's recommended as the 
    # best outlier exclusion method for skewed data
    # https://doi.org/10.3389/fpsyg.2021.675558
      
    # "For each transformed value, the square root of the untransformed value minus the minimum value of the sample divided 
    # through the sample range is calculated. The fraction bounds all values between 0 and 1, 
    # while the square root enlarges small values (Cousineau and Chartier, 2010).
    # Afterwards, these values are z-transformed and values exceeding a particular z-score (e.g., 2 or 3) 
    # are excluded. For the present simulations, we excluded RTs associated with a z-score larger/smaller than ±2 as outliers." 
    # (Berger & Kiefer, 2021)
    
    # Long story short:
    # transf_val = sqrt (  ( x - sample_min ) / ( sample_max - sample_min)  )
    # --> after this, z-transform all values and values exceeding ± 2 (or 3, but they used 2 in the paper) are excluded
      # In this function I do everything up to the z-transformation (the z-transform included)
    
    # This is basically a POMS (Little (2013), read in Moeller (2013)) transformation where you get the square root of the output afterwards and z-transform everything.
    
    # Careful, you have to to this over all participants & conditions. 
    # Sample means all your RTs here, so everything gets "pulled" into the same range.
    # If I do this for each subject & condition separately, I can't compare means anymore, 
    # because every subset of data had its own scale.
    
    
    # get min & max value (if not defined)
    if (is.na(sample_min)){
      sample_min <- min(vector) # get minimum of sample
    }

  
    if (is.na(sample_max)){
      sample_max <- max(vector) # get minimum of sample
    }
  

    # do z-transform of sqrt(POMS) transform
    vals_normalized <- scale(sqrt((vector - sample_min) / (sample_max - sample_min)))

    # return transformed values
    return(vals_normalized)
}

