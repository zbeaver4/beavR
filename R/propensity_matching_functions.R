#' Performs propensity score matching on a data.table and gives diagnostics
#'
#' Given a data.table with information about which entities should be matched against, performs propensity score matching using nearest neighbors and saves out diagnostic plots. Returns a list of those entities which are significantly different from the population
#' 
#' @usage run_propensity_matching(dt, cols_of_interest, reg_outcome_var, match_exclude_cols, bad_entities, entity_name, plot_fp, list_save_fp, list_name, control_match_num = 3, verbose = T, sample_size = NULL, entity_sample = NULL)
#' @param dt: data.table containin all the data
#' @param cols_of_interest: columns on which matching should occur
#' @param reg_outcome_var: character indicating the outcome column variable
#' @param match_exclude_cols: character vector of columns to exclude when performing the matching
#' @param bad_entities: a character vector indicating the entities for which propensity score matching should be performed
#' @param entity_name: character vector of the column in dt to be used subsetting down and identifiying 'bad_entities'
#' @param plot_fp: character filepath indicating where the psm matching plots should be stored
#' @param list_save_fp: character filepath indicating where the list of significant entities should be stored
#' @param control_match_num: The number of properties to be matched from the global population
#' @param verbose: Boolean indicating whether or not to give progress updates
#' @param sample_size: numeric indicating the sample size of the entity of interest to be taken for matching
#' @keywords propensity score matching
run_propensity_matching <- function(dt, cols_of_interest, reg_outcome_var, match_exclude_cols, bad_entities, entity_name, plot_fp, list_save_fp, list_name, control_match_num = 3, verbose = T, sample_size = NULL, entity_sample = NULL){
  
  #Creat the folder to hold results if it doesn't exist
  dir.create(plot_fp)
  
  #initialize list to store all findings as well as counter
  egregious_list <- list()
  tot_obs <- length(bad_entities)
  i <- 1
  for (egregious_offender in bad_entities){
    
    #Print out progress if desired
    if (verbose) print(paste("Starting ", egregious_offender, " (", i, " of ", tot_obs, ")", sep = ""))
    i <- i + 1
    
    #Initialize a list inside egregious list to hold info about each actor
    egregious_list[[egregious_offender]] <- list()
    
    #Copy data and create dummy variable for bad actor of interest
    temp_data <- copy(dt)
    temp_data[, c(egregious_offender) := ifelse(get(entity_name) == egregious_offender, 1, 0)]
    
    # Check to see if there's any columns with only a single unique value
    temp_data <- temp_data[, c(cols_of_interest, egregious_offender), with = F]
    
    #See if we need to sample from the remainder of the data set
    if (!is.null(sample_size)){
      
      #Get all the data related to the entity and a sample of the rest of the data.
      temp_data <- temp_data[c(which(get(egregious_offender) == 1),
                               sample(which(get(egregious_offender) == 0), sample_size))]
      
    }
    
    #See if we need to sample from the entity's observations
    if (!is.null(entity_sample)){
      
      #Get 4000 samples if the number of obs for the entity exceeds 4000
      if (nrow(temp_data[get(egregious_offender) == 1]) > 4000) {
        
        temp_data <- temp_data[c(which(get(egregious_offender) == 0),
                                 sample(which(get(egregious_offender) == 1), 4000))]
        
      }
      
    }
    
    # See if any columns need to be excluded because of singular values
    exclude_col <- temp_data[get(entity_name) == egregious_offender, names(.SD)[(sapply(.SD, FUN = function(x) length(unique(x)) == 1)) & (!names(.SD) %in% c(entity_name, egregious_offender))]]
    if (length(exclude_col) > 0) egregious_list[[egregious_offender]][['excluded_columns']] <- exclude_col
    
    #Create the formula to evaluate.
    covars_of_interest <- names(temp_data)[!names(temp_data) %in% c(egregious_offender, exclude_col, match_exclude_cols)]
    match_formula <- as.formula(gsub('init_vl_in_2013', 'log(init_vl_in_2013)', paste(egregious_offender, "~", paste(covars_of_interest, collapse = '+'))))
    
    # Run the matching using the columns that we controlled for in the regression (pre-treatment covariates) if possible
    # Note: There's a bug in the package such that no columns in the data set can have missing values.
    
    matched_df <- matchit(match_formula, 
                          data = temp_data,
                          method = 'nearest', 
                          ratio = control_match_num)
    
    # Store summary of the results.
    egregious_list[[egregious_offender]][['match_summary']] <- summary(matched_df)
    
    # Check out summary plots.
    #plot(matched_df, type = 'jitter')
    jpeg(paste(plot_fp, '/', egregious_offender, ".jpg", sep = ''))
    plot(matched_df, type = 'hist')
    dev.off()
    
    # Create a data set with only matched observations
    only_matched_data <- match.data(matched_df)
    #egregious_list[[egregious_offender]][['matched_df_head']] <- head(only_matched_data)
    
    # Run linear regression on the newly matched data. NOTE: we had to remove 'raze_ind' because it didn't occur in the 
    new_reg_formula <- as.formula(gsub('init_vl_in_2013', 'log(init_vl_in_2013)', paste(reg_outcome_var, "~", egregious_offender, "+", paste(covars_of_interest, collapse = '+'))))
    new_reg <- lm(new_reg_formula, data = only_matched_data)
    egregious_list[[egregious_offender]][['regression_summary']] <- summary(new_reg)
    egregious_list[[egregious_offender]][['regression_vif']] <- vif(new_reg)
    
  }
  
  assign(list_name, egregious_list)
  save(list = list_name, file = paste(list_save_fp, '/', list_name, '.Rdata', sep = ''))
  return(egregious_list)
}

#' Performs propensity score matching on a data.table and gives diagnostics while not including the observations associated with other 'bad entities'
#'
#' Given a data.table with information about which entities should be matched against, performs propensity score matching using nearest neighbors and saves out diagnostic plots. Returns a list of those entities which are significantly different from the population
#' 
#' @usage run_propensity_matching2(dt, cols_of_interest, reg_outcome_var, match_exclude_cols, bad_entities, entity_name, plot_fp, list_save_fp, list_name, control_match_num = 3, verbose = T, sample_size = NULL, entity_sample = NULL)
#' @param dt: data.table containin all the data
#' @param cols_of_interest: columns on which matching should occur
#' @param reg_outcome_var: character indicating the outcome column variable
#' @param match_exclude_cols: character vector of columns to exclude when performing the matching
#' @param bad_entities: a character vector indicating the entities for which propensity score matching should be performed
#' @param entity_name: character vector of the column in dt to be used subsetting down and identifiying 'bad_entities'
#' @param plot_fp: character filepath indicating where the psm matching plots should be stored
#' @param list_save_fp: character filepath indicating where the list of significant entities should be stored
#' @param control_match_num: The number of properties to be matched from the global population
#' @param verbose: Boolean indicating whether or not to give progress updates
#' @param sample_size: numeric indicating the sample size of the entity of interest to be taken for matching
#' @keywords propensity score matching
run_propensity_matching2 <- function(dt, cols_of_interest, reg_outcome_var, match_exclude_cols, bad_entities, entity_name, plot_fp, list_save_fp, list_name, control_match_num = 3, verbose = T, sample_size = NULL, entity_sample = NULL){
  
  #Creat the folder to hold results if it doesn't exist
  dir.create(plot_fp)
  
  #initialize list to store all findings
  egregious_list <- list()
  tot_obs <- length(bad_entities)
  i <- 1
  for (egregious_offender in bad_entities){
    
    #Print out progress if desired
    if (verbose) print(paste("Starting ", egregious_offender, " (", i, " of ", tot_obs, ")", sep = ""))
    i <- i + 1
    
    #Initialize a list inside egregious list to hold info about each actor
    egregious_list[[egregious_offender]] <- list()
    
    #Copy data and create dummy variable for bad actor of interest
    temp_data <- copy(dt)
    temp_data <- temp_data[!get(entity_name) %in% bad_entities[bad_entities != egregious_offender]]
    temp_data[, c(egregious_offender) := ifelse(get(entity_name) == egregious_offender, 1, 0)]
    
    # Check to see if there's any columns with only a single unique value
    temp_data <- temp_data[, c(cols_of_interest, egregious_offender), with = F]
    
    #See if we need to sample
    if (!is.null(sample_size)){
      
      #Get all the data related to the entity and a sample of the rest of the data.
      temp_data <- temp_data[c(which(get(egregious_offender) == 1),
                               sample(which(get(egregious_offender) == 0), sample_size))]
      
    }
    
    #See if we need to sample from the entity's observations
    if (!is.null(entity_sample)){
      
      #Get 10000 samples if the number of obs for the entity exceeds 10000
      if (nrow(temp_data[get(egregious_offender) == 1]) > 10000) {
        
        temp_data <- temp_data[c(which(get(egregious_offender) == 0),
                                 sample(which(get(egregious_offender) == 1), 10000))]
        
      }
      
    }
    
    exclude_col <- temp_data[get(entity_name) == egregious_offender, names(.SD)[(sapply(.SD, FUN = function(x) length(unique(x)) == 1)) & (!names(.SD) %in% c(entity_name, egregious_offender))]]
    if (length(exclude_col) > 0) egregious_list[[egregious_offender]][['excluded_columns']] <- exclude_col
    
    #Create the formula to evaluate.
    covars_of_interest <- names(temp_data)[!names(temp_data) %in% c(egregious_offender, exclude_col, match_exclude_cols)]
    match_formula <- as.formula(gsub('init_vl_in_2013', 'log(init_vl_in_2013)', paste(egregious_offender, "~", paste(covars_of_interest, collapse = '+'))))
    
    # Run the matching using the columns that we controlled for in the regression (pre-treatment covariates) if possible
    # Note: There's a bug in the package such that no columns in the data set can have missing values.
    matched_df <- matchit(match_formula, 
                          data = temp_data,
                          method = 'nearest', 
                          ratio = control_match_num)
    
    # Store summary of the results.
    egregious_list[[egregious_offender]][['match_summary']] <- summary(matched_df)
    
    # Check out summary plots.
    #plot(matched_df, type = 'jitter')
    jpeg(paste(plot_fp, '/', egregious_offender, ".jpg", sep = ''))
    plot(matched_df, type = 'hist')
    dev.off()
    
    # Create a data set with only matched observations
    only_matched_data <- match.data(matched_df)
    #egregious_list[[egregious_offender]][['matched_df_head']] <- head(only_matched_data)
    
    # Run linear regression on the newly matched data. NOTE: we had to remove 'raze_ind' because it didn't occur in the 
    new_reg_formula <- as.formula(gsub('init_vl_in_2013', 'log(init_vl_in_2013)', paste(reg_outcome_var, "~", egregious_offender, "+", paste(covars_of_interest, collapse = '+'))))
    new_reg <- lm(new_reg_formula, data = only_matched_data)
    egregious_list[[egregious_offender]][['regression_summary']] <- summary(new_reg)
    egregious_list[[egregious_offender]][['regression_vif']] <- vif(new_reg)
    
  }
  
  assign(list_name, egregious_list)
  save(list = list_name, file = paste(list_save_fp, '/', list_name, '.Rdata', sep = ''))
  
  return (egregious_list)
  
}