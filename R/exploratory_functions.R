#' Find columns that uniquely identify a row in a dataframe
#'
#' This function returns the name of each column which uniquely identifies a row in the data.frame
#' 
#' @usage find_unique_id(df)
#' @param df: dataframe
#' @keywords primary key, unique id
#' @examples
#' fake_df <- data.frame(a = 1:10, b = rep(1:5, 2))
#' find_unique_id(fake_df)
find_unique_id <- function(df) {
  unique_list <- list()
  df_names <- names(df)
  
  for (i in 1:ncol(df)) {
    if (length(unique(df[,i])) == nrow(df)) unique_list <- c(unique_list, df_names[i])
  }
  
  return(unlist(unique_list))
}

#' Take a sample of rows from a table in a database
#'
#' This function returns a data.frame corresponding to rows in a database table that have been (somewhat) randomly sampled.
#' 
#' @usage get_table_sample(conn, table_name, samp_num)
#' @param conn: ODBC database connection handle
#' @param table_name: character vector indicating the name of the table to be sampled from
#' @param samp_num: integer indicating the number of rows to sample
#' @keywords sampling, database
#' @examples
#' get_table_sample(conn, 'jerrys_table', 15) # for sampling 15 rows from jerrys_table
get_table_sample <- function(conn, table_name, samp_num) {
  samp <- RODBC::sqlQuery(conn, paste("select * from ", table_name, " TABLESAMPLE (", samp_num, " rows);"))
  samp
}

#' Gets all columns that are unique identifiers from a list of tables
#'
#' This function returns a data.frame that gives all unique columns found in the tables provided from a given database
#' 
#' @usage get_unique_ids(table_char_vect, conn, samp_num)
#' @param table_char_vect: a character vector containing the table names to inspect for unique columns
#' @param conn: ODBC database handle
#' @param samp_num: integer indicating the number of rows to sample
#' @keywords sampling, database
#' @examples
#' get_unique_ids(c('jerrys_table', 'jans_table', conn, 1000) # for obtaining unique identifiers from jan and jerrys table by checking 1000 randomly sample rows from each table
get_unique_ids <- function(table_char_vect, conn, samp_num) {
  
  #get initial data frame
  initial_df <- get_table_sample(conn, table_char_vect[1], samp_num)
  unique_cols <- find_unique_id(initial_df)
  ifelse(!is.null(unique_cols), accum_DF <- data.frame(table=rep(table_char_vect[1], length(unique_cols)), unique_id = unique_cols), accum_df <- data.frame(table = table_char_vect[1], unique_id = NA))
  
  if (length(table_char_vect>1)) {
    
    for (table1 in table_char_vect[2:length(table_char_vect)]) {
      table_df <- get_table_sample(conn, table1, 1000)
      unique_cols <- find_unique_id(table_df)
      ifelse(!is.null(unique_cols), new_df <- data.frame(table = rep(table1, length(unique_cols)), unique_id = unique_cols), new_df <- data.frame(table = table1, unique_id = NA))
      accum_df <- rbind(accum_df, new_df)
    }
    
    accum_df
  }
  else {
    initial_df
  }
}

#' converts all factor variables in a data.frame to character variables
#'
#' This function returns the original data.frame with all factor variables converted to character variables
#' 
#' @usage convert_facts_to_chars(df)
#' @param df: the data.frame to have its factor columns converted to characters
#' @keywords type conversion
#' @examples
#' fake_df <- data.frame(alpha = as.factor(c('a', 'b', 'c', 'd')), beta = 1:4)
#' str(fake_df)
#' str(convert_facts_to_chars(fake_df))
convert_facts_to_chars <- function(df){
  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)
  df
}

#' Subsets to columns with more than one unique value
#'
#' This function returns the original data.frame without columns that only have one value for all rows in the data.frame
#' 
#' @usage eliminate_single_value_cols(df)
#' @param df: the data.frame to have its single-value columns removed
#' @examples
#' fake_df <- data.frame(a = rep(1, 5), b = (1:5), c = rep('a', 5), d = c('a', 'b', 'c', 'd', 'e'))
#' eliminate_single_value_cols(fake_df)
eliminate_single_value_cols <- function(df){
  
  #Get names of columns that only have a one unique value
  all_single_value <- mapply(df, FUN = function(x) length(unique(x)) == 1)
  exclude_names <- names(all_single_value)[all_single_value == T]
  
  #Subset df based on whether df or dt
  if ('data.table' %in% class(df)){
    
    df <- df[, names(df)[!names(df) %in% exclude_names], with = F]
    
  }
  
  else{
    
    df <- df[, names(df)[!names(df) %in% exclude_names]]
    
  }
  
  #return the subset df
  return(df)
  
}

#' Gets the number of rows for a list of tables
#'
#' Given an odbc handle and a vector of table names, this function returns a named vector containing the number rows in each table
#' 
#' @usage get_table_lengths(conn, table_names)
#' @param conn: odbc handle
#' @param table_names: character vector indicating the names of tables in the database for which the number of rows is desired
#' @keywords database exploration
#' @examples 
#' get_table_lengths(conn, c('best_table', 'next_best_table'))
get_table_lengths <- function(conn, table_names){
  
  table_df <- data.frame(table = table_names, length = rep(0, length(table_names)))
  table_df$length <- table_df$length
  j <- 1
  for (table in table_names){
    
    table_df$length[j] <- RODBC::sqlQuery(conn, paste("select count(*) from ", table, ";", sep = ""))
    j <- j + 1
  }
  table_df$length <- unlist(table_df$length)
  table_df
  
}

#' Get all table, column, and view metadata from SQL Server database
#'
#' Given an odbc handle this function gras all the metadata associated with a database at the table/column level and returns it as a data.frame
#' 
#' @usage get_all_table_info(conn, table_names)
#' @param conn: odbc handle
#' @keywords database exploration
#' @examples 
#' get_all_table_info(conn)
get_all_table_info <- function(conn){
  require(ODBC, quietly = T)
  sqlQuery(conn, "SELECT a .[name] as 'Table' ,
           b.[name] as 'Column',      
           c.[name] as 'Datatype',
           
           b.[length] as 'Length',
           
           CASE
           
           WHEN b .[cdefault] > 0 THEN d. [text]
           
           ELSE NULL
           
           END as 'Default',
           
           CASE
           
           WHEN b .[isnullable] = 0 THEN 'No'
           
           ELSE 'Yes'
           
           END as 'Nullable'
           
           FROM sysobjects a
           
           INNER JOIN syscolumns b
           
           ON a .[id] = b .[id]
           
           INNER JOIN systypes c
           
           ON b .[xtype] = c .[xtype]
           
           LEFT JOIN syscomments d
           
           ON b .[cdefault] = d .[id]
           
           WHERE a .[xtype] = 'u'
           
           -- 'u' for user tables, 'v' for views.
           
           --and a.[name]='table name'
           
           AND a .[name] <> 'dtproperties'
           
           ORDER BY a. [name],b .[colorder]")
}

#' Perform left outer join of data.tables with diagnostics
#'  
#'  Given two data.tables, this function performs a left outer join and reports on any cartesian product, what percentage matched up, etc.
#' 
#' @usage dt_left_outer(dt1, dt2, key1, key2 = key1, dt2_subset_cols = NULL, cartesian = T)
#' @param dt1: First data.table (the one "on the left")
#' @param dt2: Second data.table (the one "on the right")
#' @param key1: The column (character) from dt1 on which the join is being performed
#' @param key2: The column (character) from dt2 on which the join is being performed
#' @param dt2_subset_cols: Character vector of the columns to keep in dt2 after the join is performed
#' @keywords database exploration
#' @examples 
#' first_dt <- data.table (a = 1:5, b = c('a', 'b', 'c', 'd', 'e'))
#' second_dt <- data.table (a = 3:7, c = c('yes', 'no', 'no', 'yes', 'yes'))
#' dt_left_outer (first_dt, second_dt, 'a')
dt_left_outer <- function(dt1, dt2, key1, key2 = key1, dt2_subset_cols = NULL, cartesian = T){
  
  require(data.table, quietly = T)
  
  #Set the key
  setkeyv(dt1, key1)
  setkeyv(dt2, key2)
  
  #Check to see if the second data.table should be subset
  if(!is.null(dt2_subset_cols)) dt2 <- dt2[, dt2_subset_cols, with = F]
  
  #Join the two data.tables
  new_dt <- dt2[dt1, allow.cartesian = cartesian]
  
  #Report on the matching rows and any duplication of rows that resulted from the join
  if (nrow(new_dt) != nrow(dt1)) print(paste("Number of resulting rows exceeded those in original data.table by ",
                                             nrow(new_dt) - nrow(dt1), " (",
                                             round((nrow(new_dt) - nrow(dt1)) / nrow(dt1) * 100, 3), "%)", sep = ""))
  
  matches <- sum(dt1[, get(key1)] %in% unique(dt2[, get(key2)]))
  print(paste(matches, " IDs (", round(matches / nrow(dt1), 3) * 100, "%) in the original data.table have matches in the second data.table"), sep = '')
  
  return(new_dt)
  
}

#' Trim leading whitespace
#'
#' Given a character vector, trims all leading whitespace from each component of the vector
#' 
#' @usage trim.leading(x)
#' @param x: character vector
#' @keywords strings
#' @examples 
#' trim.leading(c('  hello', 'no'))
trim.leading <- function (x)  sub("^\\s+", "", x)

#' Trim trailing whitespace
#'
#' Given a character vector, trims all trailing whitespace from each component of the vector
#' 
#' @usage trim.trailing(x)
#' @param x: character vector
#' @keywords strings
#' @examples 
#' trim.trailing(c('hello  ', 'no'))
trim.trailing <- function (x) sub("\\s+$", "", x)

#' Trim trailing whitespace
#'
#' Given a character vector, trims all leading and trailing whitespace from each component of the vector
#' 
#' @usage trim(x)
#' @param x: character vector
#' @keywords strings
#' @examples 
#' trim(c('hello  ', '  no'))
trim <- function (x) gsub("^\\s+|\\s+$", "", x)