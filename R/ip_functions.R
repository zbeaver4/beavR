#' Get bits of the mask given the total bits
#'
#' This function is a helper to maskIP below.
#' 
#' @usage mask(bits, totalBits)
#' @param bits: number of bits in mask you're looking for
#' @param totalBits: the total number of bits
#' @keywords ip, mask
mask <- function (bits, totalBits)
{
  require(bitops, quietly = T)
  m <- c(rep (1, bits), rep (0, totalBits-bits))
  
  Reduce (function (x, y) (x + y) * 2, m[-totalBits], 0) + m[totalBits]
}

#' Converting an IP Address based on its mask
#'
#' This function takes in an ip address and a mask length (number of bits) and converts the IP address to its form for the that mask length.
#' 
#' @usage maskIP(ipAddr, maskLen, fieldBits=8, fieldSep=".")
#' @param ipAddr: character form of the ip address to be converted
#' @param maskLen: integer indicating the mask length to which the ip address should be converted
#' @param fieldBits: integer indicating the number of bits in each field
#' @param fieldSep: character indicating what should be used to separate fields
#' @keywords ip, mask
#' @examples
#' maskIP ("192.7.16.42", 16)  #returns "192.7.0.0"
maskIP <- function (ipAddr, maskLen, fieldBits=8, fieldSep=".")
{
  require(bitops, quietly = T)
  fields <- as.numeric (strsplit (ipAddr, fieldSep, fixed=TRUE)[[1]])
  numFields <- length (fields)
  
  words <- maskLen %/% fieldBits
  bits  <- maskLen %%  fieldBits
  
  if (bits != 0)
  {
    wordMask <- mask (bits, fieldBits)
    leftover <- numFields - words - 1
  }
  else
  {
    wordMask <- NULL
    leftover <- numFields - words      
  }
  
  maskedFields <- fields[0:words]  # Yes, the 0 is correct and necessary here
  
  if (!is.null (wordMask))
  { maskedFields <- append (maskedFields, bitAnd (fields[words+1], wordMask)) }
  
  if (leftover > 0)
  { maskedFields <- append (maskedFields, rep (0, leftover)) }
  
  paste (maskedFields, collapse=fieldSep)
}

#' Splitting a column containing IP address and mask into separate columns for each
#'
#' This function takes a data.frame/table and a column name that indicates a column containing a combination of ip address and mask (e.g. '192.168.1.101/16') and splits this column into two separate columns. Returns a data.table.
#' 
#' @usage split_ip_network(dt, net_name)
#' @param dt: data.frame or data.table
#' @param net_name: character indicating the name of the column containing the ip address and mask
#' @keywords ip, mask
#' @examples
#' fake_dt <- data.table::data.table(ip = c('192.168.1.101/16', '10.10.2.201/24'), b = 1:2)
#' split_ip_network(fake_dt, 'ip')
split_ip_network <- function(dt, net_name){
  
  require(data.table, quietly = T)
  
  if(!'data.table' %in% class(dt)) dt <- data.table(dt)
  
  dt$ip_base <- sapply(as.character(dt[, get(net_name)]), FUN = function(x) strsplit(x, '/')[[1]][1])
  dt$mask_num <- sapply(as.character(dt[, get(net_name)]), FUN = function(x) strsplit(x, '/')[[1]][2])
  dt$mask_num <- as.numeric(dt$mask_num)
  
  dt
  
}

#' Determine whether or not an IP address is local.
#'
#' Given a character vector of IP addresses, returns a logical vector indicating whether each entry is local (T) or not (F)
#' 
#' @usage is_local_ip(ip_vect)
#' @param ip_vect: character vector containing ip addresses
#' @keywords ip, mask
#' @examples
#' fake_ip_vect <- c('192.168.1.101', '10.10.2.201', '42.1.1.1', '125.64.3.111')
#' is_local_ip(fake_ip_vect)
is_local_ip <- function(ip_vect){
  
  log_vect <- grepl('(^127\\.0\\.0\\.1)|(^10\\.)|(^172\\.1[6-9]\\.)|(^172\\.2[0-9]\\.)|(^172\\.3[0-1]\\.)|(^192\\.168\\.)', x = ip_vect)
  
  return (log_vect)
  
}