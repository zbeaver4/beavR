#' Add nodes to neo4j from a dataframe
#'
#' This function adds nodes row-wise to a neo4j graph (called "graph")
#' 
#' @usage add_nodes(df, label, id, props = NULL, verbose = FALSE, na.act = F)
#' @param
#' df: dataframe which will be traversed to row-wise to add nodes.
#' @param label: The label to assign to each node (takes a character value)
#' @param id: column name in dataframe required for a node to be created (takes a character value)
#' @param props: column names(s) in dataframe of node properties (takes a character vector); defaults to NULL
#' @param verbose: if TRUE, returns information about each node being added to the graph DB; defaults to False
#' @param na.act: not currently used...maybe in the future; defaults to False
#' @keywords nodes
#' @examples
#' add_nodes(awarded_contracts, "contract", "DUNSNumber", props = names(awarded_contracts), verbose = T)

add_nodes <- function(df, label, id, props = NULL, verbose = FALSE, na.act = F) {
  nodes <- list()
  # cannot have name of NA value
  miss <- !is.na(df[, id])
  uniq <- unique(df[miss, c(id, props), drop = FALSE])
  
  for (i in 1:nrow(uniq)) {
    arg <- as.list(uniq[i, c(id, props), drop = FALSE])
    arg <- arg[!is.na(arg)]                   
    n <- do_call(createNode, quote(graph), label, arg)
    if (verbose) cat(i, 'Created ', label, ': ', n[[1]], '\n')
    tryCatch(nodes[[uniq[i, id]]] <- n, error = function(e){
      cat("We got an error", conditionMessage(e), i, paste(uniq[i, id]), "\n")
      readline("Press <return to continue")
    }
    )
  }
  nodes
}

#' Add or Update nodes in neo4j from a dataframe
#'
#' This function Checks to see if a node already exists with the label/id combination; if not, it adds the node to Neo4j; if so, it attempts update the node based on the properties in the row. The neo4j graph must be called "graph".  In order for the update portion to work, a constraint must be set on the label/id combination. Example: addConstraint(graph, "vendor", "DUNSNumber")
#' 
#' @usage add_or_update_nodes(df, label, id, props = NULL, verbose = FALSE, na.act = F)
#' @param label: The label to assign to each node (takes a character value)
#' @param id: column name in dataframe required for a node to be created (takes a character value)
#' @param props: column names(s) in dataframe of node properties (takes a character vector); defaults to NULL
#' @param verbose: if TRUE, returns information about each node being added to the graph DB; defaults to False
#' @param na.act: not currently used...maybe in the future; defaults to False
#' @keywords nodes
#' @examples
#' add_or_update_nodes(awarded_contracts, "contract", "DUNSNumber", props = names(awarded_contracts), verbose = T)

add_or_update_nodes <- function(df, label, id, props = NULL, verbose = FALSE, na.act = F) {
  nodes <- list()
  # cannot have name of NA value
  miss <- !is.na(df[, id])
  uniq <- unique(df[miss, c(id, props), drop = FALSE])
  
  #initiate a counter for the true number of nodes added to the database
  j <- 1
  for (i in 1:nrow(uniq)) {
    arg <- as.list(uniq[i, c(id, props), drop = FALSE])
    arg <- arg[!is.na(arg)]
    
    #Try to create a node. If it already exists, try to upate its properties...probably need to rewrite this to only raeact to a specific error
    tryCatch({n <- do_call(createNode, quote(graph), label, arg); if (verbose) cat(j, 'Created ', label, ': ', n[[1]], '\n'); j <- j+ 1},
             
             #Properties are updated when an error arises because of a pre-existing node
             error = function(e){
               if(verbose) cat("Node ", paste(uniq[i, id]), "already exists and is uniquely constrained; attempting to update its properties \n")
               
               #Query the database to obtain the object we want to update
               query <- paste("MATCH (a:", label, "{", id, ": '", uniq[i, id], "'}) RETURN a", sep = "")
               object <- getSingleNode(graph, query)
               
               #Check to see if there are any properties; if so, update the existing node
               if(length(arg > 1)){                              
                 do_call(updateProp, quote(object), arg[2:length(arg)])
                 if(verbose) cat("updated properties \n")
               }
               else{
                 if(verbose) cat("No properties to update \n")
               }
               
             }
    )
    
    #Update the list of nodes to be returned by querying the database (may be a more efficient way to do this)
    query <- paste("MATCH (a:", label, "{", id, ": '", uniq[i, id], "'}) RETURN a", sep = "")
    object <- getSingleNode(graph, query)
    
    tryCatch(nodes[[uniq[i, id]]] <- object, error = function(e){
      cat("We got an error returning nodes: ", conditionMessage(e), i, paste(uniq[i, id]), "\n")
      readline("Press <return to continue")
    }
    )
    
  }
  nodes
}