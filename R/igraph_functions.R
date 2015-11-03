#' Creates a subgraph based on a vertex attribute
#'
#' Given a graph, desired attribute values, and the title of the attribute, this function creates a subgraph based on those criteria
#' 
#' @usage subgraph_from_v_attr(g, attr_values, attribute)
#' @param g: graph to be subset
#' @param attr_values: Character vector of values of the vertex attribute that are allowed
#' @param attribute: Character vector of the vertex attribute upon which to subset
#' @keywords graph, igraph
#' @examples 
#' subgraph_from_v_attr(my_graph, c('yes', 'no'), 'is_it_good')
subgraph_from_v_attr <- function(g, attr_values, attribute){
  
  #Get vertices with matching attributes
  vert_names <- eval(parse(text = paste('V(g)$name[V(g)$', 
                                        attribute,
                                        ' %in% c("',
                                        paste(attr_values, collapse = '", "'),
                                        '")]', sep = '')))
  
  #Use edge list to get subgraph
  temp_edge_list <- get.edgelist(graph = g)
  
  return (subgraph.edges(graph = g, E(g)[temp_edge_list[, 1] %in% vert_names]))
  
}

#' subset the graph based on date ranges
subset_graph_on_dates <- function (g, start_date, end_date, edge_date_var = 'rel_date'){
  
  #Subset down to a graph whose relationsips all contain the edge_date_var
  edges <- E(g)
  edge_ids <- eval(parse(text = paste('edges[!is.na(edges$',
                                      edge_date_var,
                                      ')]', sep = '')))
  
  g <- subgraph.edges(g, eids = edge_ids)
  
  
  #subset the graph based on the dates
  new_edges <- E(g)
  edge_ids <- eval(parse(text = paste('new_edges[new_edges$',
                                      edge_date_var,
                                      ' >= "',
                                      start_date,
                                      '" & new_edges$',
                                      edge_date_var,
                                      ' < "',
                                      end_date,
                                      '"]', sep = '')))
  
  return(subgraph.edges(graph = g, eids = edge_ids))
}


#' Retrieve all vertex attributes
#'
#' Given vertex indices, retrieve all vertex attributes for those nodes
#' 
#' @usage retrieve_vertex_attributes(g, index = NULL)
#' @param g: graph to use
#' @param index: numeric or named vector indicating the node(s) of interest
#' @keywords graph, igraph
#' @examples 
#' retrieve_vertex_attributes(my_graph, 1:10)
retrieve_vertex_attributes <- function(g, index = NULL) {
  
  if (is.null(index)) index <- 1:length(V(g))
  sapply(list.vertex.attributes(g), function(x) get.vertex.attribute(g, x, index))
}

#' Retrieve all edge attributes
#'
#' Given edge indices, retrieve all edge attributes for those nodes
#' 
#' @usage retrieve_edge_attributes(g, index = NULL)
#' @param g: graph to use
#' @param index: numeric or named edge indicating the relationship(s) of interest
#' @keywords graph, igraph
#' @examples 
#' retrieve_edge_attributes(my_graph, 1:10)
retrieve_edge_attributes <- function(g, index = NULL) {
  
  if (is.null(index)) index <- 1:length(E(g))
  sapply(list.edge.attributes(g), function(x) get.edge.attribute(g, x, index))
}

#' Create a data.table that can be used to load all vertices into the graph
#'
#' Given a data.table and a list containing vertices and their attributes, creates a data.table that can be used to create the vertices in an igraph graph
#' 
#' @usage make_vertices_dt(dt, vertex_info, same_v_attr_list = NULL, parse_name = F)
#' @param dt: data.table containing columns for all the vertexes and vertex attributes
#' @param vertex_info: named list, where the name is the vertex column name and the contents of each named entry is a character vector containing the names of the attribute columns to be associated with the vertex of interest
#' @param same_v_attr_list: named list where the name is the title to resolve the associated character vector names to
#' @param parse_name: whether or not to parse the entity_type and add it to the node name
#' @keywords graph, igraph
#' @examples 
#' Set information pertaining to which vertices and attributes will be added
#' vertex_info <- list(prchsr_unique = c('prchsr', 'purchase_pieces'),
#'                     candy = c('candy_typ', 'pieces'),
#'                     house = c('the_name')
#' same_v_attr_list <- list(all_pieces = c('purchase_pieces', 'pieces'))
make_vertices_dt <- function (dt, vertex_info, same_v_attr_list = NULL, parse_name = F){
  #vertex_info is a named list. The name of each element is the name of the column to be used as the vertex; a character vector containing column names to be used as attributes is associated with each list element. e.g. vertex_info = list(prchsr_unique = c('prchsr_type', 'prchsr_nm'), case_nbr = c('prop_addr_zip_5', 'sales_typ'))
  
  require(data.table, quietly = T)
  
  #initialize a list to hold all the data.tables for each vertex
  dt_list <- list()
  vertices_to_add <- names(vertex_info)
  new_col_names <- names(same_v_attr_list)
  
  for (i in 1:length(vertices_to_add)){
    
    #Get the vertex of interest
    vertex <- vertices_to_add[i]
    
    #Get the columns and attributes of interest in a data.table, skipping NAs
    new_dt <- dt[, c(vertex, vertex_info[[vertex]]), with = F][!is.na(get(vertex))]
    
    #rename the vertex column to 'name' so they can be bound properly
    setnames(new_dt, vertex, 'name')
    
    #Check to see if any of the columns in the data.table should be renamed
    if (!is.null(new_col_names)){
      
      to_change <- c()
      change_to <- c()
      
      #Loop through and find all matches
      for (j in 1:length(new_col_names)){
        
        if (any(same_v_attr_list[[j]] %in% names(new_dt))){
          
          to_change <- c(to_change, intersect(same_v_attr_list[[j]], names(new_dt)))
          change_to <- c(change_to, new_col_names[j])
          
        }
      }
      
      #Change the column names
      if (!is.null(to_change) & !is.null(change_to)) setnames(new_dt, to_change, change_to)
      
    }
    
    #Add it to the dt_list
    dt_list[[i]] <- new_dt
    
  }
  
  #bind all the data.tables together
  all_vertices <- rbindlist(dt_list, fill = T)
  
  #Get rid of duplicate names. NOTE: This only keeps the first occurrence (along with its attributes) and deletes the others. If there are attributes that need to be retained for duplicates, some merged or other data manipulation needs to be done beforehand.
  all_vertices <- all_vertices[!duplicated(name)]
  
  #If parse_name is True, add an attribute corresponding to the entity type (requires "_" separator in name attribute)
  if (parse_name) all_vertices[, entity_type := sapply(name, function(x) strsplit(x, '_')[[1]][[1]])]
  
  return (all_vertices)
}

#' Create a data.table that can be used to load all edges into the graph
#'
#' Given a data.table and a list containing edges and their attributes, creates a data.table that can be used to create the edges in an igraph graph
#' 
#' @usage make_edges_dt(dt, edge_info, same_e_attr_list = NULL)
#' @param dt: data.table containing columns for all the edges and edge attributes
#' @param edge_info: named list, where the name is the edge type and each "to" and "from" refers to the columns in the dt to go from and to
#' @param same_e_attr_list: named list where the name is the edge attributes to resolve and the associated character vector are the column names to be resolved from dt
#' @keywords graph, igraph
make_edges_dt <- function(dt, edge_info, same_e_attr_list = NULL){
  
  require(data.table, quietly = T)
  
  #Initialize variables
  dt_list <- list()
  rels_to_add <- names(edge_info)
  new_col_names <- names(same_e_attr_list)
  
  for (i in 1:length(rels_to_add)){
    
    #get the relationship of interest
    rel <- rels_to_add[i]
    
    #get the data.table info for this relationship
    new_dt <- dt[, c(edge_info[[i]][['from']], 
                     edge_info[[i]][['to']],
                     edge_info[[i]][['attributes']]), with = F]
    
    #Add a rel_type column to the data.table that indicates the name of the relationship
    new_dt[, type := rel]
    
    #Change the names of the relationship columns to 'from' and 'to'
    setnames(new_dt, 
             c(edge_info[[i]][['from']], edge_info[[i]][['to']]), 
             c('from', 'to'))
    
    #Consolidate any attributes that require being named the same
    #Check to see if any of the columns in the data.table should be renamed
    if (!is.null(new_col_names)){
      
      to_change <- c()
      change_to <- c()
      
      #Loop through and find all matches
      for (j in 1:length(new_col_names)){
        
        if (any(same_e_attr_list[[j]] %in% names(new_dt))){
          
          to_change <- c(to_change, intersect(same_e_attr_list[[j]], names(new_dt)))
          change_to <- c(change_to, new_col_names[j])
          
        }
      }
      
      #Change the column names
      if (!is.null(to_change) & !is.null(change_to)) setnames(new_dt, to_change, change_to)
      
    }
    
    #Get rid of rows with NAs in the 'from' or 'to' columns
    new_dt <- new_dt[!is.na(from) & !is.na(to)]
    
    #Add it to the list
    dt_list[[i]] <- new_dt
    
  }
  
  #Concatenate all the data.tables
  all_edges <- rbindlist(dt_list, fill = T)
  
  #Get rid of edges that are exactly identical
  all_edges <- unique(all_edges)
  
  return(all_edges)
}