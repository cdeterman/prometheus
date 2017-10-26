

get_source_inputs <- local({
  
  source_tensors = list()
  tensor_names = vector("character", length = 0)
  
  function(tensor, layer = NA, node_index = NA){
    
    if(!".has_history" %in% names(attributes(tensor))){
      return(tensor)
    }
    
    if(is(layer, 'Layer') || is.na(node_index)){
      history <- attr(tensor, ".has_history")
      layer <- history[[1]]
      node_index <- history[[2]]
    }
    
    if(length(layer$inbound_nodes) == 0){
      return(list(tensor))
    }else{
      node <- layer$inbound_nodes[[node_index]]
      if(length(node$inbound_layers) == 0){
        # reach input layer, stop
        return(node$input_tensors)
      }else{
        for(i in seq_along(node$inbound_layers)){
          x <- node$input_tensors[[i]]
          if(is.list(x)){
            for(j in seq_along(x)){
              tensor_names <- c(tensor_names, x[[j]]$name)
            }  
          }else{
            tensor_names <- c(tensor_names, x$name)
          }
          
          layer <- node$inbound_layers[[i]]
          node_index <- node$node_indices[[i]]
          previous_sources <- get_source_inputs(x, layer, node_index)
          
          # avoid input redundancy
          for(x in previous_sources){
            if(!x$name %in% tensor_names){
              source_tensors[[length(source_tensors) + 1]] <- x
            }
          }
        }
        
        return(source_tensors)
      }
    }
  }
})
