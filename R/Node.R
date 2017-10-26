

Node <- R6Class(
  "Node",
  public = list(
    
    outbound_layer = NULL,
    inbound_layers = NULL,
    node_indices = NULL,
    tensor_indices = NULL,
    input_tensors = NULL,
    output_tensors = NULL,
    input_shapes = NULL,
    output_shapes = NULL,
    
    initialize = function(outbound_layer,
                          inbound_layers,
                          node_indices,
                          tensor_indices,
                          input_tensors,
                          output_tensors,
                          input_shapes,
                          output_shapes){
      
      # layer that takes input -> output
      self$outbound_layer = outbound_layer
      
      # where inputs come from
      self$inbound_layers = inbound_layers
      self$node_indices = node_indices
      self$tensor_indices = tensor_indices
      
      # outputs
      self$input_tensors = input_tensors
      self$output_tensors = output_tensors
      
      # shapes
      self$input_shapes = input_shapes
      self$output_shapes = output_shapes
      
      for(layer in inbound_layers){
        if(!is.null(layer)){
          layer$outbound_nodes[[length(layer$outbound_nodes) + 1]] = self
        }
      }
      outbound_layer$inbound_nodes[[length(outbound_layer$inbound_nodes) + 1]] = self
      
    }
  )
)
