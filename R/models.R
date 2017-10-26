

Model <- R6Class("Model",
                 inherit = Container,
                 public = list(
                   compile = function(optimizer,
                                      loss,
                                      metrics = NA,
                                      loss_weights = NA,
                                      sample_weight_mode = NA,
                                      weighted_metrics = NA,
                                      target_tensors = NA,
                                      ...){
                     # the compile function if necessary
                     
                     self$optimizer = get_optimizer(optimizer)
                     self$loss = loss
                     self$loss_weights = loss_weights
                     self$sample_weight_mode = sample_weight_mode
                     
                     # prepare loss function
                     self$loss_function = get_losses(loss)
                   },
                   fit = function(x = NA,
                                  y = NA,
                                  batch_size = NA,
                                  epochs = 1,
                                  verbose = 1,
                                  shuffle = TRUE,
                                  class_weight = NA,
                                  sample_weight = NA,
                                  initial_epoch = 0,
                                  steps_per_epoch = NA,
                                  ...){
                     # trains the model for number of epochs
                     
                     # X - training data
                     # y - dependent variables
                     # batch_size - number of samples per update
                     # epochs - max number of epochs
                     # shuffle - whether to shuffle data for each epoch
                     # class_weight - mapping of with to each class
                     # initial_epoch - epoch to start from (continuation)
                   }
                 ))


#' @title Sequential Neural Network Model
#' @export
sequential <- R6Class(
  "sequential",
  inherit = Model,
  public = list(
    
    layers = list(),
    inputs = list(),
    outputs = list(),
    
    inbound_nodes = list(),
    outbound_nodes = list(),
    
    name = NULL,
    
    initialize = function(layers, name){
      
      if(missing(name)){
        prefix = "sequential_"
        self$name = paste0(prefix, paste(sample(letters, 10), collapse = ''))
      }else{
        self$name = name
      }
      
      if(!missing(layers)){
       for(layer in layers){
         self$add(layer)
       } 
      }
    },
    
    add = function(layer){
      # add new layer
      
      if(!is(layer, "Layer")){
        stop("The added layer must be of class 'Layer'")
      }
      
      if(length(self$outputs) == 0){
        
        # this is the first layer in the model
        
        # if an input layer (i.e. no inputs of its' own)
        if(is.null(layer$inbound_nodes)){
          # create an input layer
          x = Input(batch_shape = layer$batch_input_shape,
                    name = paste0(layer$name, '_input'))
          layer$tensor(x)
        }
        
        self$outputs = list(layer$inbound_nodes[[1]]$output_tensors[[1]])
        self$inputs = get_source_inputs(self$outputs[[1]])
        
        input_shapes = as.list(sapply(self$inputs, dim))
          
        Node$new(outbound_layer = self,
                 inbound_layers = list(), node_indices = list(),
                 tensor_indices = list(), 
                 input_tensors = self$inputs,
                 output_tensors = self$outputs,
                 input_shapes = input_shapes,
                 output_shapes = list(dim(self$outputs[[1]]))
        )
        
      }else{
        print('next layer')
        print(self$outputs)
        output_tensor = layer$tensor(self$outputs[[1]])
        self$outputs = list(output_tensor)
        self$inbound_nodes[[1]]$output_tensors = self$outputs
        
        # not sure if this will work with 'dim'
        self$inbound_nodes[[1]]$output_shapes = list(dim(self$outputs[[1]]))
      }
      
      self$layers[[length(self$layers) + 1]] = layer
      self$built = FALSE
    }
  ),
  private = list(
    trainable = TRUE,
    initial_weights = NULL
  )
)


