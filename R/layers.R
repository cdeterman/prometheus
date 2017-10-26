

Layer <- R6Class(
  "Layer",
  active = list(
    
    built = function(value){
      if(missing(value)){
        return(private$.built)
      }else{
        if(!is(value, 'logical')) stop("must provide boolean argument")
        private$.built = value
      }
    },
    
    trainable_weights = function(weights){
      if(self$trainable){
        if(missing(weights)) return(private$.trainable_weights)
        private$.trainable_weights = weights
      }
    },
    
    non_trainable_weights = function(weights){
      if(!self$trainable){
        if(missing(weights)) return(c(private$.trainable_weights, private$.non_trainable_weights))
        private$.non_trainable_weights = weights
      }
    }
    
  ),
  public = list(
    
    trainable = TRUE,
    name = NULL,
    batch_size = NULL,
    batch_input_shape = NULL,
    
    inbound_nodes = NULL,
    outbound_nodes = NULL,
    
    initialize = function(input_shape,
                          batch_input_shape = NA,
                          batch_size = NA,
                          name = NA,
                          trainable = TRUE,
                          weights = NA){
      
      self$trainable = trainable
      
      if(is.na(name)){
        self$name = paste0(sample(letters, 10), collapse = '')
      }else{
        self$name = name
      }
      
      if(!is.na(batch_input_shape) || !is.na(input_shape)){
        if(!is.na(batch_input_shape)){
          batch_input_shape = batch_input_shape
        }else{
          if(!is.na(input_shape)){
            if(!is.na(batch_size)){
              self$batch_size = batch_size
            }else{
              self$batch_size = NULL
            }
            # self$batch_input_shape = batch_size + input_shape
            batch_input_shape = input_shape
          }
        }
        self$batch_input_shape = batch_input_shape
      }
      
      if(!is.na(weights)){
        private$.initial_weights = weights
      }
      
    },
    
    add_weight = function(name,
                          shape,
                          initializer,
                          regularizer = NA,
                          trainable = TRUE){
      
      # Adds a weight variable to the layer
      
      initializer = Initializers$new()$get(initializer)
      weight = initializer(shape)
      if(trainable){
        # currently appends to list
        self$trainable_weights = weight
      }else{
        self$non_trainable_weights = weight
      }
      
      return(weight)
      
    },
    
    weights = function(){
      return(c(private$.trainable_weights, private$.non_trainable_weights))
    },
    
    set_weights = function(weights){
      internal_len = length(self$weights())
      if(length(weights) != internal_len){
        stop(paste0("provided weights length does not equal expected length: ", internal_len))
      }
      
      private$.trainable_weights = weights[[1]]
      
      if(internal_len > 1){
        private$.non_trainable_weights = weights[[2]]  
      }
      
      return(self)
    },
    
    build = function(input_shape){
      # create the layers weights
      # should be implemented for layers that have weights
      
      print('called parent method')
      self$built = TRUE
    },
    
    compute = function(inputs, ...){
      # layer logic
      # typically implemented in child layer
      
      return(inputs)
    },
    
    # ideally this would be an overloaded initialize method
    # but alas, R doesn't do overloading like that
    tensor = function(inputs, ...){
      print('calling tensor method')
      
      # parent functions
      if(!self$built){
        print('building')
        input_shapes = dim(inputs)
        
        print('got shape')
        # should be defined in child layer
        self$build(input_shapes)
      }
      self$built = TRUE
      
      print('built')
      
      if(!is.null(private$.initial_weights)){
        self$set_weights(private$.initial_weights)
      }
      
      input_shape = private$.get_shape(inputs)
      
      print('got shapes')
      
      # call compute function
      # delayedAssign("output", self$child_tensor(inputs, ...),
      #               assign.env = self)
      output = self$compute(inputs, ...)
      output_shape = private$.get_shape(output)
      
      print('add inbound node')
      # add the inbound node
      private$.add_inbound_node(input_tensors = list(inputs),
                                output_tensors = list(output),
                                input_shapes = input_shape,
                                output_shapes = output_shape,
                                ...)
    }
    
  ),
  private = list(
    .initial_weights = NULL,
    .trainable_weights = list(),
    .non_trainable_weights = list(),
    losses = list(),
    updates = list(),
    .built = FALSE,
    
    .get_shape = function(value){
      
      if(inherits(value, "Tensor")){
        return(dim(value))
      }
      
      out = switch(class(value)[1],
                   "integer" = length(value),
                   "numeric" = length(value),
                   "matrix" = dim(value),
                   stop("unrecognized class"))
      return(out)
    },
    
    .add_inbound_node = function(input_tensors, output_tensors,
                                 input_shapes, output_shapes,
                                 ...){
      
      inbound_layers = list()
      node_indices = list()
      tensor_indices = list()
      
      for(x in input_tensors){
        if(".has_history" %in% names(attributes(x))){
          history = attr(x, ".has_history")
          inbound_layers[[length(inbound_layers) + 1]] = history[[1]]
          node_indices[[length(node_indices) + 1]] = history[[2]]
          tensor_indices[[length(tensor_indices) + 1]] = history[[3]]
        }else{
          inbound_layers[[length(inbound_layers) + 1]] = NA
          node_indices[[length(node_indices) + 1]] = NA
          tensor_indices[[length(tensor_indices) + 1]] = NA
        }
      }
      
      # create node, add to inbound nodes
      Node$new(self, inbound_layers = inbound_layers,
               node_indices = node_indices, tensor_indices = tensor_indices,
               input_tensors = input_tensors, 
               output_tensors = output_tensors,
               input_shapes = input_shapes,
               output_shapes = output_shapes)
      
      # update history
      for(i in seq_along(output_tensors)){
        # output_tensors[i]._keras_shape = output_shapes[i]
        # uses_lp = any([getattr(x, '_uses_learning_phase', False) for x in input_tensors])
        # uses_lp = getattr(self, 'uses_learning_phase', False) or uses_lp
        # output_tensors[i]._uses_learning_phase = getattr(output_tensors[i], '_uses_learning_phase', False) or uses_lp
        attr(output_tensors[[i]], ".has_history") = list(self, length(self$inbound_nodes), i)
      }
    }
  )
)

Container <- R6Class(
  "Container",
  inherit = Layer,
  public = list(
    
    initialize = function(inputs, outputs, name = NA){
      if(is.na(name)){
        prefix = class_name
        name = paste0(prefix, paste(sample(letters, 10), collapse = ''), sep = "_")
      }
      self$name = name
      
      self$trainable = TRUE
      
    },
    
    # methods
    summary = function(){
      # to be filled
    },
    
    get_layer = function(){
      # to be filled
    },
    
    get_weights = function(){
      # pull weights from each layer
      weights = list()
      for(l in seq_along(self$layers)){
        weights[[l]] = self$layers[[l]]$weights()
      }
      return(weights)
    },
    
    set_weights = function(weights){
      # set weights for each layer
      
      if(length(weights) != length(self$layers)){
        stop(paste0("length of weights not equal to expected length: ", length(self$layers)))
      }
      
      for(l in seq_along(self$layers)){
        
        internal_len = length(self$layers[[1]]$weights())
        if(length(weights[[l]]) != internal_len){
          stop(paste0("provided weights length does not equal expected length: ", internal_len))
        }
        
        self$layers[[1]]$set_weights(weights)
        
      }
      
      invisible(self)
    },
    
    get_config = function(){
      # to be filled
    },
    
    compute_output_shape = function(){
      # to be filled
    }
  ),
  
  private = list(
    .per_input_losses = list(),
    .per_input_updates = list()
  )
)


#' @export
InputLayer <- R6Class(
  "InputLayer",
  inherit = Layer,
  public = list(
    
    is_placeholder = FALSE,
    
    initialize = function(input_shape = NA, batch_size = NA,
                          batch_input_shape = NA, 
                          input_tensor = NA, name){
      if(missing(name)){
        prefix = 'input'
        self$name = paste0(prefix, '_', paste(sample(letters, 10), collapse = ''))
      }else{
        self$name = name
      }
      
      if(!is.na(input_shape) & !is.na(batch_input_shape)){
        stop("Only provide input_shape OR batch_input_shape")
      }
      
      if(all(is.na(batch_input_shape))){
        if(is.na(input_shape)){
          stop("Input layer must either be passed 'batch_input_shape' or 'input_shape'")  
        }else{
          # batch_input_shape = (batch_size,) + tuple(input_shape) 
          batch_input_shape = input_shape 
        }
      }else{
        batch_input_shape = batch_input_shape
      }
      
      self$batch_input_shape = batch_input_shape
      
      if(is.na(input_tensor)){
        self$is_placeholder = TRUE
        input_tensor = Placeholder$new(batch_input_shape, self$name)
      }else{
        self$is_placeholder = FALSE
      }
      
      attr(input_tensor, ".has_history") = list(self, 1, 1)
      
      Node$new(self, inbound_layers = list(),
               node_indices = list(), tensor_indices = list(),
               input_tensors = list(input_tensor), 
               output_tensors = list(input_tensor),
               input_shapes = list(batch_input_shape),
               output_shapes = list(batch_input_shape))
    }
  )
)


#' @export
Input <- function(shape = NA, batch_shape = NA, name = NA){
  
  if(!is.na(shape) & is.na(batch_shape)){
    batch_shape = c(NA, shape)
  }
  
  input_layer = InputLayer$new(batch_input_shape = batch_shape,
                               name = name)
  
  outputs = input_layer$inbound_nodes[[1]]$output_tensors
  if(length(outputs) == 1){
    return(outputs[[1]])
  }else{
    return(outputs)
  }
}


#' @export
Dense <- R6Class(
  "Dense",
  inherit = Layer,
  public = list(
    units = NULL,
    activation = NULL,
    use_bias = NULL,
    bias = NULL,
    bias_initializer = NULL,
    kernel = NULL,
    kernel_initializer = NULL,
    
    initialize = function(units,
                          activation = "linear",
                          use_bias = TRUE,
                          kernel_initializer = 'RandomNormal',
                          bias_initializer = 'zeros',
                          ...){
      self$units = units
      self$activation = activations$new()$get(activation)
      self$use_bias = use_bias
      self$kernel_initializer = kernel_initializer
      self$bias_initializer = bias_initializer
      
      # extra arguments
      dots = list(...)
      do.call(super$initialize, args = dots)
    },
    
    build = function(input_shape){
      self$kernel = self$add_weight(name = 'kernel',
                                    shape = c(input_shape[1], self$units),
                                    initializer = self$kernel_initializer)
      if(self$use_bias){
        self$bias = self$add_weight(name = 'bias',
                                    shape = self$units,
                                    initializer = self$bias_initializer)
      }else{
        self$bias = NULL
      }
      
      self$built = TRUE
    },
    
    compute = function(inputs){
      output = dot$new(inputs, self$kernel)
      
      if(self$use_bias){
        output = add$new(output, self$bias)
      }
      
      if(!is.null(self$activation)){
        output = self$activation(output)
      }
      
      return(output)
    },
    
    get_config = function(){
      config = list(
        units = self$units,
        activation = self$activation,
        use_bias = self$use_bias,
        kernel_initializer = self$kernel_initializer,
        bias_initializer = self$kernel_initializer
      )
      
      return(config)
    }
    
  )
)


#' @export
Activation <- R6Class(
  "Activation",
  inherit = Layer,
  public = list(
    activation = NULL,
    
    initialize = function(activation){
      self$activation = activations$new()$get(activation)
    },
    
    call = function(inputs){
      return(self$activation(inputs))
    },
    
    get_config = function(){
      config = list(activation = self$activation)
      return(config)
    }
  )
)


