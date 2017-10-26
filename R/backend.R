

Tensor <- R6Class("Tensor",
                  active = list(
                    shape = function(value){
                      if(missing(value)) return(private$.shape)
                      private$.shape = value
                    },
                    
                    nrow = function(value){
                      if(missing(value)) return(private$.shape[1])
                      private$.shape[1] = value
                    },
                    
                    ncol = function(value){
                      if(missing(value)) return(private$.shape[2])
                      private$.shape[2] = value
                    }
                    
                  ),
                  public = list(
                    tensor = NULL,
                    name = NULL,
                    ops = list(),
                    
                    initialize = function(initializer, shape){
                      if(is.character(initializer)){
                        self$tensor = Initializers$new()$get(initializer)
                        self$shape = shape
                        private$.initializer = TRUE
                      }else{
                        self$tensor = initializer
                        self$shape = private$.get_shape(initializer)
                        private$.initializer = FALSE
                      }
                    },
                    
                    sin = function(){
                      self$ops[[length(self$ops) + 1]] = "sin"
                      invisible(self)
                    },
                    
                    asin = function(){
                      self$ops[[length(self$ops) + 1]] = "asin"
                      invisible(self)
                    },
                    
                    sinh = function(){
                      self$ops[[length(self$ops) + 1]] = "sinh"
                      invisible(self)
                    },
                    
                    cos = function(){
                      self$ops[[length(self$ops) + 1]] = "cos"
                      invisible(self)
                    },
                    
                    acos = function(){
                      self$ops[[length(self$ops) + 1]] = "acos"
                      invisible(self)
                    },
                    
                    cosh = function(){
                      self$ops[[length(self$ops) + 1]] = "cosh"
                      invisible(self)
                    },
                    
                    tan = function(){
                      self$ops[[length(self$ops) + 1]] = "tan"
                      invisible(self)
                    },
                    
                    atan = function(){
                      self$ops[[length(self$ops) + 1]] = "atan"
                      invisible(self)
                    },
                    
                    tanh = function(){
                      self$ops[[length(self$ops) + 1]] = "tanh"
                      invisible(self)
                    },
                    
                    max = function(){
                      self$ops[[length(self$ops) + 1]] = "max"
                      invisible(self)
                    },

                    min = function(){
                      self$ops[[length(self$ops) + 1]] = "min"
                      invisible(self)
                    },
                    
                    compute = function(){
                      if(private$.initializer){
                        
                        if(length(self$ops) == 0){
                          print("returning initializer")
                          return(self$tensor(self$shape))
                        }else{
                          output = self$tensor(self$shape)
                          for(f_str in self$ops){
                            f = eval(parse(text = f_str))
                            output = f(output)
                          }
                          return(output)  
                        }
                        
                      }else{
                        if(length(self$ops) == 0){
                          return(self$tensor)  
                        }else{
                          output = self$tensor
                          for(f_str in self$ops){
                            # print(paste0('evaluating: ', f_str))
                            f = eval(parse(text = f_str))
                            output = f(output)
                          }
                          return(output) 
                        }
                      }
                    }
                  ),
                  private = list(
                    .shape = NULL,
                    .initializer = FALSE,
                    .has_history = NULL,
                    
                    .get_shape = function(value){
                      out = switch(class(value),
                                   "integer" = length(value),
                                   "numeric" = length(value),
                                   "matrix" = dim(value),
                                   stop("unrecognized class"))
                      return(out)
                    }
                  )
)

nrow.Tensor <- function(tensor){
  return(tensor$nrow)
}

ncol.Tensor <- function(tensor){
  return(tensor$ncol)
}

dim.Tensor <- function(tensor){
  return(tensor$shape)
}

Placeholder <- R6Class("Placeholder",
                       inherit = Tensor,
                       public = list(
                         initialize = function(shape, name){
                           self$shape = shape
                           self$name = name
                         }
                       )
)


dot <- R6Class("dot",
               inherit = Tensor,
               public = list(
                 
                 x = NULL,
                 y = NULL,
                 
                 initialize = function(x, y){
                   self$x = x
                   self$y = y
                   self$shape = c(nrow(x), ncol(y))
                 },
                 
                 compute = function(){
                   self$tensor = x %*% y
                 }
               )
)

add <- R6Class('add',
               inherit = Tensor,
               public = list(
                 
                 x = NULL,
                 y = NULL,
                 
                 initialize = function(x,y){
                   self$x = x
                   self$y = y
                   self$shape = dim(x)
                 },
                 
                 compute = function(){
                   self$tensor = x + y
                 }
               )
)


update_add <- function(x, increment){
  return(c(x, x+increment))
}


