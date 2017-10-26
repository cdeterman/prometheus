
get_optimizer <- function(optimizer){
  out <- switch(optimizer,
                "BGD" = BGD,
                "SGD"= SGD,
                "MSGD" = MSGD,
                "RMSprop" = RMSprop,
                stop("unimplemented optimizer")
  )
  
  return(out)
}

Optimizer <- R6Class("Optimizer",
                     public = list(
                       
                       updates = NULL,
                       weights = NULL,
                       
                       initialize = function(...){
                         
                         self.updates = list()
                         self.weights = list()
                       },
                       
                       get_gradients = function(loss, params){
                         # grads = backend.gradients(loss, params)
                         
                         return(grads)
                       },
                       
                       set_weights = function(weights){
                         # set weights of optimizer
                         # only called after calculating gradients
                         
                         params = self$weights
                         
                         if(length(weights) != length(params)){
                           stop(paste0("provided weights length does not equal expected length: ", length(params)))
                         }
                         
                         for(p in seq_along(params)){
                           self$weights[[p]] = weights[[p]]
                         }
                         return(invisible(self))
                       },
                       
                       get_weights = function(){
                         return(self$weights)
                       }
                     )
)

BGD <- R6Class("BGD",
               inherit = Optimizer,
               public = list(
                 
                 iterations = NULL,
                 lr = NULL,
                 momentum = NULL,
                 nesterov = NULL,
                 initial_decay = NULL,
                 
                 
                 initialize = function(lr = 0.01, momentum = 0, 
                                       decay = 0, nesterov=FALSE,
                                       ...){
                   super$initialize(...)
                   
                   # these are 'variable' objects
                   self$iterations = 0
                   self$lr = lr
                   self$momentum = momentum
                   self$decay = decay
                   
                   self$initial_decay = decay
                   self$nesterov = nesterov
                 },
                 
                 get_updates = function(loss, params){
                   grads = self$get_gradients(loss, params)
                   self$updates = list(update_add(self$interations, 1))
                   
                   lr = self$lr
                   
                   if(self$initial_decay > 0){
                     lr = lr * (1 / (1 + self$decay * self$iterations))
                   }
                   
                   # momentum
                   # shapes = 
                 },
                 
                 get_config = function(){
                   config = list(
                     lr = self$lr,
                     momentum = self$momentum,
                     decay = self$decay,
                     nesterov = self$nesterov)
                   
                   return(config)
                   
                 }
               )
)

SGD <- R6Class("SGD",
               inherit = Optimizer,
               public = list()
)

