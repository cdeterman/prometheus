

#' @docType class
#' @importFrom R6 R6Class
#' @export
activations = R6Class("activations",
                      public = list(
                        get = function(act_id){
                          switch(act_id,
                            "linear" = linear,
                            "sigmoid" = sigmoid,
                            "tanh" = tanh,
                            "relu" = relu,
                            stop("unimplemented activation function")
                          )
                        }
                      )
)


linear <- function(x){
  return(x)
}

sigmoid <- function(x){
  1/(1 + exp(-x))
}

relu <- function(x){
  pmax(0, x)
}
