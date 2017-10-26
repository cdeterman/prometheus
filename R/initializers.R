# Initializers

Initializers = R6Class("Initializers",
                      public = list(
                        get = function(initializer){
                          switch(initializer,
                                 "zeros" = Zeros,
                                 "ones" = Ones,
                                 "Constant" = Constant,
                                 "RandomNormal" = RandomNormal,
                                 stop("unimplemented initializer function")
                          )
                        }
                      )
)

Zeros <- function(shape){
  if(length(shape) == 1){
    rep(0, shape)
  }else{
    matrix(0, nrow = shape[1], ncol = shape[2])  
  }
}

Ones <- function(shape){
  if(length(shape) == 1){
    rep(1, shape)
  }else{
    matrix(1, nrow = shape[1], ncol = shape[2])
  }
}

Constant <- function(shape, constant){
  if(length(shape) == 1){
    rep(constant, shape)
  }else{
    matrix(constant, nrow = shape[1], ncol = shape[2])
  }
}

RandomNormal <- function(shape){
  if(length(shape) == 1){
    rnorm(shape)
  }else{
    matrix(rnorm(prod(shape)), nrow = shape[1], ncol = shape[2])
  }
}
