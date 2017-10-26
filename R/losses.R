
get_losses <- function(loss){
  out <- switch(loss,
                "MSE" = mean_square_error,
                stop("loss function not implemented"))
}

mean_square_error <- function(y, y_pred){
  return(rowMeans((y_pred - y)^2))
}