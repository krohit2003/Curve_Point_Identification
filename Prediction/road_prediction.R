# Required libraries
required_libraries <- c("dplyr", "ggplot2", "caret", "randomForest", "purrr")

for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE)
}

predict_curve <- function(dat) {
  n <- nrow(dat)
  
  # Function to calculate the radius of curvature using 4 points before and after
  calc_radius <- function(X, Y) {
    pred_radius <- rep(1e16, n)  # Default large value for edge cases
    
    for (i in 5:(n - 4)) {  # Ensure at least 4 points before and after
      x1 <- X[i - 4]
      y1 <- Y[i - 4]
      x2 <- X[i]
      y2 <- Y[i]
      x3 <- X[i + 4]
      y3 <- Y[i + 4]
      
      A <- abs((x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2))
      d12 <- (x2 - x1)^2 + (y2 - y1)^2
      d23 <- (x3 - x2)^2 + (y3 - y2)^2
      d31 <- (x3 - x1)^2 + (y3 - y1)^2
      
      if (A != 0) {
        pred_radius[i] <- (d12 * d23 + d23 * d31 + d31 * d12) / (4 * A)
      }
    }
    if(n>=5){
    for(i in 1:4){
      pred_radius[i] <- pred_radius[5]
    }
    }
    return(log(pred_radius ))  # Log transform radius before returning
  }
  
  # Compute angles and radius
  dat <- dat %>% mutate(
    pred_radius = calc_radius(X, Y)
  )

  # Convert predicted probabilities to classes
  sorted_radius <- sort(dat$pred_radius, decreasing = FALSE, na.last = NA)
 
  threshold <- sorted_radius[1] + sd(sorted_radius)

  dat$Curve <- ifelse(dat$pred_radius < threshold, 1, 0)

  predict_output <- dat %>% select(X, Y, Curve)
  
  return(list(predict = predict_output, misc = list(log_radius = dat$radius)))
}

