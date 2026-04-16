# Symmetric log transformation
symlog <- function(x, threshold = 1, inverse = FALSE) {
  if (inverse) {
    result <- sign(x) * ifelse(abs(x) < threshold, 
                               abs(x),  # Linear region
                               threshold * exp(abs(x) - threshold))  # Inverse logarithmic region
  } else {
    result <- sign(x) * ifelse(abs(x) < threshold, 
                               abs(x),  # Linear region
                               threshold + log(abs(x) / threshold))  # Logarithmic region
  }
  
  result
}