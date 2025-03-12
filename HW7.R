# Henry Sun 
# HW 7 
# 3/12/25

# want to compute P(X = x), P(X != x), P(X < x), P(X <= x), 
# P(X > x), P(X >= x) 

# prob was removed as parameter of the function, as prob would only be used in 
# with qpois (inverse CDF)
pois.prob <- function(x, lambda, type="<="){
  # 
  if (lambda < 0){
    # lambda must be <= 0 
    lambda = 0
  }
  if (x < 0){
    # x must be <= 0
    x = 0
  }
  if (type == "="){
    # definition of PMF
    dpois(x, lambda)
  }
  if (type == "!="){
    # complement rule
    1-dpois(x, lamda)
  }
  if(type == "<="){
    # definition of CDF
    ppois(x, lambda)
  }
  if (type == "<"){
    # P(X < x) = P(X <= x-1)
    ppois(x-1, lambda)
  }
  if (type == ">="){
    # P(X >= x) = 1 - P(X <= x-1)
    1-ppois(x-1, lambda)
  }
  if (type == ">"){
    # P(X > x) = 1 - P(X <= x)
    1-ppois(x, lambda)
  }
}

# prob removed as a parameter
# according to pbeta, alpha, beta must be nonnegative
beta.prob <- function(x, alpha, beta, type="<="){
  if (alpha < 0){
    # alpha must be <= 0 
     alpha = 0
  }
  if (beta < 0){
    # beta must be <= 0 
    beta = 0
  }
  if (x < 0){
    # x must be <= 0
    x = 0
  }
  if (x < 0){
    # x must be <= 0
    x = 0
  }
  if (type == "="){
    # P(X = x) = 0 for all x
    return(0) 
  }
  if (type == "!="){
    # P(X != x) = 1 for all x
    return(1)
  }
  if(type == "<" | type == "<="){
    # P(X < x) = P(X <= x) for continuous distributions
    # definition of pbeta
    pbeta(x, alpha, beta)
  }
  if (type == ">" | type == ">="){
    # P(X > x) = P(X >= x) = 1 - P(X < x)for continouous distributions 
    1-pbeta(x, alpha, beta)
  }
  # Use dbeta and pbeta to conditionally return the correct probability
}
