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
    return(dpois(x, lambda))
  }
  if (type == "!="){
    # complement rule
    return(1-dpois(x, lamda))
  }
  if(type == "<="){
    # definition of CDF
    return(ppois(x, lambda))
  }
  if (type == "<"){
    # P(X < x) = P(X <= x-1)
    return(ppois(x-1, lambda))
  }
  if (type == ">="){
    # P(X >= x) = 1 - P(X <= x-1)
    return(1-ppois(x-1, lambda))
  }
  if (type == ">"){
    # P(X > x) = 1 - P(X <= x)
    return(1-ppois(x, lambda))
  }
}
beta.prob <- function(x, alpha, beta, prob, type="<="){
  if (type == "="){
    return(0)
  }
  # Use dbeta and pbeta to conditionally return the correct probability
}