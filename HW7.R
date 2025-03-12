# Henry Sun 
# HW 7 
# 3/12/25

# want to compute P(X = x), P(X != x), P(X < x), P(X <= x), 
# P(X > x), P(X >= x) 

# prob was removed as parameter of the function, as prob would only be used in 
# with qpois (inverse CDF)
pois.prob <- function(x, lambda, type="<="){
  if (type == "="){
    # definition of PMF
    return(dpois(x, lambda))
  }
  if (type == "!="){
    # complement rule
    return(1-dpois(x, lambda))
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

# prob removed as a parameter
# according to pbeta, alpha, beta must be nonnegative
beta.prob <- function(x, alpha, beta, type="<="){
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
    return(pbeta(x, alpha, beta))
  }
  if (type == ">" | type == ">="){
    # P(X > x) = P(X >= x) = 1 - P(X < x)for continuous distributions 
    return(1 - pbeta(x, alpha, beta))
  }
}
