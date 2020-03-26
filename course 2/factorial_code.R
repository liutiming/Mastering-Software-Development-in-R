##### functions ####

# Factorial_loop: a version that computes the factorial of an integer using looping (such as a for loop)
factorial_loop <- function(x){
  answer = 1
  if (x == 0) {
    return(answer)
  } else {
    for (i in 1:x) {
      answer = answer * i
    }
    return(answer)
  }
}

# Factorial_reduce: a version that computes the factorial using the reduce() function in the purrr package. Alternatively, you can use the Reduce() function in the base package.
multiply <- function(a,b){
  return(a*b)
}
factorial_reduce <- function(x){
  answer = 1
  

  
  if (x == 0) {
    return(answer)
  } else {
    return(reduce(1:x, multiply))
  }
}


# Factorial_func: a version that uses recursion to compute the factorial.
factorial_func <- function(x) {
  if(x == 0) {
    return(1)
  } else { 
    return(x * factorial_func(x-1))
  }
}
# Factorial_mem: a version that uses memoization to compute the factorial.

factorial_memory = c(1)
factorial_mem <- function(x) {
  if(x == 0) {
    return(1)
  } else if (is.na(factorial_memory[x])) { 
    factorial_memory[x] = factorial_mem(x-1) * x
    return(factorial_memory[x])
  } else {
    return(factorial_memory[x])
  }
}

#### benchmark ####
a <- c(1:100)
library(microbenchmark)

result <- microbenchmark(map_dbl(a, factorial_func),map_dbl(a, factorial_loop),map_dbl(a, factorial_reduce),map_dbl(a, factorial_mem))

