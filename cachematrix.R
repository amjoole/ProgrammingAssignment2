## This function gets a matrix as input and creates a list of functions
## that 1-set matrix elements 2-get matrix elements 3-set the value of solve
## (invert of a matrix) 4- get the value of solve . You should save the result
## of this function in an object like A, then you can use the function you want
## by typing A$function()
makeCacheMatrix <- function(x = matrix()) {
  ms <- NULL
  ## ms stands for Matrix Solve (invert of the matrix)
  set <- function(y) {
    x <<- y
    ms <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) ms <<- solve
  getsolve <- function() ms
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This matrix gets a list like A as input, then checks if ms(matrix solve)
## is calculated before and saved in A$getsolve(), then prints the cached value
## otherwise it will get the value of input matrix, solve it, set the value of
## calculated invert matrix by using A$setsolve() and finally print the value of
## ms which is saved in global enviroment.

cacheSolve <- function(x, ...) {
  ms <- x$getsolve()
  if(!is.null(ms)) {
    message("getting cached data")
    return(ms)
  }
  data <- x$get()
  ms <- solve(data, ...)
  x$setsolve(ms)
  ms
        ## Return a matrix that is the inverse of 'x'
}
