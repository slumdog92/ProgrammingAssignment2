## Put comments here that give an overall description of what your
## functions do
## The functions creates a matrix that can cache its inverse
## And the second one, computes the inverse. 


## Write a short comment describing this function
# The function makeCacheMatrix creates a special "matrix",
# which is really a list containing a function to:

# 1- Set the value of the matrix
# 2- Get the value of the matrix
# 3- Set the value of the inverse function
# 4- Get the value of the inverse function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setI <- function(solveM) {inv <<- solveM}
  getI <- function() {inv}
  list(set = set, get = get, setI = setI, getI = getI)
}



## Write a short comment describing this function
# The function makeCacheMatrix creates a special "matrix",
# which is really a list containing a function to:

# 1- Set the value of the matrix
# 2- Get the value of the matrix
# 3- Set the value of the inverse function
# 4- Get the value of the inverse function




## Write a short comment describing this function
# the function cacheSolve calculates the inverse function of the special "matrix"
# It checks to see if the inverse has already been calculated. 
# Otherwise, it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getI()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matI <- x$get()
  inv <- solve(matI,...)
  x$setI(inv)
  inv
}


### Test for function

dummyM <- makeCacheMatrix(matrix(5:8, 2, 2))
dummyM$get()
dummyM$getI()

cacheSolve(dummyM)
