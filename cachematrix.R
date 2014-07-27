##The file contains two functions which calculate the inverse of a matrix.
##the makeCacheMatrix function creates an matrix object which "remembers"
##its inverse in its environment(caches it) so that later we need not 
##calculate inverse but only read(get) the stored inverse of the matrix.
##the cacheSolve function when called the first time calculates the inverse
##and stores it in the matrix object created by makeCacheMatrix.On subsequent
##calls for inverting the same matrix,It checks whether the inverse is cached
##and returns the cached value.

## This function makeCacheMatrix creates a special "matrix" 
##object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##the cacheSolve function below checks whether the inverse is already calculated
##for the passed matrix.If the inverse is cached, it returns it ,or else, it calculates
##the inverse and stores it in the cached matrix inverse using the setInverse function
##of the "special" matrix object and returns the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
