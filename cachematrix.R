## makeCacheMatrix returns a object that caches the inverse of a matrix
## cacheSolve returns the inverse of a matrix

## Returns an object that caches the inverse of a matrix and list of functions
## to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(m)
  {
     x <<- m
     inv <- NULL
  }
  get <- function() x
  setinv <- function(invMatrix) inv <<- invMatrix
  getinv <- function() inv
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
    )
}


## return the inverse of a matrix. x caches the inverse of a matrix,
## if an inverse is previously cached, it returns the cached copy instead
## of recomputing the inverse

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  ## Return a cached inverse if it exists
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  ## update the cached copy of the inverse on x
  x$setinv(inv)
  inv
}