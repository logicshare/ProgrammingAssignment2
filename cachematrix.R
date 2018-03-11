## cacheSolve function uses the list of functions returned by makeCacheMatrix function to get the inverse of a matrix from cache or otherwise compute the inverse and sets the value.

## makeCacheMatrix function returns a list of functions to set or get the matrix or the inverse of matrix passed.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv_p) inv <<- inv_p
  
  getinv <- function() inv
  
  list(set = set, get = get,setinv = setinv,getinv = getinv)


}


## cacheSolve function returns the inverse of a matrix if it is available in cache. otherwise computes, set the inverse in cache and returns the inverse.

cacheSolve <- function(x, ...) {
								## Return a matrix that is the inverse of 'x'
		
  inv <- x$getinv()

  if(!is.null(inv)) {
  
    message("getting cached inverse matrix")
    
	return(inv)
  }
  
  data <- x$get()
  
  message("Computing the inverse matrix as cached value is not available.")
  
  inv <- solve(data)
  
  message("Sets the computed inverse matrix in cache to be used later for the same matrix.")
  
  x$setinv(inv)
  
  inv		
		
}
