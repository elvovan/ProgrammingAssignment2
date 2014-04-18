###This files contains set of functions for work with cached value of inverse of a matrix

## makeCacheMatrix is a "constructor" that constructs minimal set of operations
#needed for work with cached value of inverse of a matrix
makeCacheMatrix <- function(x = matrix())
{
  #cached value of the inverse
  s <- NULL
  
  #sets new value to the matrix and resets pre-cached value of the inverse
  set <- function( y )
  {
    #clearing pre-solved inverse
    s <<- NULL
    #setting new matrix
    x <<- y
  }
  
  #returns matrix itself
  get <- function() x
  
  #sets new cache
  setsolve <- function(solve) s <<- solve
  
  #returns cached value
  getsolve <- function() s
  
  #forming our "class" and returning it
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


##the function returns the inverse of the matrix created by the previous function
#checking if the value should be computed or just extracted from cache

cacheSolve <- function(x, ...) {
  
  #getting cached value
  s <- x$getsolve()
  
  #the value is not null if it was already computed and cached
  if( ! is.null(s) )
  {
    message("getting cached data")
    return(s)
  }
  
  #extracting original matrix
  data <- x$get()
  
  #computing the inverse
  s <- solve(data, ...)
  
  #storing the inverse in the cache
  x$setsolve(s)
  
  #returning a matrix that is the inverse of 'x'
  s
}
