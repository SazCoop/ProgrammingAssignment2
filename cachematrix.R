## These functions calculate the inverse of a matrix and cache it. If this matrix has previously been inverted (and thus cached) then it will retrieve this from the cache instead of calculating it again

## This function converts a matrix type into a list containing the functions necessary to cache and retrieve the inverse

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  seti <- function(solve) m <<- solve
  geti <- function() m
  list(set = set, get = get,
       seti = seti,
       geti = geti)
  
}


## This function checks to see if the inverse of the matrix has already been cached.
## If it has been cached then it will retrieve it, otherwise it will calculate the inverse and cache that value

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$geti()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$seti(m)
  m
  
}