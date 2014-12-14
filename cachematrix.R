## These two functions are used to create a special object 
## that stores a matrix and cache's its inverse

## This function creates a list, which containts 4 functions
## to get/set the value of the matrix and the inverse
makeCacheMatrix <- function(x = matrix())
{
  s <- NULL
  
  set <- function(y) 
  {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) s <<- solve
  
  getsolve <- function() s
  
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function calculates the inverse of the matrix 
## created with function "makeCacheMatrix" and stores
## it in the cache. If the cache already contains the
## inverse value, it won't be calculated again.
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
