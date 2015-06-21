## these two functions help to cache an inverse of a matrix

## this function takes a square matrix and creates an "object" 
## that containes the matrix, its inverse and provides
## corresponding mutators and accessors
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
    {
      x <<- y
      inv <<- NULL
    }
  get <- function() x
  setsolved <- function(solved) inv <<- solved
  getsolved <- function() inv
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## this function takes an "object", that contains a matrix 
## (and probably its inverse) and returns its stored inverse 
## or calculates, stores and then returns its inverse
cacheSolve <- function(x, ...) 
{
  inv <- x$getsolved()
  if(!is.null(inv)) 
    {
      message("getting cached data")
      return(inv)
    }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolved(inv)
  inv
}