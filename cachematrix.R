## The following is pair of functions to first cache, then compute the
## inverse of a matrix. 

## First we use an anonymous function to create a special matrix object 
## that will be enabled to cache its inverse:
makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y) { # This sets the function.
          x <<- y
          c <<- NULL      
    }    
    get <- function() x
    setInverse <- function(inverse) c <<- inverse
    getInverse <- function() c
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
  }

## If the matrix is unchanged and the inverse has already been calculated, 
## cacheSolve will retrieve the inverse from the cache rather than taking 
## the extra time to rerun the function. 

## Note: It will only remain in the cache while your current R session is open.

cacheSolve <- function(x, ...) {
  c <- x$getInverse()
  if(!is.null(c)) { # Checking NULL for cached data.
    message("Please wait, retrieving cached data.")
    return(c)
  }
  data <- x$get()
  c <- solve(data) # Solving data, then save.
  x$setInverse(c)
  c
}
