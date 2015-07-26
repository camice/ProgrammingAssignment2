## The following is pair of functions to first cache, then compute the
## inverse of a matrix. 

## First we use an anonymous function to create a special matrix object 
## that will be enabled to cache its inverse:
makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y) { # This sets the function to be applied.
            x <<- y
            c <<- NULL      
    }    
    get <- function() x # This gets the function.
    setInverse <- function(inverse) c <<- inverse # This sets the inverse of
                                                  # the matrix.
    getInverse <- function() c # This gets the inverse of the matrix.
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
  }

## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix. If the matrix is unchanged and the inverse has already 
## been calculated, cacheSolve will retrieve the inverse from the cache rather 
## than taking the extra time to rerun the function. 

## Note: It will only remain in the cache while your current R session is open.

cacheSolve <- function(x, ...) {
        c <- x$getInverse()
        if(!is.null(c)) { # Checking NULL for cached data.
                message("Please wait, retrieving cached data.")
        return(c)
  }
  data <- x$get()
  c <- solve(data) # Solves data, then saves.
  x$setInverse(c)
  c
}
