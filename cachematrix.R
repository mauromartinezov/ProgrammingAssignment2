    ## These functions work together to cache the inverse of a matrix.

    ## This helps avoid redundant and potentially expensive computations
    ## by retrieving the previously calculated inverse if the matrix 
    ## has not changed.

    ## This function creates a special object that stores a matrix and 
    ##caches its inverse.
    
    ## It provides methods to set and get the matrix, 
    ## as well as to set and get the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y){
    x <<- y
    inversa <<- NULL
  }
  get <- function()x
  setInverse <- function(inversacal) inversa <<- inversacal
  getInverse <- function() inversa 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
    ## This function computes the inverse of the matrix stored in the 
    ## special object returned by "makeCacheMatrix".

    ## If the inverse has already been calculated 
    ##(and the matrix has not changed),it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inversa <-x$getInverse()
  if(!is.null(inversa)){
    message("getting cached data")
    return(inversa)
  }
  
  mat <- x$get()
  inversa <- solve(mat,...)
  x$setInverse(inversa)
  inversa
        ## Return a matrix that is the inverse of 'x'
}
