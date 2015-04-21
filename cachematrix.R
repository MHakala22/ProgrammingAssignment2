## Functions computes inverse matrices. 
## The result is cached in memory, and returned if the function cacheSolve is re-used for the same data.


## Function creates list, which contains 4 functions with stored variables.
## "set" allows to set data.
## "get" returns data
## "setinverse" allows to define inverse matrix
## "getinverse" returns inverse matrix

makeCacheMatrix <- function(x = matrix()){
  I <- NULL ##Deletes inverse matrix, if exist.
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) I <<- solve
  getinverse <- function() I
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes inverse matrix for matrix stored in list created by makeCacheMatrix function. 
## If the inverse matrix for unchanged data is already cached, then the cached matrix is returned.

cacheSolve <- function(x, ...){
  I <- x$getinverse()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I ## Return a matrix that is the inverse of 'x'
}

