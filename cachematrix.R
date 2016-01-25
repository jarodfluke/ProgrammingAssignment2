## This function creates a "special" matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##create a "special" matrix object that can cache its inverse
  invdata <- NULL           ##initialize n as NULL; this will hold value of matrix inverse
  set <- function(y) {                             ##define the set function to assign new
    x <<- y                                       ## value of matrix in parent environment
    invdata <<- NULL                                ##if new matrix, reset invdata to NULL
  }
  get <- function() x     ##define the get function - returns value of the matrix aguement
  
  setinverse <- function(inverse) invdata <<- inverse ## assigns value of invdata in parent environment
  getinverse <- function() invdata              ## gets the value of invdata where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)## you need
  ## this in order to refer
  ##to the functions with the $ operator
}


## This function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##Return a matrix that is the inverse of 'x'
  invdata <- get$getinverse()
  if(!is.null(invdata)) {
    message("getting cached data")
    return(invdata)
  }
  data <- x$get()
  invdata <- solve(data,...)
  x$setinverse(inv)
  inv
}
