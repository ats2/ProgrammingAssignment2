## A pair of functions that cache the inverse of a matrix. 

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set the values of the matrix
  set <- function(matrix) {
    x <<- matrix
    m <<- NULL
  }
  # get the matrix
  get <- function() x
  
  # set the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  # get the cached inverse of the matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cachesolve: calculates the inverse of the special matrix created with makeCacheMatrix
## first checks if the inverse has already been calculated. If yes gets the inverse from the cache
## else calculates the inverse of the data and sets the value of the inverse in the cache 
## via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## if already set return the cached inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## else get the matrix, calculate, cache & return the inverse
  data <- x$get()
  m <- solve(data) 
  x$setinverse(m)
  m
}