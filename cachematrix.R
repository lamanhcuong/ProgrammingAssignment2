## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## Inputs: x - a normal matrix which is cached
## Outputs: a list of 4 items: set, get, setinverse, getinverse
## - get(): return current cached matrix
## - set(nx): set cached matrix to a new matrix nx
## - getinverse(): return the cached inversed matrix
## - setinverse(ix): set the cached inversed matrix to ix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
## Inputs: x - a cached matrix which is created from makeCacheMatrix function
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}

## ** Example code to demonstrate using the cache matrix
## ** Create a normal 10x10 matrix (mt) and view content of the matrix
## >mt <- matrix(rnorm(100), 10, 10)
## >mt
## ** create the cache matrix (cmt) by calling function makeCacheMatrix and pasing argument mt to the function 
## >cmt <- makeCacheMatrix(mt)
## ** View content of the cached matrix with cmt$get(), should be the same with mt
## >cmt$get()
## ** call cacheSolve function with passing cmt argument the first time, the inverse matrix is calculated
## >cacheSolve(cmt)
## ** call cacheSolve function the second time, the cached inverse matrix is return ("getting cached data" text is displayed)
## >cacheSolve(cmt)


