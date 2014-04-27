## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function


## This function creates the matrix

makeCacheMatrix <- function(x = matrix()) {

  m<- NULL  ## Initialise the variable, thus setting it to null

  set<- function(y)
  {
    x <<- y
    m <<- NULL 
  }
  get  <- function() x ## Display matrix
  setinverse  <- function(inverse) m<<- inverse
  getinverse  <- function() m
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


##This function computes the inverse matrix

cacheSolve<- function(x, ...) {


  m  <- x$getinverse()

  if (!is.null(m))  ##Check wheter matrix is already cached
  {
  	message("getting cached data")
  	return(m)
  }
  data<- x$get()  ##reads matrix elements and assigns it to variable data
  m<- solve(data, ...)
  x$setinverse(m)
  m  ## return inverse matrix variable
  }
