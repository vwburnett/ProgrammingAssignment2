## This function will save computational time by storing the inverse
## of a matrix in a cache, so when the same inverse is required for 
## a calculation later it can be called from the cache.

##In the makeCacheMatrix function we will first set the inv to NULL.
##This will change when the user sets the value.
##set function will set the matrix.
##get function will get the matrix.
##setinverse will set the inverse.
##getinverse will get the inverse.
##list function will encapsulate this into a list.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##cacheSolve will get the current state of the inverse.
##If this inverse already exists(meaning it is not NULL)
##the message "getting cached data" will appear and the inverse will be given.
##If the inverse is indeed NULL it will be calculated and returned.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
