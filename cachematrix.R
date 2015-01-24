## This function cache a square matrix(2X2) that is provided.
## It stores the inverse and provided it back when ever needed.

## The input to this function is a matrix. 
## There are get and set function to setting and getting the cached matix inverse

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## This function returns the inverse of the square matrix(2X2) provided.
## It take the inverse from the cache if it already have an inverse, thereby no need of calculating it again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x
  i <- x$getinverse()
  if(!is.null(i)){
    message("Getting from the cache")
    return(i)
  } 
  else {
    message("Not from the cache")
  }
  data <-x$get()
  #print(data)
  i <- solve(data)
  x$setinverse(i)
  i
}
