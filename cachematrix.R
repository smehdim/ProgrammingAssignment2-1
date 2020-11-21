makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) j<<- inverse
  getInverse <- function() j
  list(set = set,get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message('Getting cached data')
    return(j)
  }
  
  tens <- x$get()
  j<- solve(tens,...)
  x$setInverse(j)
  j
}