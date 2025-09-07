

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL  
  
  set <- function(y) {
    x <<- y       # set the matrix value
    inv <<- NULL  # reset cached inverse
  }
get <- function() x  
  
  setinverse <- function(inverse) inv <<- inverse  # cache the inverse
  getinverse <- function() inv  # return cached inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
inv <- x$getinverse()  # try to get cached inverse
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()        
  inv <- solve(data, ...) 
  x$setinverse(inv)       
  inv                     
}
