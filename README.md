
#### Caching the Mean of a Vector


<!-- -->

    makeVector <- function(x = numeric()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setmean <- function(mean) m <<- mean
            getmean <- function() m
            list(set = set, get = get,
                 setmean = setmean,
                 getmean = getmean)
    }



    cachemean <- function(x, ...) {
            m <- x$getmean()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- mean(data, ...)
            x$setmean(m)
            m
    }

### Assignment: Caching the Inverse of a Matrix

Matrix inversion is usually a costly computation and there may be some
benefit to caching the inverse of a matrix rather than computing it
repeatedly (there are also alternatives to matrix inversion that we will
not discuss here). Your assignment is to write a pair of functions that
cache the inverse of a matrix.

Write the following functions:

### 1.  `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.

    makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initially, the cached inverse is NULL
  
  set <- function(y) {
    x <<- y       # set the matrix value
    inv <<- NULL  # reset cached inverse
  }
  
  get <- function() x  # return the matrix
  
  setinverse <- function(inverse) inv <<- inverse  # cache the inverse
  getinverse <- function() inv  # return cached inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

### 3.  `cacheSolve`: This function computes the inverse of the special
  ###  "matrix" returned by `makeCacheMatrix` above. If the inverse has
  ###  already been calculated (and the matrix has not changed), then
  ###  `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # try to get cached inverse
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()        # get the matrix
  inv <- solve(data, ...) # compute inverse
  x$setinverse(inv)       # cache it
  inv                     # return inverse
}

### Computing the inverse of a square matrix can be done with the `solve`
### function in R. For example, if `X` is a square invertible matrix, then
### `solve(X)` returns its inverse.

# create a 2x2 invertible matrix
X <- matrix(c(2, 1, 1, 2), 2, 2)

# compute its inverse
solve(X)


