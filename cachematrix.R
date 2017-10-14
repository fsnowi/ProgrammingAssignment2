
## The first function, makeCacheMatrix creates a special matrix 
## which does the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix (set as "inversem")
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
s <- NULL
set <- function(y) {
  x <<- y
  s <<- NULL
}
get <- function () x
setinversem <- function (inversem) s <<- inversem
getinversem <- function() s
list (set = set, 
      get = get,
      setinversem = setinversem,
      getinversem = getinversem)
}



## The following function calculates the inverse of the special matrix 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache 
## via the setinversem function.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  s <- x$getinversem ()
  if(!is.null(s)) {
    message("getting cached matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinversem(s)
  s
}
