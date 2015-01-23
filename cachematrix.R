## Put comments here that give an overall description of what your
## functions do

# This function generates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(originalMatrix = matrix()) {
  inverseMatrix <- NULL
  set <- function(x) {
    originalMatrix <<- x
    inverseMatrix <<- NULL
  }
  get <- function() originalMatrix
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


# This function computes the inverse of the "matrix" object returned by `makeCacheMatrix` 
# above. If an inverse has already been computed, `cacheSolve` will simply red the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("Getting cached data...")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  return(inverseMatrix)
}
