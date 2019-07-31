
## Return a cache-able matrix.
makeCacheMatrix <- function(the_matrix = matrix()) {
  inverse <- NULL

  # Setter & getter for the matrix itself.
  setMatrix <- function(the_matrix_arg) {
    the_matrix <<- the_matrix_arg
    inverse <<- NULL
  }
  getMatrix <- function() the_matrix

  # Setter & getter for the inverse matrix
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse

  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,

    setInverse = setInverse,
    getInverse = getInverse
  )
}


## inverts a cache matrix, returning the cached response if available.
cacheSolve <- function(x, ...) {
  # Check if inverse is cached, return cached version if so.
  storedInverse <- x$getInverse()
  if (!is.null(storedInverse)) {
    print("Fetching inverse from cache...")
    return(storedInverse);
  }

  # Calculate inverse live otherwise
  inverse <- solve(x$getMatrix())
  x$setInverse(inverse)
  inverse
}
