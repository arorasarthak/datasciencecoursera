# Comments:
# Used camelCasing naming convention 
# 
# inverseReg -- Contains the inverse of the matrix
# 
# makeCacheMatrix() creates a list containing a function to do the following:
# 1. set the value of the matrix -- setMatrix()
# 2. get the value of the matrix -- getMatrix()
# 3. set the value of inverse of the matrix -- setInverse()
# 4. get the value of inverse of the matrix -- getInverse()

makeCacheMatrix <- function(x = matrix()) {
  inverseReg <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
    }
  getMatrix <- function() x
  setInverse <- function(matrixInverse) inverseReg <<- matrixInverse
  getInverse <- function() inverseReg
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
  }  
}


# cacheSolve() returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(inverseReg)) {
    message("getting cached data")
    return(inverseReg)
  }
  data <- x$getMatrix()
  inverseReg <- solve(data)
  x$setInverse(inverseReg)
  inverseReg
  }
}
