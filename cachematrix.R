# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  mat.inv <- NULL
  set <- function(y) {
    x <<- y
    mat.inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) mat.inv <<- inv
  getinv <- function() mat.inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinv function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  mat.inv <- x$getinv()
  if(!is.null(mat.inv)) {
    message("getting cached data.")
    return(mat.inv)
  }
  mat <- x$get()
  mat.inv <- solve(mat)
  x$setinv(mat.inv)
  mat.inv
}

# Test Results:

# > mat <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
# > mat$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# First run shows that the computation of the inverse is done and cached due to no previous computation of inverse stored in the cache.
# > cacheSolve(mat)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Second run shows that the value is retrieved from the cache and computation is skipped.
# > cacheSolve(mat)
# getting cached data.
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5