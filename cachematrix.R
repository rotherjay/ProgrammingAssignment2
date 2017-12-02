## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object
## that can cache its inverse.
##
## Arguments:
##   x: A matrix but can be set using setter of matrix
## 
## Returns:
##   A list with functions to get/set of matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Setter of matrix
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Setter of matrix
  getMatrix <- function() x
  
  ## Setter of inverse matrix
  setInverseMatrix <- function(inverse) inv <<- inverse
  
  ## Getter of inverse matrix
  getInverseMatrix <- function() inv
  
  ## Returns a list of function of getter/setter of matrix and inverse matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
##
## Args:
##   x: A matrix
##   ...: Extra arguments
##
## Returns:
##  a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverseMatrix()
  
  ## return cached matrix inverse if it's been already computed
  if (!is.null(inv)) {
    message("given inverse is already cached")
    return(inv)
  }
  
  ## Compute the inverse of matrix using solve() 
  mtx <- x$getMatrix()
  inv <- solve(mtx, ...)
  
  ## Set the inverse of matrix to cache
  x$setInverseMatrix(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
