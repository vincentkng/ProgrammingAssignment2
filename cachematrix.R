#  Create a object that stores a matrix and cache's its inverse.
# Input : Input matrix
makeCacheMatrix <- function(x = matrix()) 
{
  im <- NULL
  
  setMatrix <- function(y) 
  {
    x <<- y
    im <<- NULL
  }
  
  getMatrix <- function() x
  
  setInverse <- function(imat) im <<- imat
  
  getInverse <- function() im
  
  list(setMatrix=setMatrix, getMatrix=getMatrix,
       setInverse=setInverse,
       getInverse=getInverse)

}#makeCacheMatrix


# Return the inverse of a given matrix. 
# Return cached version if possible.
# Input : Input matrix
# Output: Inverse of matrix
cacheSolve <-function(x, ...)
{
  imat <- x$getInverse()
  
  if(!is.null(imat)) 
  {
    message("getting cached data")
    return(imat)
  }#end if
  
  mat <- x$getMatrix()
  imat <- solve(mat)
  x$setInverse(imat)
  imat
}#cacheSolve
