## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function will make a "matrix" that can store or cache the inverse of the matrix
## By caching the inverse of the matrix R will not have to calculate the inverse every 
## time an inverse is needed and will speed up processing

makeCacheMatrix <- function(x = matrix()) { ## Store a function whose args are determined by the matrix() function
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function()inv
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## This function will calculate the inverse of the matrix generated with "makeCacheMatrix".
## If this value has already been computed, it will call the value rather than re-computing.
## If the matrix has changed from original matrix generated in makeCacheMatrix then the function
## will calculate and store the inverse of the new matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data, please wait!")
    return(inv)
  }
  mat <-x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
