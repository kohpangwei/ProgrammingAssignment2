## These two functions makeCacheMatrix() and cacheSolve() allow you to 
## efficiently do repeated inversions of the same matrix by caching the result


## makeCacheMatrix takes in a matrix x and returns a special matrix object 
## that can cache its inverse. This object exposes the following functions:
## - isCached: has the inverse already been cached?
## - setInv: takes in a matrix inverse xInv and sets the cache to it
## - getInv: returns xInv
## - setData: takes in a new matrix x' and sets x to it, clearing the cache
## - getData: returns x

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  
  isCached <- function(){
    !is.null(xInv)
  }
  
  setInv <- function(inv){
    xInv <<- inv
    xInv
  }
  getInv <- function(){
    xInv
  }
  
  setData <- function(newX){
    x <<- newX
    xInv <<- NULL
  }
  getData <- function(){
    x
  }
  list(isCached = isCached,
       setInv = setInv, 
       getInv = getInv, 
       setData = setData, 
       getData = getData)
}


## cacheSolve takes in a matrix x that has been created with makeCacheMatrix()
## and returns the inverse of x. 
## If the inverse of x has already been computed, it returns the cached value
## and does not recompute the inverse.

cacheSolve <- function(x, ...) {
  
  if (!x$isCached()){
    message("Computing inverse...")
    x$setInv(solve(x$getData()))
  }
  x$getInv()
}
