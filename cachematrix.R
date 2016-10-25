## Put comments here that give an overall description of what your
## functions does

## makeCacheMatrix read in a invertable matrix, and create a list of
## of 4 functions to operate the matrix
## setmtrx will set the mtrx to the matrix you provided.
## getmtrx will printout the matrix you provided to the function
##  makeCacheMatrix()
## setmtrxinv will set the inverse of the matrix
## getmtrxinv will printout  the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  #set inverse to be NULL
  mtrxinv=NULL
  
  #define the set matrix function
  setmtrx <- function(y) {
    mtrx <<- y
    #set inverse to be NULL
    mtrxinv <<- NULL
  }
  #define the get matrix function
  getmtrx <- function() x
  #define the set inverse matrix function
  setmtrxinv <- function(solve) mtrxinv <<- solve
  #define the get inverse matrix function, Printout the inverse matrix
  getmtrxinv <- function() mtrxinv
  #return a list contains four functions 
  list(setmtrx = setmtrx, getmtrx = getmtrx,
       setmtrxinv = setmtrxinv,
       getmtrxinv = getmtrxinv)
}


## Cachesolve use input of a object which is the output of makeCacheMatrix,
## and 
## 1: compute and return the inverse matrix of the object, if the inverse matrix is NULL.
## 2: return the cached inverse matrix, if the inverse matrix already exists.

cacheSolve <- function(x, ...) {
       
 #get the current inverse matrix
  minv <- x$getmtrxinv()
  #if the current inverse matrix already exists, then return the inverse matrix.
  if (!is.null(minv)){
    message("getting cached inverse matrix")
    return(minv)
  }

  #if the current invermatrix does not exist, then solve it.
  mtrx <- x$getmtrx()
  minv <- solve(mtrx)
  #set the inverse matrix and return it.
  x$setmtrxinv(minv)
  minv
}
