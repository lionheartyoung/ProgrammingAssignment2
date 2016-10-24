## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  mtrxinv=NULL
  
  setmtrx <- function(y) {
    mtrx <<- y
    mtrxinv <<- NULL
  }
  getmtrx <- function() x
  setmtrxinv <- function(solve) mtrxinv <<- solve
  getmtrxinv <- function() mtrxinv
  list(setmtrx = setmtrx, getmtrx = getmtrx,
       setmtrxinv = setmtrxinv,
       getmtrxinv = getmtrxinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getmtrxinv()
  if (!is.null(minv)){
    message("getting cached inverse matrix")
    return(minv)
  }

  mtrx <- x$getmtrx()
  minv <- solve(mtrx)
  x$setmtrxinv(minv)
  minv
}
