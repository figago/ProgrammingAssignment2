#' R-Programming 2nd assigment
#' Lexical Scoping
#' 
#' The first function `makeCacheMatrix` creates a special matrix
#' which is a list with a getter, a setter, an inverse getter, an inverse setter.
#' The specificity is that the inverse is only computed once on the first call
#' to the inverse getter, and then cached so that subsequent calls retrieve the 
#' cached inverse.
#' When the matrix changes (with the `set` call), the cached inverse is cleared.
#' 
#' The second function makes use of the first one to compute the matrix inverse.
#' 
#' Note: We assume that the matrices used here are invertible.

#' This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getInverse <- function() inv
  setInverse <- function(i) {
    inv <<- i
  }
  list(set=set,
       get=get,
       getInverse=getInverse,
       setInverse=setInverse)
}


#' This function computes the inverse of the special "matrix"
#' returned by makeCacheMatrix above.
#' If the inverse has already been calculated (and the matrix has not changed),
#' then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mdata <- x$get()
  inv <- solve(mdata)
  x$setInverse(inv)
  inv
}
