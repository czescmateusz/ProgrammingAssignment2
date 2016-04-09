## Put comments here that give an overall description of what your
## functions do

#makeCachematrix provides a list with a function returning the value of a the matrix(set), 
#getting the value(via get), sets the inverse of a matrix, and gets the inverse of a matrix
# makeCacheMatrix creates a list containing a function to
#I'm also using the  ginv function from the MASS package to compute the inverse of non-square matrixes

makeCacheMatrix <- function(x = matrix()) {
  require(MASS)
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(ginv) inv <<- ginv
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The function returns the inverse of a matrix via a ginv function from the MASS package.
# If the inverse was already calcuated it returns the value and skips the computation

cacheSolve <- function(x, ...) {
  require(MASS)
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- ginv(data)
  x$setinverse(inv)
  inv
}

#Sample of function
matrix(1:6, 2, 3)
m <- makeCacheMatrix(matrix(1:6, 2, 3))
cacheSolve(m)






