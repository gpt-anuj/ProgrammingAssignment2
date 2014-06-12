## Inversing a matrix is costly operation, so often we would like to cache invsrse of matrix
## To Do that we create a special matrix makeCacheMatrix that caches it's inverse via 
## scoping rules in R using <<- operator 
## cacheSolve function is created to using the caching. We can get inverse of a matrix 
## using cacheSolve function. If inverse is found in Cache it is returned , else cacheSolve will
## generate inverse of matric using solve() and store it in cache of makeCacheMatrix
## For simplicity it is assumed that solve will work correctly and give back inverse of matrix
# Test this function
# Load Source   : source("cacheMatrix.R")
# Create Special Matrix and assign to a: a<- makeCacheMatrix(matrix(1:4,2,2))
# Get Inverse of Matrix, it will be computed : cacheSolve(a)
# Again get Inverse , it will come from cache : cacheSolve(a) 
# See getting cached data being printed on console

## makeCacheMatrix creates a matrix of special type 
## that has capability to cache it's inverse matrix. 
## getinverse and setinverse functions returns gets and sets inverse of matrix
## return list of all supported functions on this Vector in list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns inverse of matrix created as part of argument of 
## makeCacheMatrix. It asks for cached copy of inverse matrix, if that copy is found
## it uses it directly, otherwise it creates inverse of matrix using solve() function 
## and sets it on makeCacheMatrix, so that it can be cached.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}