## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function calculates the inverse of a matrix inputted as X and cache the inverse matrix,
## The function returns a list of function, 1. set the value of a matrix; 2. get the value of a matrix; 3. Calculate the inverse of the matrix; 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list( set = set, get = get, setinv = setinv, getinv = getinv)
} 


## The cacheSolve function retrieves the cache matrix from the makeCacheMatrix and print the inverse matrix, 
## if there is no cached matrix, it calculates the inverse of a given matrix in the makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

