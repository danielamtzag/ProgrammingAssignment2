## Let´s to calculate the inverse of a matrix, store arguments and variables in cache

## This function create a list of 4 fuctions, a function set the matrix and the other the inverse matrix  (set and setinverse)
## the other function get the matrix and the inverse matrix. (get and getinverse)
## X-  argument (square matrix)
## inverse- we have to store inverse(x) in this variable


makeCacheMatrix <- function(X = matrix()) {
  inverse <- NULL
  set <- function(M) {
    X <<- M
    inverse<<- NULL
    
  }
  get <- function() X
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
        }


## This function take as  arguments the above function results, if it don´t find the values in cache , calcule the inverse matrix.

cacheSolve <- function(X, ...) {
  inverse <- X$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- X$get()
  inverse <- solve(data, ...)
  X$setinverse(inverse)
  inverse
}


