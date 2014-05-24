## The function "makeCacheMatrix creates a special matrix that can "cached"
## the inverse of the matrix in case it is need to be used more than one time

## The "cache matrix" take as argument a inversible matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                               ##initialize the value of the inverse in null
  set <- function(y) {                      ## it sets a new value to the matrix and,because it is a new matrix,
    x <<- y                                 ## the Inverse is not calculated so inv<-NULL
    inv <<- NULL
  }
  get <- function() x                       ##get has the value of the matrix x
  setInv <- function(Inverse) inv <<- Inverse    ##setInv is used to set value of the inverse of the matrix x
  getInv <- function() inv                  ##getInv has the value of the inverse of the matrix x
  list(set = set, get = get,
       setInv = setInv,                     ##Our new "cache Matrix" has 4 "methods" get, set, getInv, setInv
       getInv = getInv)
  
}


## This function is used to solve the value of the inverse of "x". 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()                       ##if the Inverse is already solved
  if(!is.null(inv)) {                     ##return the actual value
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                         ##if the Inverse is not solved, it 
  inv <- solve(data, ...)                 ##is solved, and then the function return the value
  x$setInv(inv)
  inv
}
