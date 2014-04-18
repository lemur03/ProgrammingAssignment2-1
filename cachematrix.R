## Those functions create a special matrix which is really a list containing function to get/set of the matrix and its inverse
## The second function use the first one read the cache of the inverse matrix.

## This function create a matrix and provide four different methods
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the solve (inverse)
##    get the value of the solve (inverse)
## Example: x <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1.5))) 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse= setinverse,
         getinverse = getinverse)
}


## This method cache the inverse matrix of matrix of type MakeCacheMatrix
## Example : cacheSolve(x)
cacheSolve <- function(x, ...) {    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}