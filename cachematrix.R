## The 2 functions listed below when used together will solve for the inverse of a matrix
## and store the result to be retrieved later.  The motivation for the functions is to
## reduce compute time by not calculating the result everytime but instead store the result
## for later use.

## This function creates a special matrix object with 4 functions to be use to set and get
## the matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## This function checks to see if the inverse of a matrix x has been stored and if not, then
## calculates the inverse of matrix x and caches the inverse for later retrieval. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
