## Functions that cache the inverse of a matrix
## Usage example:
## > source('cachematrix.R')
## >  m <- makeCacheMatrix(matrix(c(3, 0, 0, 3), c(2, 2)))
## >  cacheSolve(m)
##        [,1]      [,2]
##[1,] 0.3333333 0.0000000
##[2,] 0.0000000 0.3333333

## Create a special "matrix", which is a list containing a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(y) {
        x <<- y
        c <<- NULL
    }
    get <- function() x
    setinverse <- function(inve) c <<- inve
    getinverse <- function() c
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
    c <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(c)
    }
    m <- x$get()
    c <- solve(m, ...)
    x$setinverse(c)
    c
}
