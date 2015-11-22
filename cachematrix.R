## The set of functions in cachematrix.R offers the ability to calculate and cache the inverse of a provided matrix
## Sample Usage: 
## x<-makeCacheMatrix(matrix(c(1,2,3,4),nrow = 2,ncol = 2))
## cacheSolve(x)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## makeCacheMatrix will store a matrix passed as a parameter as well as the matrix inverse
## the function offers methods in order to get/set the original as well as the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    invmtx <- NULL
    set <- function(y) {
        x <<- y
        invmtx <<- NULL
    }
    get <- function() x
    setinv <- function(inv) invmtx <<- inv
    getinv <- function() invmtx
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve receives as a parameter a list that contains functions that will get/set a matrix and its inverse
## and will return the inverse of the matrix contained in x as long as it is already calculated or calculate the
## inverse, store it in x and return it

cacheSolve <- function(x, ...) {
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
