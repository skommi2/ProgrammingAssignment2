## For second programming assignment, we are building a pair of functions
## that compute inverse of a matrix and cache the results for subsequent use.

## Function that holds matrix and inverse of matrix for later use.

makeCacheMatrix <- function(x = matrix()) {
    invmtrx <- NULL
    set <- function(y) {
        x <<- y
        invmtrx <<- NULL   # reset inverse matrix
    }
    get <- function() x
    setinverse <- function(z) invmtrx <<- z
    getinverse <- function() invmtrx
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function that computes the inverse of matrix.
## It computes and saves the result on the first use and
## returns the cached value after that.
## Please note that the input is a special matrix made with makeCacheMatrix()

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

############################################
#Test:
# Sample inversible matrix
#A <- matrix(c(1, 2, 3,  2, 3, 0,  0, 1, 2), nrow=3, byrow=TRUE)
#
#A1 <- makeCacheMatrix(A)
#cacheSolve(A1)
#cacheSolve(A1)  # should be from cache

#A2 <- makeCacheMatrix(A)
#> A2$get()
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    2    3    0
#[3,]    0    1    2
#> A2$getinverse()
#NULL
#
#cacheSolve(A2)   # new special matrix. recomputes the inverse
#
############################################
