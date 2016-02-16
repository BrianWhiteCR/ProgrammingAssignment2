## Assignment: Programming Assignment 2: Lexical Scoping
## This function will generate the Inverse of a Matrix
## and use caching to retrieve the values if they have previously
## been calculated, rather than always recalculating which
## can be a costly computation.

## The first function, makeCacheMatrix, creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## Note from my testing this will only work on a "Square" matrix
## If I was going to go further with this program I would have to
## account for a non-square matrix being entered and resolve it
## with an error message or some such prompt.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y # this refers to the parent directory, not set within the function
        i <<- NULL # as above - parent directory, not set within the function
    }
    get <- function() x # get the matrix x
    setinverse <- function(inverse) i <<- inverse # set the cache i to be the inverse of the matrix
    getinverse <- function() i # get the inverse cached value if calculated
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message ("getting cache data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
