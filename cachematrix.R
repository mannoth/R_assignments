## The goal is to accept an invertible matrix, and output its
## inverse the following way: if the inverse of the supplied matrix
## is already in cache, then the output must be "getting 
## cached data" before returning the inverse of the
## matrix. Otherwise the output must only contain the inverse.

## This function accepts a matrix, and returns a list of 4 functions:
## The set(y) function can assign a value y to variable x, the get() 
## function returns the value of x, (which is a matrix), the setinverse(inverse)
## function stores the inverse of the matrix in the variable i, and the getinverse()
## function returns the actual value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function accepts the output of makeCacheMatrix, and gives
## back the inverse of the matrix x given to the makeCacheMatrix function.
## If the inverse of x is already in cashe, then the "getting 
## cached data" message is also returned.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}
