## The following functions calculate and chache the inverse
## of a matrix so that it does not need to be re-computed
## if its contents have not changed.

## makeCacheMatrix creates a special "matrix" which is actually
## a list containing functions to:
## 1.) set the value of the matrix
## 2.) get the value of the matrix
## 3.) set the value of the matrix inverse
## 4.) get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve uses the "matrix" created in the makeCacheMatrix
## function above to see if the inverse of the matrix has
## already been calculated. If it has, it gets the inverse
## from the cache using the defined getinverse function. 
## If not, it calculates the matrix inverse and sets the value
## in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
