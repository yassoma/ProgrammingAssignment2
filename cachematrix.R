#The purposes of those two functions is to be able to cache the inverse of a matrix.


#The first function, makeCacheMatrix creates a special
#matrix object, which is a list containing functions in
#order to:

#- set/get the value of the matrix.
#- set/get the value of the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) s <<- solve
        getmatrix <- function() s
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


#The second function, cacheSolve, will calculate the
#inverse of the matrix created by the first function
#and sets the value through 'setmatrix', this is
#if hadn't been already calculated. Otherwise, it will
#skip the calculation and will retrieve the value of
#the matrix from cache.

cacheSolve <- function(x, ...) {
        s <- x$getmatrix()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        matrix <- x$get()
        s <- solve(matrix, ...)
        x$setmatrix(s)
        s
}
