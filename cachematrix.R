## The two functions below create a special "matrix" object (which is really a 
## list of operations) that can cache a given matrix and then calculate and
## cache the inverse of the matrix. The cached results can be used in the future
## without having to perform the inverse calculation every time.


## The makeCacheMatrix function takes a square, invertible matrix (x) and sets
## its value in the cache. Next, the function get the matrix from the cache,
## calculates the inverse matrix and sets the result in the cache.

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinv <- function(solve) v <<- solve
        getinv <- function() v
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function returns a matrix that is the inverse of 'x'. First,
## it checks to see if the inverse matrix has already been calculated. If so, it
## gets the inverse from the cache. If not, it calculates the inverse and sets
## the value of the matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        v <- x$getinv()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinv(v)
        v
}
