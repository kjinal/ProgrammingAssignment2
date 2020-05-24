## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function defines functions used for storing the value of x, retrieving the value of x,
# storing the inverse of x and retrieving inverse of x
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

        
## Write a short comment describing this function
# this function first checks if the inverse of x was already calculated or not.
# If the inverse was already calculated then it is retrieved from the stored variable
# If the inverse was not previously calculated then inverse is calculated using solve function
# and the inverse is also stored in a variable for future use. 
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
