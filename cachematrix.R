## Caching the inverse of a matrix

## returns a list that sets and gets the value of a matrix as well as the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # creating set function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # creating get function
    get <- function() x
    # creating setinverse function
    setinverse <- function(i) m <<- i
    # creating getinverse function
    getinverse <- function() m
    # returning list consisting of each function and their corresponding name
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## returns the inverse of the matrix, and if already calculated, retrieves it  
## from the makeCacheMatrix function

cacheSolve <- function(x, ...) {
    # setting variable m as the output from the getinverse function from x
    m <- x$getinverse()
    # checking if inverse has been calculated and returning the inverse matrix if so
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # if not, calculates the matrix inverse 
    data <- x$get()
    m <- solve(data, ...)
    # sets the value of the inverse in the cache as the one calculated
    x$setinverse(m)
    m
}
