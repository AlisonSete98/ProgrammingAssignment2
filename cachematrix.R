##makeCacheMatrix and cacheSolve functions work together to cache a matrix's inverse
##and not needing to recalculate it every time one wants to see it.

##makeCacheMatrix will create a special object, a list of four functions related to
##matrix in question: 
#1. set(): sets the matrix;
#2. get(): returns the matrix;
#3. setinverse(): sets the matrix's inverse;
#4. getinverse(): gets the matrix's inverse.

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


## cacheSolve is the function that will do the calculation of the inverse. Once
##calculated, the inverse is cached and set to the special list created with the
##above function. If called again over the same matrix, it will print the message
##that the data is in cache before shows the inverse matrix.

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