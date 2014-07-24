## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
    
    ## Initialize, then set and get the matrix, returning the matrix itself
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    set_inverse <- function(inverse) {
        m <<- inverse
    }
    get_inverse <- function() {
            m
    }
    
    ## Return a list of all the methods
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not
## changed), then the "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Reference to list created in first function
    m <- x$get_inverse()
    
    ## Check if m is already in cache, then if it is, just return the already calculated inverse
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    ## Getting the matrix from the list object
    data <- x$get()
    
    ## Using the solve function (matrix multiplication) to calculate the inverse of the matrix
    m <- solve(data)
    
    ## Setting the inverse to the object
    x$set_inverse(m)
    
    ## Return the matrix
    m
}
