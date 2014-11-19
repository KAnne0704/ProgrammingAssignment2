## Below are two functions that create a special "matrix" object that stores a matrix and caches its inverse

## makeCacheMatrix creates a matrix which is essentially a function that sets the value of a matrix, gets the value
## of that matrix, sets the value of the inverse of that matrix and gets the value of the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                        ## creating a list object to eventually store the matrix value
        set <- function(y) {                             ## set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                              ## get the original value of the matrix
        setinverse <- function(solve) m <<- solve        ## set the value of the matrix's inverse and store using superassignment
        getinverse <- function() m                       ## get the value of the matrix's inverse
        list(set = set, get = get,                       ## create a list for output
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve will compute, cache and return the inverse of the "special" matrix returned by the makeCacheMatrix 
## function above if the inverse hasn't been cached yet. If the inverse has been cached, the function will 
## retrieve the cached inverse
## We assume the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                              ## accesses the object 'x' and gets the value of its inverse                           
        if(!is.null(m)) {                                ## checks to see if the inverse has already been calculated
                message("getting cached data")           
                return(m)                                ## returns the inverse from the cache if it has already
        }                                                ## been calculated
        data <- x$get()                                  ## if inverse hasn't yet been created, returns original matrix
        m <- solve(data, ...)                            ## if inverse hasn't yet been created, computes matrix inverse
        x$setinverse(m)                                  ## store inverse calculated
        m                                                ## return the calculated inverse
}
