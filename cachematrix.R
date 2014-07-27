## These functions speed up the computation of the inverse of a matrix by 
## storing previous inverse computations in a cache. If the user or calling 
## function has inverted the matrix before, then that cached value is returned. If
## not, the inverse is computed as normal and stored for possible future use.
## The matrix must be stored withing an object of makeCacheMatrix type.
## Usage:
## myCacheMat <- makeCacheMatrix(myMat)
## myCacheInv <- cacheSolve(myCacheMat)

## makeCacheMatrix creates a new object that contains a matrix and its
## inverse. Initially the inverse is NULL, but if the user or a calling 
## function calls cacheSolve on the same object, the inverse will be
## computed and stored for future use. The function provides functions
## enabling the calling function or user to get or set the object, and 
## get or set the inverse of the contained matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize object's input matrix's inverse to NULL
    inverse <- NULL
    
    # set enables matrix insice an existing object to be 
    # overwritten by the calling function or user
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # get returns the matrix stored in this object
    get <- function() x
    
    # setInverse enables calling funcion to set the inverse of the matrix
    # in this object. Input argument is a matrix.
    setInverse <- function(inv) inverse <<- inv
    
    # get inverse of matrix stored in this object
    getInverse <- function() inverse
    
    # define the fields and methods of this class; return as a list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of a matrix contained 
## within a makeCacheMatrix object. If the inverse has 
## already been computed for the passed in object, the
## cached value is returned. If it hasn't been computed,
## then the inverse is computed and stored for future use.

cacheSolve <- function(x, ...) {
    
    # Retrieve inverse from this object.
    inverse <- x$getInverse()
    
    # Inverse exists already -- return it with a message 
    # that it was cached.
    if (!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    
    # Inverse doesn't exist yet. Compute it with R 'solve' 
    # function and store it in this object via the setInverse() 
    # function from the object's specification.
    mat <- x$get()
    inverse <- solve(mat)
    x$setInverse(inverse)
    inverse
}

