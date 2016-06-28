## makeCacheMatrix creates a special 'Matrix' object that allows its inverse 
## to be cached after that is calculated for the first time.  Thus making the 
## operation more efficient.
## The function cacheSolve takes a Matrix created by makeCacheMatrix and if its
## inverse has not yet been calculated it will calculate it and cache it.  
## Subsequent calls to the same matrix will return the cached inverse matrix.

## makeCacheMatrix takes a normal matrix and will return a special matrix 
## object to be used with cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## the set function stores the original matrix in set's parent env
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get will return the original matrix
    get <- function() x
    ## setInv will be used to cache the inverse matrix
    setInv <- function(inverse) i <<- inverse 
    ## getInv will return the cached inverse matrix
    getInv <- function() i
    
    #returning the new list containing the data and functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve takes a matrix created by makeCacheMatrix and tries to fetch the
## cached inverse matrix, if not found will calculate the inverse and cache it
## for later use.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'

    #if the matrix inverse is not calculated yet then this will return NULL
    i <- x$getInv()  #if x was not created with makeMatrix an error will fire
    
    #checking if the inverse matrix has already been calculated
    if(!is.null(i)) {
        message("Returning the cached inverse...")
        return(i)       #returning the stored inverse
    }
    
    #retrieving the original matrix
    data <- x$get()
    #calculating the inverse matrix
    i <- solve(data)
    #caching the inverse
    x$setInv(i)
    message("Inverse matrix cached.")

    #returning the calculated inverse
    i
}