## A pair of functions to allow for caching and retrieval of matrix
## inversion results without recalculation.

## Example usage:
##    myMatrix <- matrix(c(2,2,3,2), 2, 2)
##    x <- makeCacheMatrix(myMatrix)
##    cacheSolve(x)      -> prints inverse matrix
##    cacheSolve(x)      -> collects and prints inverse matrix 
##                                  (with "getting cached data" message)



## makeCacheMatrix is where the matrix value is get and set
## and also where the value of the inverse is get and set.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {  ## assigns a matrix value
        
        ## if a new matrix is sent to the function, it is stored
        ## and the value of the inverse is reset
        x <<- y
        inv <<- NULL
        
    }
    get <- function() x  ## gets the matrix
    
    setinv <- function(solve) inv <<- solve  ## asigns a value to the inverse
    
    getinv <- function() inv  ## retrieves the stored inverse 
                              ##(null if this has not yet been calculated)
    
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix created in the above function
## but checks to see if this has already been calculated before performing the operation
## returning the previously calculated value if present

cacheSolve <- function(x, ...) {
        
    inv <- x$getinv()    ## collects the value of the inverse from the special 'matrix'
                         ## created in teh above function
    
    if(!is.null(inv)) {
        message("getting cached data") # it knew it's own inverse
        return(inv);   ## so return that
    }
    
    ## if we get here, then it didn't know it's own inverse
    
    data <- x$get()
    inv <- solve(data, ...)  ## so calculate the inverse
    x$setinv(inv);  ## and store it with the matrix for future reference
    inv  ## return the calculated inverse
}
