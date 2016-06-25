## This function takes a matrix as input and creates a special matrix object(list)
## It stores input matrix, its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    #This function sets the x to the input matrix and m to NULL
    set <- function(input) {
        x <<- input
        m <<- NULL
    }
    
    #Function to call input matrix
    get <- function() x
    
    #This function stores the inverted matrix
    cacheInverse <- function(z) {
        m <<- z
    }
    
    #Function to call inverted matrix
    getInverse <- function() m
    
    list(set = set, 
         get = get, 
         cacheInverse = cacheInverse, 
         getInverse = getInverse)
}


## This function takes the special matrix object as input, checks if it is already inverted and returns 
##  the inverted matrix.

cacheSolve <- function(y= matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverseMatrix <- y$getInverse()
    
    # Checking if the input matrix is already inverted
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    
    inputMatrix <- y$get()                  #Calling to get input matrix
    inverseMatrix <- solve(inputMatrix)     #inverting the input matrix
    y$cacheInverse(inverseMatrix)           #Caching the inverted matrix
    
    inverseMatrix
}