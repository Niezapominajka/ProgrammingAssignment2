## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    #initialize objects, x (above as an argument for the function) and invrs (below as an empty matrix)
    
    ## MORE EXPALANTIONS: (according the text https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md) 
    ## invrs is set to NULL, initializing it as an object within the makeCacheMatraic() environment to be used by later code in the function.
    ## x is initialized as a function argument, so no further initialization is required within the function
    
    invrs <- NULL
    
    ## Now the code provides four basic behaviors that are typical for data elements within an object-oriented program. 
    ## The "getters" are program modules that retrieve data within an object, 
    ## The "setters" are program modules that set the data values within an object.
    
    
    # defining the set() function
    
    ## the <<- form of the assignment operator
    ## assigns the value on the right side  of the operator 'y' to an object in the parent environment (makeVector environ.) 
    ## named by the object on the left side of the operator 'x'. Other words: assigning the y to the x which is in makeVector environ.) 
    ## if there is already a valid inverse matrix cached in 'invrs', whenever x is reset, 
    ## the value of 'invrs' cached in the memory of the object is cleared, forcing subsequent calls to cachemean() 
    ## to recalculate the inverse matrix rather than retrieving the wrong value from cache.
    
    
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    
    #defining the get() function
    
    ## This function takes advantage of the lexical scoping features in R. 
    ## Since the symbol x is not defined within get(), R retrieves it from the parent environment of makeCacheMatrix().
    
    get <- function() x
    
    #defining the setinverse() function
    
    ## invrs is defined in the parent environment (makeCacheMatrix) and we need to access it after setinverse() completes, 
    ## so, the code uses the <<- form of the assignment operator to assign the input argument to the value of invrs
    ## in the parent environment (makeCacheMatrix).
    
    setinverse <- function(inverse) invrs <<- inverse
    
    #defining the getinverse() function, similar to get() function
    
    getinverse <- function() invrs
    
    ## At this point we have getters and setters defined for both of the data objects within our makeCacheMatrix() object.
    
    #assigning each of these functions as an element within a list(), and returns it to the parent environment.
    
    ## When the function ends, it returns a fully formed object to be used by downstream R code. 
    ## Each element in the list is named. This allows to use the $ form of the extract operator to access the functions
    ## by name rather than using the [[ form of the extract operator, to get the contents of the vector.
    
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## checking if the matrix already exists in cache and if yes just retrieves from there
## the list obtained from the makeCacheMatrix function above is an argument for this function
## The structure of the function is based on the cachemean function from assigment example

cacheSolve <- function(x, ...) {
    
## Return a matrix that is the inverse of 'x'


    invrs <- x$getinverse()
    
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    mat <- x$get()
    invrs <- solve(mat, ...)
    x$setinverse(invrs)
    invrs
    
}
