## Two functions which calculates and caches the inverse of a matrix
    
## Creates a special matrix object that can cache its inverse.
## Function returns getters and setters for the matrix and its cached inverse (in a list)
    
makeCacheMatrix <- function(m = matrix()) 
    {
        ## initialises inverse object
        cached_inv <- NULL 
        
        ##function sets matrix
        set <- function(matrix) 
        {
            m <<- matrix
            cached_inv <<- NULL
        }
        ## function which returns matrix
        get <- function() m
        
        ## function sets inverse of matrix
        set_inv <- function(inverse) cached_inv <<- inverse
        
        ##function gets inverse of matrix (which has been cached)
        get_inv <- function() cached_inv
        
        ## Returns a list of the methods
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
        
        
        
    }
    
 
 ## Function retrieves and returns the inverse matrix if it has been cached. 
 ## IF it has not calculated and cached the inverse, then it does so.
 ## The function is used with the special matix created using the "createCacheMatrix" function
    
cacheSolve <- function(x, ...) 
    {
        ## get the matrix that is the inverse of x
        cached_inv <- x$get_inv
        
        ## if the inverse has been cached, then return the inverse
        if( !is.null(cached_inv) ) 
        {
            message("getting cached data")
            return(cached_inv)
        }
        
        ## Get the matrix from our object passed in the arguments
        matrix_data<-x$get()
        
        ## Calculates the inverse of the matrix by matrix multiplication
        m<-solve(matrix_data) %*% data
        
        ## Sets the inverse to the object passed in the arguments
        x$set_inv(m)
        
        ## Return the matrix
        m
        
    }
