## cachematrix.R
##
## Lauren Rousseau 
##
## 22/02/2015

## The function makeCacheMatrix extends the normal matrix with the ability 
## to store a cached version of the matrix, which in this case is the 
## inverted matrix.
## 
## The function cacheSolve takes an extended matrix as a parameter and
## extends the Solve function to return a cached 
## version of the inverted matrix if one is available. This function
## assumes that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.
## There are 4 functions:
## set(y) which updates the matrix contents.
## get() which returns the matrix.
## setinvertmatrix(invertmatrix) which stores the cached inverted matrix value.
## getinvertmatrix() which returns the stored value of the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        ## initialise the cached value to null
        m <- NULL
        
        set <- function(y) {
                ## set value of matrix to input and cached matrix to null.
                x <<- y
                m <<- NULL
        }
        
        get <- function() {
                ## return value of matrix
                x
        }
        
        setinvertmatrix <- function(invertmatrix) {
                ## set the value of the inverted matrix
                m <<- invertmatrix
        }
        
        getinvertmatrix <- function() {
                ## return the value of the inverted matrix
                m
        }
        
        ## return extended matrix as a list
        list(set = set, 
             get = get,
             setinvertmatrix = setinvertmatrix,
             getinvertmatrix = getinvertmatrix)
}


## This function returns the inverse of the extended matrix
## returned by function makeCacheMatrix. If the inverse has 
## already been calculated (and the matrix has not changed), then
## this function retrieves the inverse from the cache, otherwise
## the function solves for the inverse of the matrix and caches
## the value.

cacheSolve <- function(x, ...) {
        
        ## Retrieve the cached inverse of the matrix
        m <- x$getinvertmatrix()
        
        ## Determine if the inverse matrix has been cached.
        if(!is.null(m)) {
                
                message("getting cached data")
                
                ## the matrix has been cached so 
                ## returned the cached value
                return(m)
        }
        
        ## Otherwise get the value of the matrix
        data <- x$get()
        
        ## calculate the inverse of the matrix
        m <- solve(data, ...)
        
        ## set the cached value of the inverse of the matrix
        x$setinvertmatrix(m)
        
        ## return the inverted value of the matrix
        m
}
