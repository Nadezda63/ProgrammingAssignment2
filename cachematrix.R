## This functions create a matrix object which caches its inverse
## x is an ordinary matrix
## you can get the value 
## change the value
## and get inverse

## This function creates a special "matrix" object that can cache its inverse
## contains a list of four functions to set, to get the value of a matrix
## and to set and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y    #set the value
                m <<- NULL #clear the cache
        }
        
        get <- function() x #define function to get the value of a matrix
        setinverse <- function(solve) m <<- solve #calculates the inverse of the matrix and caches the result
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) # return a list
}


## this function computes the inverse of the special matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated
## then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()     #this fetches the cached value for the inverse
        if(!is.null(m)) {       #If the cache was not empty, then
                message("getting cached data")
                return(m)
        }# the cache was empty
        data <- x$get() #get value of a matrix
        m <- solve(data, ...) #calculate inverse
        x$setinverse(m)
        m         #return the inverse
        
}

## THE FOLLOWING ARE A LIST OF COMMANDS TO TEST THESE FUNCTIONS

#>setwd("")
#>source("cacheMatrix.R")
#>a <- makeCacheMatrix(matrix(rnorm(25),nrow = 5,ncol = 5))
#>a$get()
#>a$getinverse()
#>cacheSolve(a)
#>a$getinverse()  # this is only to show you that the mean has been stored and does not affect anything
#>cacheSolve(a)
