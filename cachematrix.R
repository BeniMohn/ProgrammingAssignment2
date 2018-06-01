## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # Setting inverse to null on initiation
        inverse <- NULL
        
        # Defining set function to assign new matrix and deleting invers
        set <- function(y){
                x <<- y
                inverse <<- NULL 
        }
        
        # Functin to return current 'x'
        get <- function() x
        
        # Function to set value for inverse
        setinverse <- function(inv) inverse <<- inv
        
        # Function to get current inverse
        getinverse <- function() inverse
        
        # Returning list of functions
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Getting current inverse
        inverse <- x$getinverse()
        
        # If inverse is not null return the inverse
        if (!is.null(inverse)) {
                return(inverse)
        }
        
        # Getting current matrix from 'cache'
        matrix <- x$get()
        
        # Solving for inverse
        inverse <- solve(matrix)
        
        # Setting value for the inverse
        x$setinverse(inverse)
        
        # Returning inverse
        inverse
}
