## Function to set Inverse of a matrix to a variable and return that when called repeatedly 
## with same matrix values with out computing inverse of a matrix each time


## This function can set the variable with inverse of a Matrix passed
makeCacheMatrix <- function(x = matrix()) 
{
        ##variable to hold inverse matrix
        invM = NULL
        
        set = function(y)
        {
                x <<- y
                invM <<- NULL
        }
        
        get = function()
        {
                x
        }
        
        setinverse = function(inverse)
        {
                invM <<- inverse
        }
        
        getinverse = function()
        {
                invM
        }
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
} 


## Function to check if the inverse of them matrix is already in the variable else to compute.
cacheSolve <- function(x, ...) 
{ 
        ## Return a matrix that is the inverse of 'x' 
        inverse = x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        inverse = solve(x$get())
        x$setinverse(inverse)
        inverse
} 
