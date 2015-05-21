##R Programming - Coursera
##Maria Muller
##5/21/15
## These functions cache a matrix and invert it
##cacheSolve function assumes the matrix is already invertible


makeCacheMatrix <- function(x = matrix()) {
        
        #initialize m to nothing
        m <- NULL
        
        #create a set function that will reference input (y) and 
        #ensures m is null (outside the scope of current function)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #create a function that returns the matrix, x
        get <- function() x
        
        
        #create a function that  inverses m
        #input matrix
        setinverse <- function(solve) m <<- solve
        
        #create a function that returns the inverted matrix, m
        getinverse <- function() m
        
        #returns a list of functions available within makeCacheMatrix()
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x=matrix(), ...) {
        
        #set a variable to the inverse of the submitted matrix (x) 
        #by calling the getinverse function above
        m <- x$getinverse()
        
        #if there's data in the returned matrix, just re-use m 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #set myData to the output of the cached matrix via the get function
        myData <- x$get()
        
        #use the  solve function to invert the matrix of m
        #which should be the cached data from the get function if it existed
        m <- solve(myData, ...)
        
        #invert the matrix and put into cache
        x$setinverse(m)
        
        #return the cached matrix that is inverted
        m

}
