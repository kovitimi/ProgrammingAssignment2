## These functions cache the inverse of a matrix and return it from the cache

## The makeCacheMatrix function creates a special matrix that is a list
## containing a function to set and get the value of the matrix, 
## set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) { #the input is the x matrix
        i <- NULL #resetting the inverse to null every time the makeCacheMatrix is called
        set <- function(y) { #caches the values of the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x #returns the original value of the matrix
        setinverse <- function(solve) i <<- solve #the cacheSolve will use this the first time its called
        getinverse <- function() i #returns the cahced value of the matrix on susequent calls
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) #the list of internal functions (methods) so the calling function knows how to reach these methods
}



## The cacheSolve function calculates the inverse of the special matrix created with the above function

cacheSolve <- function(x, ...) { #the input is x created by makeCachematrix
        i <- x$getinverse() #accesses the object x and gets the value of the inverse
        if(!is.null(i)) { 
                message("getting cached data")
                return(i)#if the inverse is already cached, gets it and returnes it 
        }
        data <- x$get() #if not than it gets the value of the matrix 
        i <- solve(data, ...) #and computes the inverse 
        x$setinverse(i) #and stores in the cache
        i #returns the inverse
}


## Cheers to Bill Hilton for the insight and Pavel Kirjanas for sharing it