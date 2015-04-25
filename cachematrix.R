## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The functions below help to cache the inverse of a matrix. 



## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
	  ## function changing the matrix x stored 
	  ## in the main function (makeCacheMatrix)
        set <- function(y) { 	
                x <<- y		## substituting the matrix x with y (the input) 
					## in the main function
                i <<- NULL 	## restoring to null the value of the inverse
				   	## matrix i
        }
	  ## function returning the matrix x stored in the main function
        get <- function() x 	
	  ## function storing the value of the input in a variable i 
	  ## into the main function 
        setinverse <- function(solve) i <<- solve 
	  ## function returning the value of the input in a variable i
        getinverse <- function() i 
	  ## storing the four functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve will retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        i <- x$getinverse()
	  ## function returning a message and the value i  
	  ## if i already exists in memory and is not null
	  if(!is.null(i)) {  
                message("getting cached data")
                return(i)
        }
	  ## calculating the inverse matrix and storing it in the object 
	  ## generated assigned with makeCasheMatrix    
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}