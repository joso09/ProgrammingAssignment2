# This function stores a matrix and "remeber" the last matrix that was inversed 

## Store one matrix and inverse a matrix
## Store the last matrix that was inversed last.

makeCacheMatrix <- function(x = matrix()) {
        invc <- NULL
        x <- NULL
        org <- NULL
        
        set <- function(data) {
                x <<- data
        }
        get <- function() x
        setinvc <- function(x = matrix()) {
                org <<- x
                invc <<- solve(x)
        }
        getinvc <- function() invc
        getorg <- function() org
        list(set = set, get = get, setinvc = setinvc, getinvc = getinvc, getorg = getorg)
}


cacheSolve <- function(x, ...) {
        
        ## If one matrix already has been inversed then check if it was the same 
        ## matrix that was inversed the last time if so return the cashed inversed matrix
        ## else inverse 'x'
        
        if(!is.null(x$getinvc()) & identical(x$getorg(), x$get()) ) {
                message("getting cached data")
                return(x$getinvc())
        }
        message("create new inverse")
        x$setinvc(  x$get()  )
        data <- x$getinvc()
        data
}