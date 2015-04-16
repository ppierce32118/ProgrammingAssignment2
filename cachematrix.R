## matrix value and inverse matrix value 
## - add a matrix with makeCacheMatrix 
## - get the inverse matrix with cacheSolve
## - the inverse is only calculated the first time


## create a function to hold matrix value and inverse of matrix value
## - sets matrix value 
## - gets matrix value
## - sets inverse of matrix value
## - gets inverse of matrix value

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        ## set the matrix value and initialize the inverse value
        setmatrix <- function(newmatrix) {
                x <<- newmatrix 
                inversematrix <<- NULL
        }
        ## get the matrix value
        getmatrix <- function() x
        ## set the inverse matrix value
        setinversematrix <- function(newinversematrix) {
                inversematrix <<- newinversematrix
        }
        ## get the inverse matrix value
        getinversematrix <- function() inversematrix
        ## Return a list with functions for the matrix
        list(setmatrix=setmatrix, 
             getmatrix=getmatrix, 
             setinversematrix=setinversematrix, 
             getinversematrix=getinversematrix)
}


## gets inverse matrix (stored in list created by function makeCacheMatrix)
## from cache if not null or solve the matrix for inverse (and store)

cacheSolve <- function(x, ...) {
        ## gets inverse matrix value
        inversematrix <- x$getinversematrix()
        if (!is.null(inversematrix)) {
                ## Return the cached inverse matrix if not null
                return(inversematrix)
        }
        ## inverse matrix is null so solve the matrix for inverse value
        inversematrix <- solve(x$getmatrix(), ...)
        x$setinversematrix(inversematrix)
        ## Return a matrix that is the inverse of 'x'
        inversematrix
}
