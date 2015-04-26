## makeChacheMatrix function creates a special matrix 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)

}




## cacheSolve functionb returns a matrix that is the inverse of 'x'
## If its already been computed once, use cache
## to return value. Print out message when chached
## data is used
cacheSolve <- function(x, ...) {
    
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("return cached data...")
        return(m)
    }
    
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m

}
