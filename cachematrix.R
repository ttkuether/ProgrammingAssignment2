
## This function creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
    x <<- y
    m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m<<-solve
    getmatrix <- function() m
    list(set=set,get=get,
          setmatrix=setmatrix,
          getmatrix=getmatrix)
}


## This function computes the inverse from the makecachematrix function.  
##If the inverse has already been calculated it should be retrieved from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message('Getting cached data')
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
