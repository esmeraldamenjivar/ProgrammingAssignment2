##Matrix inversion
##Catches the inverse of a matrix 

## creates a special matrix that can catch its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse)i <<- inverse
        getinverse <- function()i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse

}


## Finds inverse of the special matrix created

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if( !is.null(i)) {
                message("retrieving catched data")
                return(i) 
        }
        mtinverse <- x$get()
        i <- solve(mtinverse, ...)
        x$setinverse(i)
        i
}
