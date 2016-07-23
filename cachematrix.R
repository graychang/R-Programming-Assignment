## These functions can cache the value of inverse matrix to save the time when 
## we are doing complicate project.

## The first function creates a list of function that
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}


## The Second function will call the cache inverse if available or it will do the
## computation for the matrix in the first function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ... )
        x$setinverse(inv)
        inv
}
