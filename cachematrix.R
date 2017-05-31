## This function creates a special "matrix" object that can cache its inverse.
## 4 Functions used to Set and Get Matrix and to Set and Get Inverse
## Functions are stored in a list

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {  ## Set Matrix
    x <<- y
    inv <<- NULL
    }
    get <- function() x  ## Get Matrix
    setI <- function(inverse) inv <<- inverse  ## Set Inverse
    getI <- function() inv ## Get Inverse
    list(set=set, get=get, setI=setI, getI=getI)  ## List of Set/Get Functions
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv <- x$getI()
    if(!is.null(inv)) {  ## If Inverse Already Cached
    return(inv)  ## Print Inverse
    }
    myMat <- x$get()  ## Else Calculate Inverse
    inv <- solve(myMat) ## Solve Inverse of Matrix
    x$setI(inv)
    return(inv)  ## Return Inverse
}
