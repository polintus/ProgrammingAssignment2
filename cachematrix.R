## The function makeCacheMatrix creates a special matrix, 
## which is a list containing functions: 1.to set the values and dimensions of the matrix,
## 2. get the values and dims of the matrix, 3. set the value of the inverse of the matrix
## and 4. get the value of the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a matrix which will cache the inverse of a matrix for future retrieval.
  
    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The function cacheSolve takes a matrix x as an argument and returns the inverse of 'x'. 
## If there is a cached version of the inverse of x, it will retrieve it instead of 
## solving the matrix again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
