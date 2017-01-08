## Put comments here that give an overall description of what your
## the two functions combined will calculate and cache the inverse of a matrix 
## to save the computation time if the inverse has been in the cache area


## Write a short comment describing this function
## this first function creates matrix as input and returns a list including four elements related to matrix and inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    
    
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


## Write a short comment describing this function
## the second function is ti check if the inverse of matrix has been saved in the cached area, if there, return cached data, 
## else calcualte the inverse of the new matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
