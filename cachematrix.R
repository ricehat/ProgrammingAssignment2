
## The first function, makeCacheMatrix returns a list that contains
## functions that allow you to
##    1. set a matrix
##    2. get the matrix
##    3. set the inverse of the matrix
##    4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
    inv_matrix <- NULL
  
    set <- function(y){
        x <<- y
        inv_matrix <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv_matrix <<- inverse
    getinv <- function () inv_matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}

## The cacheSolve function calculates the inverse matrix using the solve()
## function. However, it first checks if the inverse has already been
## calculated. If it finds it in the cache, it will return it. If not, it
## will compute the inverse of the matrix and save it to the cache.

cacheSolve <- function(x, ...) {
    
    inv_matrix <- x$getinv()
    
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    
    data <- x$get()
    inv_matrix <- solve(data, ...)
    x$getinv(inv_matrix)
    inv_matrix
    
}
