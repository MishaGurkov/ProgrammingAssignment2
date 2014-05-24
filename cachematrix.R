## Set of function the stores the inverse of matrix in cashe and extract it from cashe

## makeCacheMatrix takes a matrix and returns a list of 4 functions :
# 1. set is a function that sets the matrix that need's to be inverted
# 2. get is a function that returns the matrix that need's to be inverted
# 3. setcache is a function that writes it's argument to cashe
# 2. getcache is a function that extracts it's argument from cashe

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setcache <- function(matrix) cache <<- matrix
        getcache <- function() cache
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}


## cacheSolve takes a matrix 
# checks for the matrix inverse in cache and returns the cashed matrix inverse
# in case cashe is empty cacheSolve calculates and returns the inverse

cacheSolve <- function(x, ...) {
        cache <- x$getcache()
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data, ...)
        x$setcache(cache)
        cache
}
