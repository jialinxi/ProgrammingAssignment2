## Computing the inverse of a matrix might be a time-consuming process. This function
## allows us to cache the inverse matrix instead of computing it repeatedly.

## This function creates a special "matrix" object that can cache its inverse. It creates a list containing functions to
## 1, Set value of the matrix
## 2, Get value of the matrix
## 3, Set inverse of the matrix
## 4, Get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set =set, get = get, setInverse = setInverse, 
        getInverse = getInverse)
}


## The following function calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the mean from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
         mat <- x$get()
        inv <- solve(mat)
        x$setInverse(inv)
        inv
}
