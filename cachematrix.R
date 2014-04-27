## Title: cachematrix.R
## The two functions below allow for computing the inverse of matrices using a cache
## Also included is some example code demonstrating their use

## MakeCacheMatrix returns a list which stores a matrix along with several functions that allow for cacheing the matrix's inverse 

## The main input to the function is x, the matrix being stored
## i, the variable used to store the matrix's inverse, starts as NULL
## There are four functions included:
##      "set" allows the user to reset the matrix stored in x after the CacheMatrix has been initialized
##      "get" returns the saved matrix x
##      "setinverse" saves a passed parameter, the computed inverse, to i
##      "getinverse" returns the inverse i

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## CacheSolve is a function that returns the inverse of a matrix created by makeCacheMatrix

## If there exists a cached inverse for that matrix, the function returns the inverse matrix
## If not, it finds the inverse, caches it, and then returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}

## Example use of makeCacheMatrix and cacheSolve
cachedM <- makeCacheMatrix(matrix(1:4,nrow=2))  #Create a CacheMatrix
cacheSolve(cachedM)                             #Compute the inverse; it hasn't been computed before, so it's computed and cached
cacheSolve(cachedM)                             #This time, the inverse has been cached, so it is looked up instead of computed