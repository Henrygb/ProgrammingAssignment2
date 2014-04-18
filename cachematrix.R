## These two R functions can cache potentially time-consuming 
## computations: in this particular case matrix inversion.

## The first function makeCacheMatrix() 
## sets up the cache (but does not calculate the inverse) 
## producing a list of functions: set(), get(), setinverse(), getinverse()

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(y) {
                m <<- y
                inv <<- NULL
               }
        get <- function() m
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
       }


## The second function cacheSolve() 
## takes the matrix, checks if there is an inverse cached,
## and if so then returns it, but if not calculates it and caches it
## all assuming that the matrix is invertible

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        inv <- m$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
               }
        data <- m$get()
        inv <- solve(data, ...)
        m$setinverse(inv)
        inv 
       }

## Example to illustrate use of functions (remove # at beginning of lines)
#mymat <- rbind(c(0,2),c(1,3))  ## inverse is rbind(c(-1.5,1),c(0.5,3))
#cmat <- makeCacheMatrix(mymat) ## set up on mymat but do not calculate yet
#cacheSolve(cmat)               ## nothing cached so calculate and cache 
#cacheSolve(cmat)               ## this time inverse cached so use cache

## Code based on Roger D. Peng's cachevectormean example
