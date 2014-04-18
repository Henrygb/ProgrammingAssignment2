## These two R functions can cache potentially time-consuming 
## computations: in this particular case matrix inversion.

## The first function makeCacheMatrix() 
## sets up the cache (but does not calculate the inverse) 
## producing a list of functions: set(), get(), setinverse(), getinverse()

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
               }
        get <- function() mat
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

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
        inv <- mat$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
               }
        data <- mat$get()
        inv <- solve(data, ...)
        mat$setinverse(inv)
        inv 
       }

## Example to illustrate use of functions (remove # at beginning of lines)
# mymat <- rbind(c(0,2),c(1,3))    ## inverse is rbind(c(-1.5,1),c(0.5,3))
# invmat <- makeCacheMatrix(mymat) ## set up on mymat but not calculate yet
# cacheSolve(invmat)               ## nothing cached so calculate and cache 
# cacheSolve(invmat)               ## this time inverse cached so use cache

## Code based on Roger D. Peng's cachevectormean example
