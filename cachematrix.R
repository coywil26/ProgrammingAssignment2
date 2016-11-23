## My function should calculate the inverse of 
## a matrix and then cache this

## This makes the creates a special matrix which can
## be used to cache the inverse by
## 1. set value of  matrix
## 2. get value of matrix
## 3. set  value of  inverse  matrix
## 4. get value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function() x 
    setinverse<-function(inverse) i<<-inverse
    getinverse<- function() i 
    list(set=set,get=get,setinverse=setinverse,
    getinverse=getinverse)
}


## This caculates the inverse of the matrix
## created above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    d<-x$get()
    ##this is where inverse is actually calculated
    ## first checks to see if inverse inverse has been 
    ##calculated. If so it gets the inverse from the cache
    ## and skips computation. Otherwise, it caculates the inverse matrix
    ##and sets the inverse value using the setinverse function.
    i<-solve(d,...)
    x$setinverse(i)
    i
}
