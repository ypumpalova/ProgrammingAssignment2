## makeCacheMatrix is a function that stores a matrix and its inverse
## cacheSolve computes the inverse of the matrix you input, unless this has already been done, in which case it returns the previously solved output 


makeCacheMatrix <- function(x = matrix()) {
invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invrs<<-inverse
    getinverse <- function() invrs
    list (set=set, get=get,
          setinverse=setinverse,
          getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
       invrs<- x$getinverse()
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    data<- x$get()
    invrs<-solve(data,...) #This generic function solves the equation a %*% x = b for x, where b
    x$setinverse(invrs) #can be either a vector or a matrix. If b is missing, it is taken to be an identity
    invrs #matrix and solve will return the inverse of a
}
