# pair of functions to cache the inverse of matrix
# it is assumed matrix is square and invertible 

## makeCacheMatrix

# The makeCacheMatrix function creates a special "matrix", 
# which is really a list containing a function to
# 1) set the value of the matrix()
# 2) get the value of the matrix
# 3) set the value of the inverse
# 4) get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {

    #set inverse matrix m to NULL
    inv.m <- NULL
    
    #set function - set matrix values 
    #             - and reset inverse matrix to null
    set <- function(y) {
        x <<- y
        inv.m <<- NULL
    }
    
    # get function - return matrix values
    get <- function() x
    
    # setinverse function  - sets cached inverse matrix
    setinverse <- function(inverse) inv.m <<- inverse
    
    # getinverse function  - gets cached inverse matrix
    getinverse <- function() inv.m
    
    # store 4 functions in a list
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve

# The cacheSolve function calculates the inverse of the special "matrix" created with the makeCacheMatrix function.
# It first checks to see if the inverse has already been calculated. 
# If inverse has been calculated, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data 
# and sets the value of the inverse in the cache via the setcache function.



cachesolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'm'
    
    #checked to see if inverse already cached
    inv.m <- x$getinverse()
    
    
    if(!is.null(inv.m)) {
        message("getting cached data")
        return(inv.m)
    }
    
    # added test for invertible 
    # not required as assumed  matrix invertible but assists testing 
    invertible <- function(m) "matrix" %in% class(try(solve(m),silent=TRUE))
    
    # get matrix values
    m <- x$get()
    
    #test if invertible 
    if (!invertible(m)) {
        message("Matrix is not invertible")
        return()
    }
    m.inv <- solve(m, ...)
    x$setinverse(m.inv)
    m.inv
}


# test functions work

# A <- makeCacheMatrix()
# A$set(matrix(c(23,7,3,4,13,17,22,26,31),3,3))
# A$get()
# cachesolve(A)
# A$getinverse()
# cachesolve(A)
# A$set(matrix(c(53,79,3,4,13,17,22,26,31),3,3))
# A$get()
# cachesolve(A)
# A$getinverse()
# A$set(matrix(1:9,3,3))
# A$get()
# cachesolve(A)
# A$getinverse()

