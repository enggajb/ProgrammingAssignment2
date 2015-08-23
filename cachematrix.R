##
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##      retrieve the inverse from the cache

## W# x is a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse

makeCacheMatrix <- function(x = matrix()) {

        inv = NULL
        set = function(y) {
 
          x <<- y
          inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
    }


## Foe CaschSolve function, x is output of makeCacheMatrix()
        ## which return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        
        # Check if the inverse has already been calculated
        #  if so, get it from the cache and stops/skips the computation.
        if (!is.null(inv)){
                 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sinverse matrix in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
## -- Part 3 - Test Result
## mytest function to check the results. if it is in cache, time should be less. 
## First time will create cache and second time use cache data to calculate
## mat is invertible matrix as input for this function
mytest = function(mat){
        
        mytemp = makeCacheMatrix(mat)
        
        start.time = Sys.time()
        ## Calling first time
        cacheSolve(mytemp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time = Sys.time()
        # Calling Second time which will return cache info
        cacheSolve(mytemp)
        dur = Sys.time() - start.time
        print(dur)
}


## -- Part 4 -- call the funtion with passing matrix
set.seed(20)
r = rnorm(1000000)
mymat = matrix(r, nrow=1000, ncol=1000)
mytest(mymat)

## --- Part 5 --  Results from above is given below

#Time difference of 1.289074 secs
#getting cached data
#Time difference of 0.0009999275 secs

