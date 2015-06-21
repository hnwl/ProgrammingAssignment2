## These two functions allow matrices to be created that are capable of storing
## values in cache. First the special matrix is created by passing the first
## function a regular matrix, that is assumed to be invertable. To compute the
## inverse of the matrix the function cacheSolve needs to be run with a cache matrix
## object as a parameter. When this is done the cache matrix function getinv will
## return the previously computed value computed by cacheSolve. Running cacheSolve
## again will return the cached value.

## this function constructs matrices that can store their inverse in cache 

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   #set the actual values of the matrix
   set <- function(y){
      x <<- y
      inv <<- NULL
   }
   #display elements of the matrix
   get <- function() x
   #store calculated inverse 
   setinv <- function(i) inv <<- i
   #fetch cached value, will return null if no value is found
   getinv <- function() inv
   #list the different functions that the cached matrix object supports
   list(set = set, get = get, setinv = setinv, getinv = getinv)
   

}


## helper function that checks whether a solution exists in cache and computes one
## in case there is none


cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
   inv <- x$getinv()
   # check whether a previous cached solution exists
   if(!is.null(inv)){
      message("getting cached data")
      return(inv)
   }
   # the following section is run only if no previously cached value is found
   data <- x$get()
   inv <- solve(data, ...)
   x$setinv(inv) #storing the solution in cache
   inv
   
}
