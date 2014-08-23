## Initiazliza the "special matrix" and its functions: get, setinv, getinv.

makeCacheMatrix <- function(x = matrix()) {
    cache_inv<-NULL #initialization: we cannot test the correct caching without it
  
    get <- function() x #get the original matrix
    
    setinv <- function(inv=NULL) cache_inv <<- inv #caching the inverse
    
    getinv <- function() cache_inv #retrieve the cahed matrix (NULL if no result is cached)
    
    list(get = get,
         setinv = setinv,
         getinv = getinv)
}


## Retrieve from memory the cached inverse matrix; if it doesn't exist calculates it and caches it 

cacheSolve <- function(x, ...) {    
    inv <- x$getinv() #getting the inverse matrix
    
    #testing if the inverse exists (is already cached)
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    
    inv <- solve(x$get(), ...) #calculating the inverse
    x$setinv(inv)              #caching the inverse
    
    ## Return a matrix that is the inverse of 'x'
    inv
}