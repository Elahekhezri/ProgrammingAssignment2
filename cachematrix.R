
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(value = matrix()) {
  inverse <- NULL                           
  set <- function(n) {                    
    value <<- n                             
    inverse <<- NULL                      
  }
  get <- function() value                   
  
  setinv <- function(inv) inverse <<- inv  
  getinv <- function() inverse                     
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



cacheSolve <- function(value, ...) {
  
  inv <- value$getinv()
  if(!is.null(inverse)) { return(inverse) }
  result <- value$get()
  inverse <- solve(result, ...)
  value$setinv(inverse)
  inverse
}