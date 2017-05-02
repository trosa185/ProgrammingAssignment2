## The following function is used to create a special object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
   inv = NULL
   set = function(y) {
      x <<- y ## '<<-" is used to assign a value to an object in an environment different from the current enviornment
      inv <<- NULL
   }
   get = function() x
   setinverse = function(inverse) inv <<- inverse
   getinverse = function() inv
   list(
      set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
      
}


## The following function is used to cache the inverse of the matrix that has been stored in the previous function (makeCacheMatrix)

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   
   inv = x$getinverse()
   
   ## if the matrix's inverse is already in list then get from cache
      if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
      }
   
   ## else, calculate the inverse
      newmatrix = x$get()
      inv = solve(newmatrix, ...)
      
   ## sets value of new inverse matrix in cache list
      x$setinverse(inv)
      
      return(inv)
}


