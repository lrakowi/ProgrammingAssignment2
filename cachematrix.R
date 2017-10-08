## Course 2 > Week 3 > Programming Assignment 2 - Cacheing the Inverse of a Matrix 


## This assignment is to write a pair of functions that cache the inverse of a matrix.

  ## makeCacheMatrix: 
  ## This function creates a special "matrix" object that can cache its inverse.

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

  ## cacheSolve: 
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  ## Additionally, If the inverse has already been calculated (and the matrix has not changed), 
  ## then the cachesolve should retrieve the inverse from the cache. 

    cacheSolve <- function(x, ...) {
       inv = x$getinv()
       
      if (!is.null(inv)){
        message("getting cached data")
        return(inv)
      }
       
      mat.data = x$get()
      inv = solve(mat.data, ...)
      x$setinv(inv)
      return(inv)
    }
    
  ## Testing
    test <- matrix(c(2,4,6,8),2,2)
    cache_mat <- makeCacheMatrix(test)
    cacheSolve(cache_mat)
    
   ## [,1]  [,2]
   ## [1,] -1.0  0.75
   ## [2,]  0.5 -0.25
    
## End Assignment