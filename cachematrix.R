
# define the argument with default mode of "matrix"
## initialize inv as NULL which will hold value of matrix inverse 
## define the set function to assign new 
## value of matrix in parent environment
## if there is a new matrix, reset inv to NULL
## define the GET function which returns value of the matrix argument

makeCacheMatrix <- function(x = matrix()) {}
  inv <- NULL                             
  set <- function(y) {                    
     x <<- y                            
    inv <<- NULL                        
  }
  get <- function() x                    
  
  setinverse <- function(inverse) inv <<- inverse  
  ## assigns value of inv in the parent environment
  getinverse <- function() inv  
  ## gets value of inv 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  ## need this for referring 
  
  
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse() 
    if(!is.null(inv)) {
      message("getting cacheddata")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }
  