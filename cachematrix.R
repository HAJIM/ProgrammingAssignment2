## 
## 

## This function makeCacheMatrix creates a special Matrix

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve calculates the mean of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if(!is.null(m)) {             ##Checks to see if the inverse has already been calculated
    message("getting cached inverse")
    return(m)                   ##Get the inverse from the cache and exit
  }
  matrix <- x$get()     
  m <- solve(matrix, ...)       ##Calculate the inverse of the matrix 
  x$setinverse(m)               ##Set the inverse in the cache
  m
}
