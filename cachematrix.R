## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                   # makeCacheMatrix is a function that stores functions
  set <- function(y){         # set is the first function stored that changes the vector stored in the main function
    x <<- y                   # '<<-' assign a value (y) to an object in an environment that is
    m <<- NULL                #  different from the current environment
  }
  get <- function() x         # get is the second function that returns the vector x stored in the main function.
  
  setinverse <- function(solve) m <<- solve # stores the value of the input in a variable m into the main function makeCacheMatrix
  getinverse <- function() m                # returns m in this case the inverse of the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}                             # storing the four functions into the makeCacheMatrix function


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {           # 'if' here checks wether the m has been calculated or not
    message("getting cached data")
    return(m)                 # if yes, then simply returns m without recalculating. 
  }
  data <- x$get()             # if no, run the function (in this case "solve") on m and returns it. 
  m <- solve(data, ...)    
  x$setinverse(m)
  m                            
}
