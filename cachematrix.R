# programming assignment 2 for week 3

makeCacheMatrix <- function(x = matrix) {
  
# instantiate the inverseMatrix and initially set the value to NULL\
# then makeCacheMatrix can be used within calling cacheSolve to the getter, setter and inverse functions
  
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(iv) inverseMatrix <<- iv
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)   
    
}

cacheSolve <- function(x, ...) {
  
 # check whether matrix there is an inverseMatrix already calculated 
  
  # if so get the inverse matrix from the makeCacheMatrix cached value
  
  # if not then calculate the inverse 
  
  inverseMatrix <- x$getInverse()
  
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  if (is.null(inverseMatrix)) {
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setInverse(inverseMatrix)
  }

  # Give back the inverse Matrix 
  inverseMatrix
}