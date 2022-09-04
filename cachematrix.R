# set up the main makeCacheMatrix function
makeCacheMatrix <- function (x = matrix()){
  m <- solve(x) # calculate the inverse data, if set m <- NULL, then cacheinverse will calculate the inverse data when it detects m <- NULL
  
  #set the value of the vector
  set <- function(y){ 
    x <<- y
    m <<- m
  }
  
  #get the value of the vector 
  get <- function() x 
  
  #set the inversed value of the vector
  setinverse <- function(inverse) m <<- inverse
  
  #get the inversed value of the vector
  getinverse <- function() m
  
  # list the results
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


# Set up the function to inverse the matrix, it first check if the matrix has already been inversed. If so, get the inversed matrix from the cache, and skips the compuation. Otherwise, it computes.
cacheinverse <- function (x, ...) 
{
  
  # check if m is NULL, if NULL, message "getting cached data"
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # Invert the value got from the matrix 
  Matrix_invert <- x$get()
  m <- solve(Matrix_invert,...)
  
  # Set the value from the inversed matrix
  x$setinverse(m)
  m
}


