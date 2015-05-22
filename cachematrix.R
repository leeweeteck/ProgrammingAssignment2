
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the cache to be NULL
  m.inv <- NULL
  
  # this function sets the value of "x" & "m.inv" in the parent environment
  set <- function(y) {
    x <<- y
    m.inv <<- NULL
  }
  
  # this function returns the value of the original matrix, which is labelled 
  # as "x" in the makeCacheMatrix function
  get <- function() x
  
  # this function updates the value of "m.inv" in the parent environment
  set.m.inv <- function(matrix.inv) m.inv <<- matrix.inv
  
  # this function returns the value of the cache, i.e. "m.inv"
  get.m.inv <- function() m.inv
  
  # this creates a list of the four functions
  list(set = set, get = get,
       set.m.inv = set.m.inv,
       get.m.inv = get.m.inv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated and 
## the matrix has not changed, then this function will retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
  
  # load the matrix inverse from the cache
  m <- x$get.m.inv()
  
  # returns the value from the cache if the previous load command retrieves
  # a non-null value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # get the original matrix
  mat <- x$get()
  
  # compute the inverse of the matrix
  m <- solve(mat, ...)
  
  # store the value of the matrix inverse in the cache
  x$set.m.inv(m)
  
  # print the value of the matrix inverse
  m
}



