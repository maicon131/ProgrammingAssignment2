## These functions were created to compute the inverse of a given matrix and
##cache the result eliminating the need to cumpute it over and over again

## The makeCacheMatrix function creates a special "matrix" object that can cache
##its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}

## The cacheSolve function computes the inverse of the special "matrix" created
##in the function above. This function only calculates if there's not a previous
##result, if there is a previous calculation (and the matrix has not changed), 
##then the cacheSolve should retrieve the inverse of the cached matrix

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
  
}
