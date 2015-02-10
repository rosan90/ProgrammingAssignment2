## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  get <- function() x
  setmx <- function(solve) mx <<- solve
  getmx <- function() mx
  list(set = set, get = get,
       setmx = setmx,
       getmx = getmx) 
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  xc <- makeCacheMatrix(x)
  
  mx <- xc$getmx()
  
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  data <- xc$get()
  mx <- solve(data, ...)
  xc$setmx(mx)
  mx
}
