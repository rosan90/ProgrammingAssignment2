## makeCacheMatrix - defines a list of functions
## cacheSolve - calls makeCacheMatrix to see if the inverse function has already been saved and stored in the global variable mx
##   - if it has then the global variable mx is retrieved
##   - if it hasnt then the inverse function is calculated and stored as the global variable mx

## defines a list of functions

makeCacheMatrix <- function(x = matrix()) {

  ## define the set function
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  
  ## define the get function
  get <- function() x
  
  ## define the setmx function. The output of the funtion(solve) is store in the global variable mx
  setmx <- function(solve) mx <<- solve
  
  ## define the getmx function. 
  getmx <- function() mx
  
  ## create the list of functions. Calling format is $name
  list(set = set, get = get,
       setmx = setmx,
       getmx = getmx) 
  
}


## solve the inverse funtion of an input and store in a global variable for later reuse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## call the makeCacheMatrix and pass the value to be inverted
  xc <- makeCacheMatrix(x)
  
  ## us the makeCacheMatrix function getmx to retrieve mx from the global variable pool
  mx <- xc$getmx()
  
  ## see if the variable mx had already been calculated
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  
  ## the global variable mx had not been calculated - calculate now and store 
  data <- xc$get()
  mx <- solve(data, ...)
  xc$setmx(mx)
  mx
}
