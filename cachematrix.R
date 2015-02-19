#The first function, makeCacheMatrix creates a special "matrix", 
#which is really a list containing a function to
#set/get the value of the cacheMatrix

#set the value of the cached matrix
#get the value of the cached matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcmatrix <- function(cmatrix) m <<- cmatrix
  getcmatrix <- function() m
  #set the value of the cached matrix
  #get the value of the cached matrix
  list(set = set, get = get,
       setcmatrix = setcmatrix,
       getcmatrix = getcmatrix)
}

#the cacheSolve() function calculates the inverse of the cached matrix
#created with the above function. However, it first checks to see if the inverse
#has already been calculated. If so, it gets the inversefrom the cache and skips 
#the computation. Otherwise, it calculates the inverse of the data and sets the 
#value of the inverse in the cache via the setinversematrix function.


cacheSolve <- function(x, ...) {
  m <- x$getinversematrix()
  #checks to see if matrix is previously cached
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if previous value was null, then inverse of cached matrix will be calculated
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}
