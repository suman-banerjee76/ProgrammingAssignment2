## Put comments here that give an overall description of what your
## functions do
#1.  `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.
#2.  `cacheSolve`: This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

##This function creates a special "matrix" object that can cache its inverse.
#The first function, `makeCacheMatrix` returns a List containing a function to
#1.  set the value of the Matrix
#2.  get the value of the Matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv_mat) inv <<- inv_mat
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


#The function "cacheSolve" calculates the inverse of the  "Matrix"
#created with the above function. 
#It first checks to see if the inverse has already been calculated. 
#If so, it `get`s the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data using solve function 
#and sets the value of the inverse in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_val <- x$getinv()
   #print(paste("value of inverse is",inv_val))
  if(!is.null(inv_val)) {
    message("getting cached data")
    return(inv_val)
  }
  data <- x$get()
  #print(data)
  inv_val <- solve(data, ...)
  #print(paste("after solve value is",inv_val))
  x$setinv(inv_val)
  inv_val
}

