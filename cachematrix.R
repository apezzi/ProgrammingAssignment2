## This functions are used to create a special matrix than can cache it inverse to avoid perfoming
## redundant computations.

## This function creates a special matrix which is really a list containing a function to:
# change the matrix to another matrix (set())
# get the value of the matrix (get())
# get and set the INVERSE matrix of the special matrix (getinv(),setinv())

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
  }
  get<- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(inv,get=get,set=set,setinv=setinv,getinv=getinv)

}


## This function returns the value of the inverse of a special matrix created by the above function and
## caches the result.
## When the matrix hasn´t changed and the inverse has been already computed 
## it returns the inverse without using solve() function again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
