## Use makeCacheMatrix to set a matrix and use CacheSolve to
## retrieve its inverse. Previously calculated inverse
## will not be recalculated, rather, a "cached" inverse will be
## printed.

## Set, store, and retrive cached inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
      ## set as empty
  set <- function(y){
    x <<- y
    m <<- NULL
  }
      ## set function replaces x with defined y and 
      ## ensures m is empty; y must be matrix
  get <- function() x
      ## get prints contents of x
  setsolve <- function(solve) m <<- solve
      ## setsolve stores inverse of the matrix as 
      ## calculated in cacheSolve
  getsolve <- function() m
      ## getsolve prints the inverse of the matrix as 
      ## calculated in cacheSolve
  list(set=set, get=get, setsolve=setsolve, 
       getsolve=getsolve)
      ## generates the list of 4 functions to call
}


## Calculate and print an inverse matrix and store in 
## setsolve above, or print stored inverse saved in cache if 
## previously calculated

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
      ## set m as results of getsolve above
  if(!is.null(m)){
        ## if m is not null, cacheSolve has been called previously
    message("getting cached data")
    return (m)
        ## print message and the inverse (previously calculated)
  }
  data <- x$get()
      ## if m is not null, set "data" to the matrix stored above
  m <- solve(data)
      ## solve the inverse of the "data" and set as m
  x$setsolve(m)
      ## perform setsolve above to set to calculated m (for
      ## recalling cached inverse later)
  m
      ## print m, the inverse
}
