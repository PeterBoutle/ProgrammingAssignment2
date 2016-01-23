## These two functions will solve the inverse of x.  If this has already been calculated
## then the function will return a result from the cache.

## Make cache Matrix takes a matrix x as args and returns a list containing four functions.
## get and set return or set the value of x whilst setsolve and getsolve set or retrieve the 
## cached inverse of x.

makeCacheMatrix <- function(x = matrix()) {
  get <- function() x
  set <- function(y){
                      x <<- y
                      slv <<- NULL
  }
  setSolve <- function(sol) slv <<-sol
  getSolve <-function() slv
  list(get=get,set=set,setSolve = setSolve,getSolve=getSolve)
}


## Cache solve takes the list of functions from make cache matrix as the arg.
## if the cache contains the inverse of x already then it will return it from the cache
##if the cache is empty (x$getSolve() is null) then it will recaclulate it.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  slv <-x$getSolve()
  if (!is.null(slv))
  {
    return(slv)
  }
  data <-x$get()
  slv <- solve(data)
  x$setSolve(slv)
  slv
  }


