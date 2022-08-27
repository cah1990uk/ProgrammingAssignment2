## Put comments here that give an overall description of what your
## functions do
# This function will create a special matrix and cache its inverse

## Write a short comment describing this function
#This function creates the special matrix
# The assignment operator "<<-" ensures values persist after the function call
# and are available later via the parent environment.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setcache <- function(mycache) m <<- mycache
      getcache <- function() m
      list(set = set, get = get, setcache = setcache, getcache = getcache)
}


## Write a short comment describing this function
#This function creates the inverse matrix and cache it if it doesn't already exist

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      #      mymatrix <- solve(x)
      #      mymatrix
      m <- x$getcache()
      if(!is.null(m)) {
            message("getting cached inverse")
            return(m)
      }
      myinverse <- x$get()
      m <- solve(myinverse)
      x$setcache(m)
      m
}
