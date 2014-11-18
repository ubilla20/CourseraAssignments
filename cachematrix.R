## The two functions work together. The first sets up the Cache and the second
## function uses the list from makeCacheMatrix to give the proper matrix.
## The solve function can be resource intensive. By setting up the cache 
## it makes so that the functions don't have to recompute the whole matrix

## This is the makeCacheMatrix. It creates a list that has four elements.
## The later function will use some of the functions in this function.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
## The cacheSolve uses the list and functions from makeCacheMatrix to produce
## the appropriate inverse matrix. 

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m

        ## Return a matrix that is the inverse of 'x'
}
