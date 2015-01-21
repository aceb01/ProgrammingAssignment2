## In two main steps (makeCacheMatrix and cacheSolve) a matrix is first created and then inversed
## the inversion only occurrs if there is not already an inversed matrix saved in the cache.

## Step 1: creating a function containing further functions to set or get the vector (here: matrix)...

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y){
    x<<-y
    m<<-NULL    
  }
  get<-function()x
  
  ##...and functions to set or get the inverse of that matrix in/from the cache
  setmatrix <-function(solve)
    m<<-solve
  getmatrix<-function()m
  
  ## listing the above functions
  list(set=set, get=get, setmatrix=setmatrix,getmatrix=getmatrix)
}

## Step 2: computing the inverse of matrix created by makeCacheMatrix
cacheSolve <- function(x=matrix(), ...) {
  
  ## checking if matrix already exists
  m<-x$getmatrix()
  
    ## instructing that if the matrix already exists, it's to be taken from the cache
  if(!is.null(m)){
    message ("getting cached data")
    return(m)
  }
    ## getting the matrix and calculate its inverse
  matrix<-x$get()
  m<-solve(matrix,...)
  
  ## setting inserve of the matrix in the cache
  x$setmatrix(m)
  m
}
