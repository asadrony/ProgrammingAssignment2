## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  mdat<-NULL
  set<-function(x){
    m<<-x
    mdat<<-NULL
  }
  ## Method to get and set the matrix
  get<-function() m
  setmatrix<-function(solve) mdat<<- solve
  getmatrix<-function() mdat
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above.

cacheSolve <- function(m = matrix(), ...) {
        ## Return a matrix that is the inverse of 'mat'
  mdat<-m$getmatrix()
  if(!is.null(mdat)){
    message("getting cached data")
    return(mdat)
  }
  ## Get the matrix from our object
  matrix<-m$get()
  mdat<-solve(matrix, ...)
  ## Set the inverse to the object
  m$setmatrix(mdat)
  mdat
}
