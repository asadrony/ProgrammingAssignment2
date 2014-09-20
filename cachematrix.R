## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mdat<-NULL
  set<-function(m){
    mat<<-m
    mdat<<-NULL
  }
  ## Method to get and set the matrix
  get<-function() mat
  setmatrix<-function(solve) mdat<<- solve
  getmatrix<-function() mdat
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'mat'
  mdat<-mat$getmatrix()
  if(!is.null(mdat)){
    message("getting cached data")
    return(mdat)
  }
  ## Get the matrix from our object
  matrix<-mat$get()
  mdat<-solve(matrix, ...)
  ## Set the inverse to the object
  mat$setmatrix(mdat)
  mdat
}
