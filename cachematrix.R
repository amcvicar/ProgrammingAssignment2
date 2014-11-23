## Functions to reduce the computational cost associated with calculating
## the inverse of a matrix by allowing the inverse to be stored and called from a 
## special matrix rather than computing it repeatedly

##Example:
## mat <- matrix(c(7,-2,3,5), 2, byrow = T)
## x <- makeCacheMatrix(mat)
## cacheSolve(x) ##first call will calculate and store the inverse
## cacheSolve(x) ##second call will get the stored inverse

## makeCacheMatrix
## Makes a special matrix to store the inverse or a square matrix with the following steps:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  ##defines a function to set the vector, x, to a new vector, y, and resets the inverse matrix, m, to NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ##returns the matrix, x
  get<-function() x
  setmatrix<-function(solve) m<<- solve ##sets the inverse matrix, m, to solve
  getmatrix<-function() m ##returns the inverse matrix, m
  ##returns the 'special matrix'
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## cacheSolve
## Function to return a matrix that is the inverse of 'x' either from the cache
## or computed using the solve function
cacheSolve <- function(x, ...) {
  
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}