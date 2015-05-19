## Two R functions are created: makeCacheMatrix and cacheSolve.
## Together these two functions solve the inverse of a matrix 
## (assuming that the input matrix is always inversible) and store
## the inverse matrix in the cache for future calls, thusing saving
## computation resources. 

## When a matrix is first given, it should be passed to the makeCacheMatrix
## function. The output (e.g., temp) should then be passed to the
## cacheSolve function to solve the inverse. In subsequent situations
## when the inverse is needed, just call cacheSolve(temp).

## Created on RStudio 3.1.2 Windows 7 OS

## The makeCacheMatrix takes an input matrix and 
## output a list of 4 functions, which are:
## 1. set: set the value of the matrix
## 2. get: get the value of the matrix
## 3. setInv: set the value of the inverse
## 4. getInv: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  
  set<-function(y){
    x<<-y
    inv<<-NULL
  }  
  get<-function() x  
  setInv<-function(solve) inv<<-solve  
  getInv<-function() inv 
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The cachesolve function takes the output from makeCacheMatrix
## as its input and outputs the inverse of the original matrix. 
## It first looks up the inverse of the matrix in the cache. 
## If the inverse already exisits, it outputs the inverse directly
## thus saving time. Otherwise, it computes the inverse and outputs
## the inverse. Meanwhile, the inverse is stored in the cache for
## future calls. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) ## this assignment assumes the input matrix is inverable 
  x$setInv(inv)
  inv
}
