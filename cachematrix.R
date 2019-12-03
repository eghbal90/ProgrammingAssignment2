## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
  matInv=NULL
  
  set <-function (y){
    x<<-y
    m<<-NULL
  }
  get <- function()x
  setMatInv <- function(solve) matInv<<-solve
  getMatInv <-function () matInv
  list(set=set, get=get, setMatInv=setMatInv, getMatInv=getMatInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  matInv<-x$getMatInv()
  if (!is.null(matInv)){
    message("getting cached data")
    return(matInv)
    
  }
  data<-x$get()
  matInv<-solve(data)
  x$setMatInv(matInv)
  matInv
}
