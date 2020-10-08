## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#the makeCacheMatrix accept a matrix object as an input
#return a list of four function for setting getting a matrix
#and setting getting the inverse of the matrix. 
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(invm) inv<<-invm
  getinv<-function() inv
  list(set = set, get = get, setinv=setinv,getinv = getinv)
}


## Write a short comment describing this function
#accept the return value of the above function as an input 
# casheSolve check if an inverse of the matrix in the cache 
#it return the inverse, otherwise if compute the inverse of the
#matrix passed to the above function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if (!is.null(inv)){
    message("getteing cashed data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
