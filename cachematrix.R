## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set <-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setInverse<-function(inverse) {inv<<-inverse}
  getInverse<-function() {inv}
  list(set =set,get =get, setInverse=setInverse,getInverse=getInverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve<-function(x,...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("Retrieving from Cached Data")
    return(inv)
  }
  mat <-x$get()
  inv <-solve(mat,...)
  x$setInverse(inv)
  inv
}

#Verification Steps 
#mymatrix<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
#mymatrix$get()
#mymatrix$getInverse()
#NULL
#cacheSolve(mymatrix)
