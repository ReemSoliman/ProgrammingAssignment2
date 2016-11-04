## The matrix inverse computing is expensive, if a matrix inserve will be used more than once in the code, it's
## better for the performance to compute it once and cache it.
## makeCacheMatrix creates a new matrix object that caches the matrix and it's inverse
## The cacheSolve function checks if the invere is cached in the memory to use it or compute the inverse and cache it.

## This function creates a matrix object that can cache its inverse.
## and returns a list of functions that can be used to access the matrix object
makeCacheMatrix <- function(mat  = matrix()) {
  #Set the matrix Inverse to Null when constructing/ creating a new object (new matrix)
  matInverse <- NULL;
  #Return the cached Matrix
  getMatrix <- function() mat
  #Set the cached matrix to new one
  setMatrix <- function(newMat)
  {
    #setting the matrix of the parent enviroment with a new matrix
    mat <<- newMat
    #set the matrix inverse of the parent enviroment to null, as the matrix changed.
    matInverse<<- NULL
  }
  #return the cached matrix inverse
  getInverse <- function() matInverse
  #set the cached inverse of the parent enviroment with a new value
  setInverse <- function(newInv) matInverse <<- newInv
  
  #return a list of the matrix object functions
  list(getMatrix=getMatrix, setMatrix=setMatrix, getInverse=getInverse, setInverse=setInverse )

}


##This function checks if the inverse of argument object is cached.
##if exists: it returns the cached inverse.
##if it doesn't exists, it computes it and cache it.

cacheSolve<- function(m,...){
    
    #Retrieve the matrix inverse by calling getInverse() function in m (matrix) object
    inv <- m$getInverse()
    #check the value of the returned inverse from getInverse()
    if(!is.null(inv)){
      #the value of the inverse was cached before
      print("Getting cashed data!!")
      #return the cached inverse
      return(inv)
    }
    
    #if the matrix inverse was not cached
    #Get the matrix of the object m
    newMatrix <-m$getMatrix()
    #compute the matrix inverse
    newInverse <- solve(newMatrix)
    #cache the inverse
    m$setInverse(newInverse)
    ## Return a matrix that is the inverse of 'm'
    newInverse
}
       

