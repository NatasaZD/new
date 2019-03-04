## Below are two functions that are used to create a special object that stores
## matrix and cache's its inverse.  

## The first function (makeCacheMatrix) creates a special object, which is 
## a list containing four functions writen within the makeCacheMatrix function. 
## makeCacheMatrix function is used to get a matrix as an input, 
## set the value of a matrix, get the value of a matrix, set the inverse matrix, 
## and get the inverse matrix.  
## <<- operator is used to assign a value to an object in the environment that is
## different from the current environment

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function()x
  setinverse <- function(inverse)inv <<- inverse
  getinverse <- function()inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The second function (cacheSolve) takes the output of the previous matrix 
## makeCacheMatrix as an input and checks if inverse matrix from makeCacheMatrix
## has any value in it or not.
## If the inverse matrix from the makeCacheMatrix 
## has some value in it (is not NULL), it returns a message "getting cached data".
## If the inverse matrix from the makeCacheMatrix is empty, 
## it gets the original matrix data and set the invertible matrix by using 
## the solve function.
## At the end the function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x,...){
  inv <- x$inverse
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  return(inv)
}
