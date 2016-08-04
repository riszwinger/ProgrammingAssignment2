## Caching the Inverse of a Matrix

###Function makeCacheMatrix : 
## This function creates a special "matrix" object that can cache its inverse.

###Function cacheSolve : 
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.



makeCacheMatrix <- function(x=matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## makeCacheMatrix creates a list containing a function to
#  set the value of the matrix
#  get the value of the matrix
#  set the value of the Inverse
#  get the value of the Inverse


cacheInverse <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}



###########################################
##########***---INPUT---***##########
###########################################
m1 <- matrix(c(1,2,3,4),2,2)  
solve(m1) ## Inverse m1
m2 <- matrix(c(12,2,21,4),2,2)
solve(m2) ## Inverse m2

###########################################
##########***---TEST---***##########
###########################################

Input <-makeCacheMatrix(m1)
# Check Initial Input to makeCacheMatrix function
Input$get()
# Check Inverse value for first time 
# Expected Value NULL , since no Inverse has been calculated
Input$getInverse()

## call cacheInverse function to calculate Inverse for first time
cacheInverse(Input)
## Now Inverse is cached , so below call will get the inverse of Input matrix
Input$getInverse()
solve(m1) ## TEST Case 1 : Output matches with above call 

## Setting input matrix 
Input$set(m2)
cacheInverse(Input)
Input$getInverse()
cacheInverse(Input) ## getting cahed data
solve(m2) ## TEST case 2 : Output matches with above call 

## Setting Inverse 
Input$setInverse(m1)
Input$getInverse()
cacheInverse(Input)
cacheInverse(Input) ## getting cahed data
m1 ## TEST case 3 : Output matches with above call


