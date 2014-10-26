## Put comments here that give an overall description of what your
## functions do:

## The first function 'makeCacheMatrix' creates a function to cache
## the inverse of a matrix. Also returns functions to get and set values of
## the matrix and get inverse from the cache.
## The second function 'cacheSolve' checks if the inverse already exists in the 
## cache and returns it. If not it calculates the inverse and sets it into the cache
## and returns the inverse.

## Write a short comment describing this function

## The function below returns a vector containing list of functions to:
## set value of matrix, get value of matrix, set inverse and get inverse of the 
## matrix, resepectively.

makeCacheMatrix <- function(x = matrix())
  {
  invs <- NULL              ## clears inverse value in cache
  
  set <- function(y) {      ## set function
    x <<- y                 ## setting value of matrix to x
    invs <<- NULL           ## and clearing inverse value 
  }  
  
  get <- function() x       ## get function, returns value of matrix
  
  setinverse <- function(i) invs <<- i      ## sets inverse value   
  getinverse <- function() invs             ## gets inverse value
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ##returns function list
  
  }


## Write a short comment describing this function

## The function below checks if the inverse already exists and returns it from the cache,
## if not calculates the inverse of x, sets it in the cache and returns it.

cacheSolve <- function(x, ...) 
  { 
     i <- x$getinverse()       ## gets inverse value by calling getinverse() from vector 
                               ## created by 'makeCacheMatrix'
     
      if(!is.null(i)) {        ## checks if 'i' is not null (inverse exists)
      message("getting cached data")    
      return(i)                ## returns inverse
    }
    
                               ## if inverse doesn't exist in cache, calculates it here
    data <- x$get()            ## assigns value of matrix to 'data'
    i <- solve(data, ...)      ## assigns inverse value of 'data' to i
    x$setinverse(i)            ## sets value of i to cache
    message("setting inverse to cache")
    i                          ## returns inverse value
  }


## To test the script:
## First create a sample matrix and assign it to an object: m<-matrix(c(1:4),2,2)
## Assign makeCacheMatrix(m) to an object: mt<-makeCacheMatrix(m)
## Run cacheSolve: cacheSolve(mt)
## It should return the message "setting inverse to cache" and should print the inverse
## Run cacheSolve(mt) again
## It should return the message "getting cached data" and should print the inverse
