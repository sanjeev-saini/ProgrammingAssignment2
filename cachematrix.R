## Programming Assignment 2: Caching inverse of a matrix
## Functions provide ability to solve the inverse of a matrix and cache the result
##    which avoids processing repeatedly.


## Creates a list of functions:
## set : To set the matrix provided
## get : to get the matrix provided
## setcache: to save the cache of the inverse of the matrix provided
## getcache: to get the cache of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  invX <- NULL
  
  set <- function(y = matrix()){
    x <<- y
    InvX <<- NULL
  } 
  
  get <- function() x
  
  setcache <- function(z = matrix()){
    invX <<- z
  }
  
  getcache <- function() invX
  
  l_lst <- list(set = set, 
                get = get,
                getcache = getcache,
                setcache = setcache)
  
  l_lst
}

## Takes an object of makeCacheMatrix as argument
## calculates and saves inverse of matrix in cache
## If inverse of matrix already exists then returns cached value

cacheSolve <- function(x){
  l_inv <- x$getcache()
  
  if(!is.null(l_inv)){
    message("Getting cached Inverse Matrix")
    return(l_inv)
  }
  
  l_mtx <- x$get()
  
  l_inv <- solve(l_mtx)
  
  x$setcache(l_inv)
  
  l_inv
}