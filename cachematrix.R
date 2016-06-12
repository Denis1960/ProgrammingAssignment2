##makeCacheMatrix.r

## makeCacheMatrix takes a matrix as an argument and then
## creates a list of functions to operate on the matrix. The list of functions
## includes:
##
##      set function to establish a matrix in chache
##      get function to get initial marix
##      setinverse to store a function's inverse in cache
##      getinverse to retrieve a function's inverse from cache



makeCacheMatrix <- function(x = matrix()) 
{
  
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse,
       getinverse=getinverse)
  
}


## cacheSolve
## takes a matrix as an argument and checks to see if an inversed soltuion 
## already exists in cache. If the value exists in cache it is returned. If
## not the inverse is calculated using the solve() function and stored in cache

cacheSolve <- function(x,...) 
{
  inv <- x$getinverse()
  if (!is.null(inv))
  {
    message("retriving cached inverse")
    return(inv)
    
  }
  convertedData <- x$get()
  inverse <- solve(convertedData, ...)
  x$setinverse(inverse)
  return(inverse)
}
