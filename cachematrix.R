# By Christian Kincaid
# Coursera R Programming Assignment 2

# The following two functions are used to cache the inverse of a matrix 

# makeCacheMatrix:
# 1) sets the value of the matrix
# 2) gets the value of the matrix
# 3) sets the value of the inverse
# 4) gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
  {
    i <- NULL
    set <- function(y)
    {
      x <<- y
      i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve computes the inverse of the matrix

cacheSolve <- function(x, ...) 
  {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i))
    {
      message("getting cached data")
      return(i)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
  }