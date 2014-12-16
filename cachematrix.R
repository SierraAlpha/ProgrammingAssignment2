##The following functions will cache the vaue of a given matrix, x, and cache and return the
##inverse of x.  They also allow a user to pull the value of x and reset the value of x.

## The makeCacheMatrix is a function takes a given matrix, x, and stores the matrix
## outside of the environment of the function.  It also defines the get, getinverse,
##and setinverse functions--get will pull the value of the matrix x, setinverse
##sets the value for the inverse of x as part of the cacheSolve function below, and getinverse
##will return the inverse of x provided the cacheSolve function has been run.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   ##caches the value of the inverse of x as Null
  set <- function(y) { ##allows new values for x to be set
  x <<- y
  i <<- NULL
    }
  get <- function() x  ##returns the matrix x passed to the function
  setinverse <- function(solve) i <<- solve   ##defines the setinverse function
  getinverse <- function() i  ##defines getinverse function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## cacheSolve calculates, returns, and stores the inverse of matrix x stored in makeCacheMatrix

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()  ##gets the inverse, as defined in makeCacheMatrix
  if(!is.null(i)) {  ##i is null when if this is the first time x has been passed to function
    message("getting cached data")
    return(i) #returns the new value of i which is defined below
  }
  data <- x$get() ## pulls the cached value of x into the cacheSolve function
  i <- solve(data, ...)  ##creates the inverse of matrix x
  x$setinverse(i)  ##caches the value of i so it can be pulled from outside the function
  i ##returns i
}
