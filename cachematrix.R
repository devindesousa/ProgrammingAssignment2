## The below two functions create a matrix & calculate the inverse of the matrix.
## Input: x - square matrix
## Output: Inverse matrix of 'the x

## This function, makeCacheMatrix, creates a special "matrix"
## which is a list that contains 4 functions.
## 1st function: Sets the matrix value
## 2nd function: Gets the matrix value
## 3rd function: Sets the inverse matrix value
## 4th function: Gets the inverse matrix value
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The "cacheSolve" function calculates the inverse of the input matrix 'x'
## Where 'x' is a matrix that was created in 'makeCacheMatrix'
## It checks if the inverse has already been calculated & skips the computation, i.e. Solve function.
## Otherwise if the inverse has not been previously calculated then it does so & returns the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Test matrix, which has to be a square matrix
# B = matrix( c(2, 4, 3, 1), nrow=2, ncol=2) 