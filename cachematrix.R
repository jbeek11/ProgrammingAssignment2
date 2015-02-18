## In this script there are two functions, that can be used to take a square matrix, calculate its inverse matrix,
## store it in cache, and when the inverse for the same matrix is requested again, retrieve it from cache,
## rather than calculate it again (which is a CPU-intensive task).

## The functions should be used as follows:
## Create 2x2 matrix: matrix <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## Create special 'vector' with matrix within and the functions we need and assign it: x <- makeCacheMatrix(matrix)
## Calculate inverse matrix: cacheSolve(x)
## Result is the inverse matrix, result is also cached within 'x'
## When called again with the same matrix: cacheSolve(x)
## Inverse is not calculated but retrieved from cache

## This function , makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean
## when called with a predefined matrix as argument, this is stored in variable 'x'
## when called without matrix as argument, an empty matrix is created
makeCacheMatrix <- function(x = matrix()) {
  ## First, set the cached inverse matrix to NULL, we are starting a 'new' run here
  i <- NULL
  ## Define what to do when setting the non-inverse matrix
  set <- function(y) {
    ## Assign new matrix to global variable 'x'
    x <<- y
    ## Set the cached inverse matrix to NULL, since we are working with a new matrix
    i <<- NULL
  }
  ## Define what to do when returning the non-inverse matrix
  get <- function() x
  ## Define what to do when setting the cached inverse matrix
  ## This method gets a matrix that is already inversed from the cacheSolve() function,
  ## so all we have to do is store it in the global 'i' variable
  setinverse <- function(inverse) i <<- inverse
  ## Define what to do when returning the inverse matrix
  getinverse <- function() i
  ## Define what to show when performing this function without assigning it directly to a variable
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Check if matrix is in the cache
  i <- x$getinverse()
  if(!is.null(i)) {
    ## Inverse matrix found in cache
    message("getting cached inverse matrix")
    return(i)
  }
  ## Inverse matrix not found in cache, calcullating inverse matrix and put it in cache
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
