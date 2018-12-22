## These functions allow us to define a new set of functions for a cached matrix. The first function
## gives us the tools to make a cached matrix, the second gives us a tool to calculate/return its inverse


## This function defines the internal cached inverse and defines get and set for the original matrix
## x, as well as the inverted matrix stored_inverse.
makeCacheMatrix <- function(x = matrix()) {

  ## This function is basically identical to the sample. No calculations done here, just storiing
  ## of values
  stored_inverse <- NULL
  set <- function(y) {
    x <<- y
    stored_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) stored_inverse <<- inverse
  getinverse <- function() stored_inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function takes a makeCacheMatrix type and
## checks to see if there is a stored inverse and returns it. If there isn't
## one stored already, then it will run the solve computation and store it using setinverse.
cacheSolve <- function(x, ...) {

  ## store whatever inversed matrix is stored. It will be NULL if there is none.
  inversed_matrix <- x$getinverse()
  
  #if it's not null, that means it's already cached and we don't have to calc again
  if (!is.null(inversed_matrix)) {
    message("Inversed Matrix is already stored, skipping calculation")
  } else
  ## if it is null, then we run the solve function and store it back in using setinverse
  {
    inversed_matrix <- solve(x$get(), ...)
    x$setinverse(inversed_matrix)
  }

  #return inversed matrix
  inversed_matrix
  
}
