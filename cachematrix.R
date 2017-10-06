## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix - In this function, the matrix to be inverted is assigned 
#to a list of functions, where the functions will do appropriate get and set
# of the pointers

  #get() - to get the matrix 
  #set() - to set the matrix to environment variable
  #getMatrix() - This function will return the cached inverse of matrix
  #setMatrix() - This function will set the inversed matrix in cached place

# I have added a logic to verify the symmetrical nature of matrix

# cacheSolve - This function will find the inverse of the matrix
# First it checks if the inverse is already available, then it returns
# the inverse of matrix cached in memory
# if the inverse is not available, then it will compute the 
# inverse of the matrix and share the results
  #solve() function is used for finding inverse of a square matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  if (nrow(x)!= ncol(x))
  {  
    print("Assymetrical matrix, solve function can't be used for Inv calc")
    return (NULL)
  }
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(inv_matrix) m <<- inv_matrix
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
