## Programming Assignment 2 - Coursera R-Programming Course

## makeCacheMatrix:  Method to return a list of functions on a given matrix
## Input Arguement:  Arguement is a Square Matrix
## Retrun Value:     Returns a list of the following functions
##                    1. Set the value of the matrix
##                     2. Get the value of the matrix
##                    3. Set the value of the inverse
##                    4. Get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  
  # Initially set to NULL
  # Changes when the user sets the value
  inv <- NULL
  
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method set's the Inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Method to get Inverse matrix
  getinverse <- function() inv
  
  # Create a List with the operations to set and get the Original Matrix and Inverse Matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}



## cacheSolve: Method to compute the inverse of the matrix.
## Input Arguement: Arguement is a Square Matrix
## Retrun Value: If 
##                inverse of the martix is already available in cache return the cached value
##               else
##                  after computing return the inverse of the matrix


cacheSolve <- function(x, ...) {
  
  # Check whether inverse of the matrix is already available, 
  # If available return the chached data
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message(" Cache value being returned... ")
    return(inv)
  }
  
  # If a cached data is not available, control of the code reaches this point
  # Find the inverse of the Matrix stored in x, using solve() method
  
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse matrix, and return the value of inverse matrix
  x$setinverse(inv)
  inv
}

## Usage of the Cache Matrix function
## m  <- matrix(1:2, nrow=2, ncol=2) // Create 2x2 matrix, use only Square Matrix !!!
## cm <- makeCacheMatrix(m) // Make the matrix and set the values that we intend to use
## cm$get() // Print the Original matix
## cacheSolve(cm) // Returns the inverse of the matrix and since its first call, would cache the value too
## cacheSolve(cm) // Subsequent call to this method returns the value from cache
