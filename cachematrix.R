## These two functions are used to store a matrix and cache its inverse.
## Caching avoids repeating the expensive computation of a matrix inverse multiple times.

##############################################
## Function 1: makeCacheMatrix
##############################################

## This function creates a special object (a list of functions) that stores:
## - A matrix
## - Its inverse (once calculated and cached)
## It returns a list of 4 functions: set(), get(), setInverse(), and getInverse()

makeCacheMatrix <- function(x = matrix()) {
  
  # This variable will hold the cached inverse of the matrix
  inverseMatrix <- NULL

  # 1. set() function: sets the value of the matrix
  # It also clears any previously stored inverse when the matrix changes
  set <- function(y) {
    x <<- y                 # <<- assigns the new matrix to the variable in the parent environment
    inverseMatrix <<- NULL # Reset the inverse cache since the matrix has changed
  }

  # 2. get() function: returns the current matrix
  get <- function() {
    x
  }

  # 3. setInverse() function: stores the inverse matrix in the cache
  setInverse <- function(inverse) {
    inverseMatrix <<- inverse
  }

  # 4. getInverse() function: returns the cached inverse if available
  getInverse <- function() {
    inverseMatrix
  }

  # Return a list of the four functions above so they can be accessed outside
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##############################################
## Function 2: cacheSolve
##############################################

## This function calculates and returns the inverse of the matrix stored in the object created by makeCacheMatrix
## It first checks if the inverse has already been cached.
## If yes, it retrieves the cached inverse (saving time).
## If not, it calculates the inverse using solve(), stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
  
  # Try to get the cached inverse matrix
  inverseMatrix <- x$getInverse()
  
  # If the inverse is already cached, return it with a message
  if (!is.null(inverseMatrix)) {
    message("Getting cached inverse...")
    return(inverseMatrix)
  }

  # If the inverse is not cached, compute it

  # Step 1: Get the matrix from the cache object
  data <- x$get()

  # Step 2: Compute the inverse using solve()
  inverseMatrix <- solve(data, ...)

  # Step 3: Cache the inverse for future use
  x$setInverse(inverseMatrix)

  # Step 4: Return the newly computed inverse
  inverseMatrix
}
