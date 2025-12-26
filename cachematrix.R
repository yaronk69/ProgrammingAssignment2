## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # 1. Function to set the matrix
  set <- function(y) {
    x <<- y      # Use <<- to assign value to 'x' in the parent environment
    inv <<- NULL # If the matrix changes, reset the inverse to NULL
  }
  
  # 2. Function to get the matrix
  get <- function() x
  
  # 3. Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # 4. Function to get the inverse
  getInverse <- function() inv
  
  # Return a list of the four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  # If the inverse exists, return it from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not in cache: get the matrix, calculate the inverse, and cache it
  data <- x$get()
  inv <- solve(data, ...) # solve() calculates the matrix inverse
  x$setInverse(inv)       # Save the result back to the cache
  
  inv
}

# 1. Create a simple 2x2 matrix
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

# 2. First run: Calculates the inverse (No "cached data" message)
cacheSolve(my_matrix)

# 3. Second run: Retrieves from cache (Should see "getting cached data")
cacheSolve(my_matrix)