
setwd("C:\\Users\\m1029322\\Desktop\\test")

# Matrix inverse calculation


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


my_matrix <- makeCacheMatrix(matrix(numeric(0), 0,0) )  #Creating dummy matrix
my_matrix$get()
my_matrix$set(matrix(c(45, 23, 45, 32), 2, 2))
my_matrix$get()     #Just to display matrix
my_matrix$getInverse()  # (this will result null)
cacheSolve(my_matrix)
my_matrix$getInverse()  # (this will result inverse matrix)