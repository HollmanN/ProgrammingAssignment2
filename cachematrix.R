#Author: Hollman N. 2016-03-15

#Following the example on https://github.com/rdpeng/ProgrammingAssignment2,
#this function uses the inverse variable to store in cache the result of
#the function solve(x) (The inverse of the matrix x)

#Example of use:
#example <- matrix(c(4,3,3,2), nrow = 2, ncol = 2 , byrow = TRUE)
#cacheMatrix <- makeCacheMatrix(example)
#cacheSolve(cacheMatrix)
#Verify results at http://www.mathwords.com/i/inverse_of_a_matrix.htm

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(matrix) {
    #Check the type of the matrix argument
    if(identical(class(matrix), "matrix"))
    {
      x <<- matrix
      inverse <<- NULL
    }
    else
    {
      stop("The argument of the function 'set' must be a matrix type!")
    }
  }
  get <- function() x
  #Check the type of the inv argument
  setInverse <- function(inv)
  {
    if(identical(class(inv), "matrix"))
    {
      inverse <<- inv
    }
    else
    {
      stop("The argument of the function 'setInverse' must be a matrix type!")
    }
  }
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#Function to return a matrix that is the inverse of 'x'
#using a cache mechanism

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  else
  {
    data <- x$get()
    message("Calculating inverse via solve(data)")
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
  }
}
