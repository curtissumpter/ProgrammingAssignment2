## Put comments here that give an overall description of what your
## functions do

## Number of Functions: 3

## Names of Functions:
##     matequal:
##             Purpose: tells if two matrixes are equal.  It is an internal function not
##             meant for user use but it can be.
##     makeCacheMatrix:
##             Purpose: it is initialized with a n x n matrix. This is assumed and not
##             error checked.  It returns a list of four functions: get - retrieves the 
##             matrix currently held by the function, set - changes the matrix currently
##             held by the function, getinverse - gets the inverse currently held by 
##             the function.  setinverse - sets the inverse currently held by the function.
##             Note: This function does not actually calculate anything. It simply gets
##             and sets when appropriate.  It's purpose is to allow for the inverse to 
##             be stored and cached, shrinking elapsed and user times.
##     cacheSolve:
##             Purpose: the function is initialized with an instance of the makeCacheMatrix
##             object.  The actual calculation of the inverse is done within this object.
##             This function is built to work with the makeCacheMatrix function in order
##             to calculate the inverse as little as possible.



matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)


makeCacheMatrix <- function(x = matrix()) {

  # Begin by setting the inverse to null
  
  inverse <- NULL
  
  # The inner set function should set the x value to origMatrix and
  ## the inverse to NULL only if the origMatrix and the x value are unequal. 
  # Otherwise both variables are left alone.
  
  set <- function(origMatrix) {
    
    if(!(matequal(origMatrix, x)))
    {
      
      message("matrixes unequal.  Changing x.")
      x <<- origMatrix
      inverse <<- NULL
            
    }
    else message("matrixes equal.  Not changing anything.")
  }
  
  # get calls an anonymous function that returns the x matrix
  
  get <- function() x
  
  # sets the inverse of the x matrix
  
  setinverse <- function(i) inverse <<- i
  
  # gets the variable inverse which should be if the object is used properly the
  # inverse of the matrix
  
  getinverse <- function() inverse
  
  # returns a list of functions 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
  
}

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x' which is a makeCacheMatrix function 
  ## which returns a list.
  
  i <- x$getinverse()
  
  ## If the cached value of the inverse actually exists, return it and exit.
  
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  ## If the cached value of the inverse does not exist continue the function.
  
  ## Get the current value of the matrix within the makeCacheMatrix function.
  
  data <- x$get()
  
  ## Calculate the inverse here.
  
  i <- solve(data)
  
  ## Set the inverse here in the caching list object
  
  x$setinverse(i)
  
  ## return the inverse value
  
  i
  


}
