## The following functions will compute the inverse of a matrix taking the benefit of 
#  caching the inverse of a matrix rather than compute it repeatedly. Matrix inversions are computational heavy operations and
# hence caching the inverse for a matrix which does not frequently changed will help save computation expense. 
# I wrote the pair of functions that cache the inverse of a matrix.


#The function "makeCacheMatrix" will create a special "matrix" object that can cache its inverse.It returns the list of all functions that
# can be applied on the special matrix object (such as set/get matrix, set/get matrix inverse)

makeCacheMatrix <- function(x = matrix())
{
  # Deining the Inverse of Matrix(i)  as NULL
  i <- NULL
  
  # Defining the function to set the value of the Matrix. The function 
  # also set the inverse of matrix as NULL. This is to ensure the cache of 
  # the matrix inverse  is flushed in case the matrix (x) is changed.
  set <- function(y)
  {
    # The operation <<- is applied to assign the value to objects x and i which are in an environmeny which is
    # different than the current environment. Current Env: is inside  the function get, environment of x and i is
    # the immediate parent environment (corresponding to env inside function makeCacheMatrix)
    x <<- y
    i <<- NULL
  }
  
  #Define the function which will return the value of the  matrix x
  get <- function() x
  
  #Define the function which will set the value of the inverse of the matrix (inverse is called as i). Pease note <<- operator is used
  # for value assignment. This is to cache the value of the matrix inverse.
  setinverse <- function(inverse) i <<- inverse
  
  # Define the function to get the value of the matrix inverse. Cache value is returned if the original matrix "x" is not changed.
  # Assumption is that any changes to original matrix x will be done only via method "set" (Defined in this function)
  getinverse <- function() i
  
  # Returns the list of all the functions defined inside the function "makeCacheMatrix". This list will be returned as the output.
  list(set = set, get =  get, setinverse = setinverse, getinverse = getinverse)
  
}


# The function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...)
{ 
  #Find the matrix inverse, if already exits.
  i <- x$getinverse()
  
  #Check if the inverse is null or not. If NOT NULL, the value of the inverse is extract from the cache. It will exit the function
  # after retrieving the matrix inverse
  if(!is.null(i))
  {
    print("getting cached data")
    return(i)
  }
  
  # In case, cache value does not exists or is null, we will calculate the matrix inverse from scratch. The following
  # step will retrive the value of the original matrix.
  data <- x$get()
  
  #Calculate the inverse of the matrix using solve function. Assumption: original matrix is a square invertible matrix.
  i <- solve(data)
  
  # Set the value of the inverse to the special matrix
  x$setinverse(i)
  
  # Return Matrix Inverse
  i
}
