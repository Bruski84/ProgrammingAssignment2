#This function creates a special "matrix", which is really a list containing a function to
#1.	set the value of the vector
#2. 	get the value of the vector
#3.	set the value of the mean
#4.	get the value of the mean 

# For this to run, x must be an invertable matrix
# If x is not an invertable matrix, the solve function in the next function will produce an error
# For assignment purposes, assume only invertable matrices are used for input


makeCacheMatrix <- function(x = matrix()) 

{
	inverse <- NULL
	set <- function(y) 
{
	
	x <<- y	 	
	inverse <<- NULL
}
      get <- function() x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


#This next function takes the output matrix of makeCacheMatrix and returns the inverse of the original input matrix
#Once again, assume that the input matrix is invertible, or there will be an error returned


cacheSolve <- function(x, ...) 

{
	inverse <- x$getinverse()
	if(!is.null(inverse)) 
{     
	message("getting cached data")
      print(inverse)
      
}
      matdata <- x$get()
      inverse <- solve(matdata, ...)
      x$setinverse(inverse)
      print(inverse)
}
