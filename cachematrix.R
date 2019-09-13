# This function creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # Define the argument with default mode of 'matrix'
        Matx_inverse <- NULL # We initialize 'Matx_inverse' as NULL. It will hold value of matrix inverse
        
        set <- function(y) {
                x <<- y
                Matx_inverse <<- NULL
        } # We defined the 'set' function to assign the new value of the matrix
        # in parent environment
        
        get <- function() {
                x
        } # Return value of the matrix argument
        
        setinverse <- function(inverse) {
                Matx_inverse <<- inverse
        } # Assign value of 'Matx_inverse' in the parent environment
        
        getinverse <- function() {
                Matx_inverse
        } # Get the value of 'Matx_inverse' where called
        
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}

# This function computes the inverse of the special 'matrix' returned by 'makeCacheMatrix'
# function

# If the inverse has been already calculated (and the matrix has not changed),
# the   'cacheSolve' function (bellow) will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        Matx_inverse <- x$getinverse()
        
        if(!is.null(Matx_inverse)) {
                message('getting cached data')
                return(Matx_inverse)
        }
        
        data <- x$get() # Get the original matrix data
        Matx_inverse <- solve(data, ...) # Use 'solve()' function to inverse the matrix
        x$setinverse(Matx_inverse) # Set the invertible matrix
        Matx_inverse # Return the invertible matrix
}
