## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    set_matrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Function to get the matrix
    get_matrix <- function() x
    
    # Function to set the inverse of a matrix
    set_inverse <- function(inverse) m <<- inverse
    
    # Function to get the inverse of a matrix
    get_inverse <- function() m
    
    # construct a list of the elements
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$get_inverse()
    
    # If the inverse of matrix is avaliable, notify user with a message 
    # and return the inverse of matrix
    if(!is.null(m)) {
        message("getting cached invertible matrix")
        return(m)
    }
    
    # If the inverse of matrix is not available, then it will be calculated below
    data <- x$get_matrix()  # Retrieve the matrix from previous function
    m <- solve(data, ...)   # Calculate the inverse of matrix
    x$set_inverse(m)        # Set the inverse of matrix
    m                       # Return the inverse of matrix
}
