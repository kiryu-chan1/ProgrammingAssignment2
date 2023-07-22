
# the functions primarily receives a matrix stores the matrix in a variable  
# locally to the function and then an inverse is made using the solve function
# and then store it as a cache for retrieval later

# this function is mainly used as a base to sit out the setters and getters to 
# build up the cache for storage and retrieval
makeCacheMatrix <- function(x = matrix()) {
    # creates a null object to store the cache in
    i <- NULL
    # a function to store new input that can be cached later and removing the 
    # existing cache all by calling the <<- operator that changes the variable
    # in a lexical scope (the variable defined in the function)
    setter <- function(y) {
        x <<- y
        i <<- NULL
    }
    # a function to print out the stored input 
    getter <- function() x
    # a function that is called later to store the inverse matrix as in the 
    # cache variable using <<-
    setinversemat <- function(inv) i <<- inv
    # a function to print out the stored cache
    getinversemat <- function() i
    # a list to name the function available to easily call them later using the 
    # $ operator
    list(setter = setter, getter = getter,
         setinversemat = setinversemat,
         getinversemat = getinversemat)
}


# the function takes in the list of objects that is stored in a variable using 
# the makeCacheMatrix function to operate on these objects and to check if there
# is a cache already in store if not it sets a new cache following the fucntion
# solve() that makes an inverse matrix
cacheSolve <- function(x, ...) {
    # extracts the cache variable
    i <- x$getinversemat()
    # checks out if a cache already exists
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # extracts the input 
    data <- x$getter()
    # stores the inverse matrix in a variable
    i <- solve(data, ...)
    # sets the inverse to the cache variable
    x$setinversemat(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
# debug(makeCacheMatrix)
# debug(cacheSolve)
# mymatrix <- makeCacheMatrix(matrix(data = sample(1:9, 9), nrow = 3, ncol = 3))
# cacheSolve(mymatrix)
# mymatrix$getinversemat()
