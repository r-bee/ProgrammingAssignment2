## The functions below allow caching of inverse matrix calculations.
## A matrix x can first be converted into a special matrix object xSpec
## using makeCacheMatrix(x). The xSpec object contains x and can also
## store its inverse. The function cacheSolve(y) takes a special matrix 
## object y and checks if the inverse has already been caclulated and 
## stored in y. If yes it returns the cached inverse otherwise it 
## calculates it. 


## Function: makeCacheMatrix(x)
## Arguments:
##   x:  an NxN matrix
## Returns:
##   A special matrix object which contains the matrix itself and
##   potnetially its inverse, if the inverse has been calculated
## Description:
##   The function takes an NxN matrix x and returns a list
##   of functions to set and access the matrix and its inverse: 
##      get     accesses the matrix
##      set     sets/resets the matrix,
##      setInv  sets the inverse of the matrix (calculated elsewhere) 
##      getInv  accesses the inverse of the matrix 
##   Initially, the matrix is initially set to x, and the inverse 
##   is set to NULL.
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    # set function / initial matrix is x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }    
    # get function 
    get <- function() x
    # setInv function
    setI <- function(inverse) inv <<- inverse
    # getInv function
    getI <- function() inv
    # return a list of the functions
    list(set = set, get = get,
         setInv = setI,
         getInv = getI)
         
}



## Function: cacheSolve(x)
## Arguments:
##   x:  a NxN special matrix object containing a matrix and
##       potentially its inverse
## Returns:
##   The inverse of the matrix in x
## Description:
##   Returns the inverse of the matrix in x. The function first checks  
##   if the inverse has been pre-calculated and stored in x. If yes, 
##   it returns the pre-calculated inverse. If not, it calculates
##   the inverse using solve(), returns it and stores it in the inverse 
##   part of x for future use.
cacheSolve <- function(x, ...) {
 
    # Check to see if the inverse is already in x
    inv <- x$getInv()
    if(!is.null(inv)) {
        ## return the stored inverse and exit
        message("getting cached data")
        return(inv)
    }   
    # Inverse is not in x
    # Get the matrix part of x
    matr <- x$get()
    # Calculate inverse
    inv <- solve(matr, ...)
    # Set the inverse in x for future use and return it
    x$setInv(inv)
    inv
    
}
