## All variables except 'x' have been named appropriately to have a better understanding of how the 
## function works.

## makeCacheMatrix is used to store cached data of a matrix and its inverse(if calculated)
## The output(A list of functions) is assigned to a variable

makeCacheMatrix <- function(x = matrix()) {

    currentInverse = NULL
    
    get = function(){
        x
    }
    
    set = function(newMatrix){
        x <<- newMatrix
        currentInverse <<- NULL
    }
    
    getInverse = function(){
        currentInverse
    }
    
    setInverse = function(calculatedInverse){
        currentInverse <<- calculatedInverse
    }
    
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)    
    
}


##cacheSolve finds the inverse of the matrix stored in a variable created using makeCacheMatrix function
##It then associates the inverse with that variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if(is.null(x$getInverse()) == TRUE){
        return(x$setInverse(solve(x$get(), ...)))
    }
    
    else{
        return(x$getInverse())
    }
    
}
