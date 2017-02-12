## R Programming Week 3 Assignment
## Pair of functions that cache the inverse of a matrix.

##  makeCacheMatrix creates a special "matrix", a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        inverseMatrix <- NULL
        
        set <- function(y){
                x <<- y
                inverseMatrix <<- NULL              
        }
        get <- function() x
        getInverse <- function() inverseMatrix
        setInverse <- function(inversed = NULL) inverseMatrix <<- inversed
        list(set = set, get = get, getInverse = getInverse, setInverse = 
                     setInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data) #%*% data
        x$setInverse(m)
        m
}

#TEST CODE
#assignmentWeek3 <- makeCacheMatrix()

#assignmentWeek3$get()
#assignmentWeek3$getInverse()
#assignmentWeek3$setInverse()

#testmatrix <- matrix(rnorm(16, 5), 4, 4)
#assignmentWeek3$set(testmatrix)

#cacheSolve(assignmentWeek3)
