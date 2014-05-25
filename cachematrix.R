## CRAIG SNODGRASS
## MAY 25, 2014
## PROGRAM ASSIGNMENT #2

## FUNCTION 1 OF 2
## AT THE HIGHEST LEVEL, THE FUNCTION NOT ONLY ESTABLISHES THE MATRIX
## BUT CALCULATES INVERSE AT THE SAME TIME AND STORES TO BE CALLED
## AGAIN LATER.  THIS WAY THE EXPENSIVE OPERATION OF CALCULATING THE
## INVERSE IS ONLY DONE ONCE DURING CREATION
## NEEDS A MATRIX AS AN INPUT
## OUTPUT IS ACTUALLY A LIST OF 4 FUNCTIONS
#### set - ASSIGNS THE DATA TO SET
#### get - RETURNS THE DATA
#### setinverse - THE FUNCTION THAT PERFORMS THE MATRIX INVERSE
#### getinverse - THE FUNCTION THAT RETURNS THE RESULT OF THE INVERSE

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#########################################################################
## THIS IS FOR INTERNAL LOGIG TESTING AFTER FUNCTION IS RUN
## HIGHLIGHT CODE BETWEEN ## AND EXECUTE
## amatrix <- makeCacheMatrix()  ## ASSIGN amatrix AS THE FUNCTION
## class(amatrix) ## MAKE SURE THE OUTPUT IS A LIST
## class(amatrix$set) ## MAKE SURE IT IS A FUNCTION
## class(amatrix$get) ## MAKE SURE IT IS A FUNCTION
## class(amatrix$setinverse) ## MAKE SURE IT IS A FUNCTION
## class(amatrix$getinverse) ## MAKE SURE IT IS A FUNCTION
## str(amatrix) ## ENSURE THE LIST IS ACTUALLY A LIST OF 4 FUNCTIONS
###### TEST CASE 1 - SIMPLE 2X2
## testmatrix1 = (matrix(c(1,2,3,4), nrow=2, ncol=2)) ## SIMPLE 2X2
## amatrix1 = makeCacheMatrix(testmatrix1) ## RUNS FUNCTION
## testmatrix1 ## RETURNS ORIGINAL MATRIX
## amatrix1$get() ## SHOULD RETURN SAME MATRIX AS ORIGINAL
###### TEST CASE 2 - 7X7
## testmatrix2 = matrix(stats::rnorm(49), nrow=7, ncol=7)
## amatrix2 = makeCacheMatrix(testmatrix2) ## RUNS FUNCTION
## testmatrix2 ## RETURNS ORIGINAL MATRIX
## amatrix2$get() ## SHOULD RETURN SAME MATRIX AS ORIGINAL
#########################################################################

## FUNCTION 2 OF 2
## WHEN THIS FUNCTION IS USED TO CREATE A MATRIX, IT ALSO CALCULATES
## THE INVERSE SO IT CAN BE REFERENCED LATER IN CACHE
## IF THE INVERSE HAS ALREADY BEEN CALCULATED, IT DOES NOT RECALCULATE
## A NOTE WILL BE GIVEN WHEN THE CACHED RESULT IS BEING RETURNED

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#########################################################################
## THIS IS FOR INTERNAL LOGIG TESTING.  HIGHLIGHT AND EXECUTE
###### TEST CASE 1
## cacheSolve(amatrix1) ## WILL RETURN THE INVERSE MATRIX
## amatrix1$get() %*% cacheSolve(amatrix1) ## IDENTITY MATRIX CHECK
## cacheSolve(amatrix1) ## WILL RETURN THE INVERSE MATRIX AS WELL
#### AS THE MESSAGE THAT THE INVERSE WAS RETRIEVED FROM CACHE
## amatrix1$get() %*% cacheSolve(amatrix1) ## IDENTITY MATRIX CHECK
## WILL ALSO RETURNED MESSAGE THAT THE INVERSE WAS RETRIVED FROM CACHE
###### TEST CASE 2
## cacheSolve(amatrix2) ## WILL RETURN THE INVERSE MATRIX
## amatrix2$get() %*% cacheSolve(amatrix2) ## IDENTITY MATRIX CHECK
## cacheSolve(amatrix2) ## WILL RETURN THE INVERSE MATRIX AS WELL
#### AS THE MESSAGE THAT THE INVERSE WAS RETRIEVED FROM CACHE
## amatrix2$get() %*% cacheSolve(amatrix2) ## IDENTITY MATRIX CHECK
## WILL ALSO RETURNED MESSAGE THAT THE INVERSE WAS RETRIVED FROM CACHE
