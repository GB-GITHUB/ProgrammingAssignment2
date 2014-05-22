## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
# INPUT: a matrix x
# OUTPUT: a "special" matrix with methods
makeCacheMatrix <- function(x = matrix()) {
        cInvMatrix <- NULL

        # method setMatrix
        setMatrix <- function(matrix) {
                x <<- matrix
                cInvMatrix <- NULL
        }

        # method getMatrix
        getMatrix <- function() {x}
        
        # method setInvMatrix
        setInvMatrix <- function(i_matrix) {cInvMatrix<<-i_matrix}
        
        # method getInvMatrix
        getInvMatrix <- function() {cInvMatrix}

        # Return "special" matrix
        list(setMatrix=setMatrix, getMatrix=getMatrix,
             setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


# cacheSolve: 
#        Computes he inverse of the special matrix returned by makeCacheMatrix.
#        If inverse has already been calculated,, then cacheSolve retrieve the inverse from the cache
# INPUT: a matrix x
# OUTPUT: a "special" matrix with methods

cacheSolve <- function(x, ...) {
        ## retrieved cached matrices
        cInvMatrix <- x$getInvMatrix()
        cMatrix <- x$getMatrix()
        print(cMatrix)
        print(cInvMatrix)
        
        ## create an identity matrix with same size as given matrix
        idMatrix = diag(ncol(cMatrix))
        print(idMatrix)
        print(!is.null(cInvMatrix))
        print(dim(cMatrix) == dim(cInvMatrix))
        print(cMatrix %*% cInvMatrix)
        print(all((cMatrix %*% cInvMatrix) == idMatrix))

        ## Check if cached matrix exist
        ## Check if cached Matrix has same size than matrix to solve
        ## check if dot product of given matrix and cached inverse matrix gives an identity matrix
        if(!is.null(cInvMatrix) && dim(cMatrix) == dim(cInvMatrix) && all((cMatrix %*% cInvMatrix) == idMatrix)) {
                message("cached inverse matrix available")
                return(cInvMatrix)
        } else{
                message("cached inverse matrix does not exist")                
        }

        # Solve inverse matrix
        cInvMatrix <- solve(x$getMatrix(), ...)
        # Cached inverse matrix
        x$setInvMatrix(cInvMatrix)
        # Return inverse matrix
        cInvMatrix
}
