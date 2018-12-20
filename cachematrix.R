## the following script contains two functions. The first function (named makeCacheMatrix) allows to create an object
## that contains a matrix and its inverse. The second function (named cacheSolve) allows to compute the inverse of the matrix 
## stored in the object created with makeCacheMatrix.
## Assunption: it is assumed that the input matrix is invertible 

## makeCacheMatrix builds a set of functions and returns the list of these functions.
## The returned function allows: to set the matrix (set), to get the matrix (get), to set the matrix inverse (setSolve) and 
## to get the matrix inverse ## (getSolve)
## Usage examples:
## Object creation: myMatrix <- makeCacheMatrix(matrix(c(2,5,1,4,3,7,5,8,4),3,3))
## Object get: myMatrix$get()
## Object set: myMatrix$set(matrix(c(4,6,1,3),2,2))
## Get solved matrix: myMatrix$getSolve()
## Store solved matrix in myMatrix object (solMat has been created by cacheSolve function) : myMatrix$setSolve(solMat)
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(mat2) {
                x <<- mat2
                s <<- NULLmymatrix
        }
        get <- function() x
        getSolve <- function() s
        setSolve <- function(solved) s <<- solved
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## cacheSolve inverts the matrix stored in "matrix" object (see makeCacheMatrix) that has been provided as input parameter
## if the solved matrix has been already computed the function return the solved matrix already stored in the object
## otherwise coumputes it and stores it in the "matrix" object
## Usage examples: cacheSolve(myMatrix)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if (!is.null(s)) {
                message("getting solved matrix")
                return(s)
        }
        my_mat <- x$get()
        s <- solve(my_mat, ...)
        x$setSolve(s)
        s
}

