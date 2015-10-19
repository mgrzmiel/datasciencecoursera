## R Programming
## Programming Assignment #2
## The goal of this project is to write a pair of functions which allow to cache the inversion of matrix
## since that computation is extremly time-consuming.


# makeCacheMatri - this function create a set a functions which allows to get the matrix, set the matrix,
# set the inverse matrix and get the inverse matrix if already cached.
makeCacheMatrix <- function(x = matrix()) {
    # set the inverse matrix to null
    inverse_matrix <-NULL
    # set the matrix and the inverse matrix toi null
    set_matrix <- function(y){
        x <<- y
        inverse_matrix <<- NULL
    }
    
    # return the matrix
    get_matrix<-function(){
        x
    }
    
    #set the inverse matrix
    set_inverse_matrix <- function(inv_matrix){
        inverse_matrix <<- inv_matrix
    }
    
    # get the inverse matrix
    get_inverse_matrix <- function(){
        inverse_matrix
    }
    
    # put all functions to the list
    list(set_matrix = set_matrix, get_matrix = get_matrix,
    set_inverse_matrix = set_inverse_matrix,
    get_inverse_matrix = get_inverse_matrix)
}



## cacheSolve - this function returns the inverse matrix of the provided matrix. However in the
## case when the inverse matrix has been already calculated ach cached, it return the cached data
## to avoid executing the same work many times.
cacheSolve <- function(x, ...) {
    # check if the inverse matrix is already claculated and chached in the memory
    inverse_matrix <- x$get_inverse_matrix()
    
    if(!is.null(inverse_matrix)){
        message("getting cached data")
        return(inverse_matrix)
    }
    
    # get the matrix which will be inverse
    matrix<-x$get_matrix()
    # inverse the matrix
    inverse_matrix<-solve(matrix)
    # set the inverse matrix so in the future used cached data
    x$set_inverse_matrix(inverse_matrix)
    # return a matrix that is the inverse of 'x'
    return(inverse_matrix)
}

## testing
m<-matrix(1:4, 2, 2)
some_matrix<-makeCacheMatrix(m)
cacheSolve(some_matrix)



