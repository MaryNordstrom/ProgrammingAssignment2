
                #makeCacheMatrix contains 4 functions
                #1.set takes a matrix y and changes the matrix in the main function to y
                #2.get returns the matrix x stored in the main function
                #3.setinverse is a function that stores the inverse matrix in ix
                #4.getinverse is a function that returns the inverse matrix (ix)
                #getinverse is a function that returns the inverse matrix (ix)
makeCacheMatrix <- function(x = matrix()) {
                #makeCacheMatrix stores 4 functions in a list
ix <- NULL
set <- function(y){
  x<<-y
  ix<<-NULL
}
get <- function()x
setinverse <- function(inverse) ix<<-inverse
getinverse <- function() ix
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



                #cacheSolve verifies the existance (and is not NULL) of the  
                ##inverse matrix (ix) stored previously with getmatrix
                #IF it exists in memory, it returns the message and value of inverse
                #ELSE, if the inverse matrix does not exist in memory,
                ##data takes the stored matrix with makeCacheMatrix
                #solve computes the inverse of the square matrix called "data"
                #ix stores the inverted matrix
                #x$setinverse(ix) stores the inverse in the object generated with makeCacheMatrix
cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
 ix <- x$getinverse()
 if(!is.null(ix)) {
   message("getting cached data")
   return(ix)
  }
 data <- x$get()
 ix <- solve(data)
 x$setinverse(ix)
 ix
}
