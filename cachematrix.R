## Here is my solution for assignment 2.

##step 1 - make a function to define a square invertable matrix called 'x'
##set the value and set the inverse to NULL (m)

makecachematrix <- function(x = matrix()) {
  m = NULL
  set = function(y) {
  x <<- y
  m <<- NULL
  }
  get = function() x
  setinv = function(inverse) m <<- inverse 
  getinv = function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  }

##step2 Make a function that checks to see if there is an inverse allready calculated for the matrix.
cachesolve <- function(x=matrix, ...) {
  m = x$getinv()
  if (!is.null(m)){
  message("getting cached data")
  return(m)
  }

##Or else, just calculate it if it doesn't already exist       
  mat.data = x$get()
  m = solve(mat.data, ...)
  x$setinv(m)
  return(m)
}

##step 3 assign a matrix to the function to test it
##run it twice to get the "getting cached data message"
g<-makecachematrix()
g$set(matrix(1:4,2,2))
cachesolve(g)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

cachesolve(g)
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
