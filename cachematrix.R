## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
##  First it sets the data of the matrix
##  Second it gets the data of the matrix
##  Third it sets the inverse of the matrix
##  Fourth it gets the inverse of the matrix
##  Finally it creates a list of the functions above
##
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function(){return(x)}
  setInverse<-function(inverse){m<<-inverse}
  getInverse<-function(){return(m)}
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

##  First it checks if there is an cached values of calculated inverse matrix.
##  If so, it returns cached data
##  If not, it creates a variable named 'data' and assigns it 
##  to have the original matrix stored in x, solves the matrix, 
##  assigns new cached inverse values to x and returns the inverse values of x.
##  

cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setInverse(m)
  return(m)
        ## Return a matrix that is the inverse of 'x'
}
