## Function makeCacheMatrix takes matrix as an argument and allows to
##  1. Set matrix values
##  2. Get matrix
##  3. Set inverse matrix
##  4. Get inverse matrix
## 


makeCacheMatrix <- function(x = matrix()) {
  nc<-ncol(x)
  nr<-nrow(x)
  im<-matrix(ncol = nc,nrow = nr)
  set<-function(y = matrix()){
    ncy<-ncol(y)
    nry<-nrow(y)
    x<<-matrix(ncol = ncy,nrow = nry)
    x<<-y
    im<<-matrix(ncol = ncy,nrow = nry)
  }
  get <- function() x
  setInvMatr <- function(matr) 
    im <<- matr
  getInvMatr <- function() im
  list(set = set, get = get,
       setInvMatr = setInvMatr,
       getInvMatr = getInvMatr)
}

## Function cacheSolve uses subfunctions from makeCacheMatrix and first it checks wheter inverse martix has been already calculated,
## and if not it calculates the inverse matrix by using default function solve()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInvMatr()
  if(!is.na(sum(im))) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInvMatr(im)
  im

}

##Example with a matrix 3x3

x<-matrix(c(3,5,2,7,8,4,4,9,4),ncol=3,nrow=3)
x
cacheSolve(makeCacheMatrix(x))
