
# the set of the two main functions are  used to cache the inverse of a matrix to help save the unnecessary costly 
# computation
# function 1: makeCacheMatrix, function 2: cacheSolve
# to run/test the functions, do the following after source ("cachematrix.R")
# step 1, define a matrix x 
# step 2: run "a=makeCacheMatrix(x)"
# step 3: run "cacheSolve(a)".  if it is the first time,it will use solve() to calculate inverse
# step4: run"cacheSolve(a)" again, it will use the cached value.  


#  define a function to decide if the input matrix to makeCacheMatrix() has inverse using try function 
# this function will be called in the cache
checkInverse <- function(n) class(try(solve(n),silent=T))=="matrix"

# the makeCacheMaxtrix function creates a special "matrix" object that can cache its inverse
# the return value formakeCacheMatrix () will be input to the cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
  
  # define four functions called setInverse() and getInverse(), set() and get(()
  m<-NULL 
  set <-function (y){
    x<<- y
   m<<- NULL
    
  }
  get <-function () x
  setInverse<-function(inverse)  m<<-inverse 
  getInverse<- function ()m 
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


# cacheSolve() function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache
# the input for cacheSolve function is the return value of makeCacheMatrix ()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # first check if x has inverse, if not, return
  if (checkInverse (x$get()) == FALSE)
  {
   message ("the input matrix does not have an inverse! Check again")
   return   
  }
  else
  {
    
    m <-x$getInverse()
    if(!is.null(m)) {
       # if the m is not null, meaning the inverse is already calculated and cached
      
        message("getting cached inverse matrix")
        return(m)
      
      
    }
    
    #else, use solve function to get the inverse and return 
    message("do not find any cached value or the input matrix changed, use Solve() to calculate the inverse")
    m<-solve(x$get())
  
    x$setInverse(m)
    
    return (m)
    
  }
  
  
}
