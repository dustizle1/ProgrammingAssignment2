######################## Week 3, Assignment 2: Lexical Scoping #####################
 

## These two functions were created to save processing time when repeatedly solving
## the same matrix inversion. Instead of computing a solution every time it is needed,
## these functions work together to store a cached solution that is retrieved every
## time following the creation of the cache.

## Academic Honest Disclaimer: Code was written with the assistance of a youtube video
## by Pradesh Pudasaini, entitled "Coursera's R Programming Week 3 Peer-Graded 
## Assignment (Lexical Scoping)". This source is available at the following location:
## https://www.youtube.com/watch?v=9WXQa9NlGDs





## The below function creates the cache of the matrix inversion. This 
## function mediates the cacheSolve function and is used to store a value
## for later use to save processing time.

makeCacheMatrix <- function(sc = matrix()) {
        invm <- NULL
        set <- function(sb) {
                sc <<- sb
                invm <<- NULL
        }
        get <- function() {sc}
        setInverse <- function(inverse) {invm <<- inverse}
        getInverse <- function() {invm}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The below function has two processes. The first is check whether the solution
## to the matrix is already in the system cache. It does this by checking if the
## "inv" object is has a value. If it does not have a value (is NULL), makeCacheMatrix
## is called to create a cached solution based on the value of solve(). If there
## is a value for "inv", then a text message will display indicating a solution is
## in the cache and will be available shortly. The end result is still a solution 
## to the matrix, however there is an intermediate process if the matrix had not
## been previously solved.

cacheSolve <- function(sc, ...) {
        invm <- sc$getInverse()
        if(!is.null(invm)) {
                message("data fastball, comin' hot!")
                return(invm)
        }
     mat <- sc$get()
     invm <- solve(mat, ...)
     sc$setInverse(invm)
     invm
}





