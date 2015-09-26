cacheSolve <- function(x){
    mc <- makeCacheMatrix()
    mc$set(x)
    if ( is.null(mc$getInverse()) ){
        mc$setInverse(solve(mc$get()))
    }
    mc$getInverse()
}