## The following R functions cache and then compute the inverse of a given matrix. Matrix inversion is known
## to be a costly operation so caching the inverse of the matrix before computation could prove handyy and 
## more efficient instead of repeatedly computing it.

## 'makeCacheMatrix' creates a special matrix object that is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        matrice<-NULL
        
        set<-function(y) {
                x<<-y
                matrice<<-NULL
        }
        
        get<-function() return x
        
        setInverseMatrix<-function(inverse) matrice<<-inverse
        
        getInverseMatrix<-function() return(x)
        
        return(list(set=set,get=get, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix))

} ##end of function 'makeCacheMatrix


## The 'cacheSolve' function computes the inverse of a special "matrix" returned by the 'makeCacheMatrix' above
## If the inverse has already een calculated in a previous computation (and the matrix has not change), then
## the 'cacheSolve' function should retrieve the inverse from the cache. Faster, more efficient and performant.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrice <-x$getInverseMatrix()
        
        ## Using an 'if' statement to ensure that the matrix is not NULL.
        ## If the inverse matrix resulting from x is non-NULL, returned the cached data
        
        if(!is.null(matrice)){
                
                message("Getting the cached data")
                
                return(matrice)
                
        } ## end of 'if' statement
        
        data<-x$get()
        
        matrice<-solve(data,...)
        
        return(matrice)
        
} ##end of 'cacheSolve' function
