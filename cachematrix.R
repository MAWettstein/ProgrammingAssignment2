## This file contains a set of functions to create and manipulate a wrap-
## per object around a matrix that make it possible to avoid recaculating
## the inverse of the matrix if it has already been calculated.  They a-
## chieve this by caching the matrix and its inverse, and if the matrix
## has not changed, simply retrievingthe cached inverse, otherwise re-
## calulating, caching and returning the inverse of the new matrix.  The
## main functions are makeCacheMatrix, which creates the wrapper for a
## given matrix, and cacheSolve, which either retrieves the cached in-
## verse, or recalculates it if the matrix has been updated.

## Utility function replaceCachedMatrixWithNew compares the cached matrix
## with a new matrix in search of a reason to replace the cachedMatrix
## with the new one -- essentially, if the new matrix is different from
## the cached matrix, or if the cached matrix is invalid.  This function
## takes two arguments, the cached matrix and the new matrix, and returns
## a logical value that represents the result of the comparison: TRUE =
## "replace cached with new, since they're diferent", FALSE = "no need to
## replace cached with new, since they're the same".

replaceCachedMatrixWithNew <- function( cachedMatrix , newMatrix ) {

    # First condition for replacement: matrices are of different sizes,
    # as revealed by "dim" returning different values on both matrices:

    if ( ! all( dim( cachedMatrix ) == dim( newMatrix ) ) ) {
        return( TRUE )

    # Second condition for replacement: cached matrix contains one or
    # more NA values, in which case it is invalid, and cannot be invert-
    # ed.  The assignment specified that matrices passed to these func-
    # tions would always be invertible, therefore square and numerical,
    # so no need to check the new matrix for this:

    } else if ( sum( as.numeric( is.na( cachedMatrix ) ) ) > 0 ) {
        return( TRUE )

    # Third condition for replacement: if matrices are of same size (i.e.
    # they pass condition 1) and numerical (they pass condition 2), then
    # their elements are all of the same values:

    } else if ( ! all( cachedMatrix == newMatrix ) ) {
        return( TRUE )

    # Failing all of the above three conditions means that the cached
    # matrix needs to be replaced with the new matrix:

    } else {
        return( FALSE )
    }

}

## Function makeCacheMatrix creates a list object with four named items,
## each containing a function, one that saves away ("...$setMatrix( )")
## and another that retrieves ("...$getMatrix( )") a matrix, another that
## saves away ("...$setInverse( )" away and yet another that retrieves
## the inverse of the saved-off matrix.  If no argument is passed to this
## function, an empty matrix is saved away, otherwise the matrix passed
## to it is saved away, as if it had been called with no arguments, fol-
## lowed by a call to the list object's setMatrix function with the
## passed matrix as its parameter.  Hidden variables cachedMatrix and
## cachedInverse are redefined for each call to makeCacheMatrix, so that
## two objects created by this function have their own, independent
## caches:

makeCacheMatrix <- function( cachedMatrix = matrix( ) ) {

    # Define the hidden variable that saves off ("caches") the inverse
    # of the matrix, so we can retrieve it later if the matrix hasn't
    # changed since. and also, if its value is NULL, have a way of know-
    # in that the matrix *has* changed, prompting us to calculate its in-
    # verse:

    cachedInverse <- NULL

    # Object function ("method") setMatrix caches the matrix passed as
    # its argument by storing it in hidden variable setMatrix, but only
    # if there's a reason to do so.  Utility function CachedMatrixWithNew
    # (defined above, see its description) compares the currently cached
    # matrix with the new one, and if they are different, or the cached
    # matrix is undefined, returns a logical value of TRUE, prompting the
    # replacement of the cached matrix with the new one.  Also, the
    # cached matrix inverse gets cleared, which lets us know that the in-
    # verse needs to be recalculated when the time comes to retrieve the
    # inverse:

    setMatrix <- function( newMatrix ) {
        if ( replaceCachedMatricesWithNew( cachedMatrix , newMatrix ) ) {
            cachedMatrix  <<- newMatrix
            cachedInverse <<- NULL
        }
    }

    # Method getMatrix simply returns the cached matrix:

    getMatrix <- function( ) {
        return( cachedMatrix )
    }

    # Method setInverse caches the matrix inverse passed as is argument
    # by storing it in the hidden variable cachedInverse for later re-
    # trieval:

    setInverse <- function( newInverse ) {
        cachedInverse <<- newInverse
    }

    # Method getMatrix simply returns the cached matrix inverse:

    getInverse <- function( ) {
        return( cachedInverse )
    }

    # What this function returns is a list (an "object") whose named
    # itmes setMatrix, getMatrix, setInverse and getInverse contain the
    # methods defined above, which give us access to the cached matrix
    # and matrix inverse:

    return( list(
        setMatrix  = setMatrix  ,
        getMatrix  = getMatrix  ,
        setInverse = setInverse ,
        getInverse = getInverse
    ) )

}

## Function cacheSolve takes a matrix object returned by makeCacheMatrix
## (see above), determines if the inverse of the matrix cached inside the
## object is itself cached (which it is, if the cached matrix has not
## changed since the last invocation of this function) and, if the matrix
## is invertible (i.e., not NA), calculates the inverse and caches it
## away.  Whatever inverse is cached away at that point is what is re-
## turned.

cacheSolve <- function( matrixObject ) {

    # First, retrieve the currently cached matrix and matrix inverse:

    cachedInverse <- matrixObject$getInverse( )
    cachedMatrix <- matrixObject$getMatrix( )

    # If the cached matrix is NA, then don't attempt to find the in-
    # verse, just flag a warning and return NULL:

    if ( sum( as.numeric( is.na( cachedMatrix ) ) ) > 0 ) {
        warning( "Cannot invert an NA matrix." )
        return( NULL )

    # If the matrix is not NA, but the inverse is NULL (as it would be
    # if the cached matrix had been updated), then re-calculate the in-
    # verse and cache it away:

    } else if ( is.null( cachedInverse ) ) {
        cachedInverse <- solve( cachedMatrix )
        matrixObject$setInverse( cachedInverse )
    }

    # Return the cached inverse, whether it was just re-calculated or
    # not:

    return( cachedInverse )

}
