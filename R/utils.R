"%without%" <-  function(x, y) x[!(x %in% y)]

"%nin%" <- function(x, y) {
    return( !(x %in% y) )
}
