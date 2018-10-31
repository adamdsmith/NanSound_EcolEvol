expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

# Define function to capitalize first letter of string
Cap <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}
