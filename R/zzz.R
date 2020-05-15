ct <- function(l) Filter(Negate(is.null), l)

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
        paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

trim <- function(str) gsub("^\\s+|\\s+$", "", str)

# stct <- function(str, pattern) regmatches(str, regexpr(pattern, str))
mtch <- function(str, pattern, ...) {
  regmatches(str, regexec(pattern, str, ...))
}

# unless(FALSE, 5)
# unless(TRUE, 5)
unless <- function(x, y) {
  if (x) force(y)
}

last <- function(x) x[length(x)]

# asb("asdf")
# asb("")
# asb(FALSE)
asb <- function(x) {
  if (is.character(x) && nzchar(x)) return(TRUE)
  return(FALSE)
}
