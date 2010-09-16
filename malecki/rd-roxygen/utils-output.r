
comment_line <- function(x, exdent = 0) { 
  if (missing(x)) return("#' ")
  
  strwrap(x, width = 80, exdent = exdent, prefix = "#' ")
}

comment_tag <- function(tag, value) {
  if (is.null(value) || value == "" || length(value) == 0) return()
  
  comment_line(paste(tag, value), exdent = 2)
}