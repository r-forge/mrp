
tag <- function(x) attr(x, "Rd_tag")

untag <- function(x) {
  attr(x, "Rd_tag") <- "TEXT"
  x
}

reconstruct <- function(rd) {
  if (is.null(rd)) return()
  
  if (is.list(rd)) {
    special <- tag(rd) == toupper(tag(rd))
    prefix <- ifelse(special, "", paste(tag(rd), "{", sep = ""))
    suffix <- ifelse(special, "", "}")
    
    paste(prefix, paste(sapply(rd, reconstruct), collapse = ""), suffix, 
     sep = "")
  } else {
    rd
  }  
}