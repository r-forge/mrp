# Parse input Rd file -------------------------------------------------------
parse_file <- fuction(path) {
  rd <- tools::parse_Rd(path)

  tags <- sapply(rd, tag)
  tags <- gsub("\\\\", "", tags)
  names(rd) <- tags

  # Remove top-level text strings - just line breaks between sections
  rd <- rd[tags != "TEXT"]

  out <- list()
  # Title, description, value and examples, need to be stitched into a 
  # single string.
  out$title <- reconstruct(untag(rd$title))
  out$desc <- gsub("$\n+|\n+^", "", reconstruct(untag(rd$description)))
  out$details <- reconstruct(untag(rd$details))
  out$value <- reconstruct(untag(rd$value))
  out$examples <- reconstruct(untag(rd$examples))

  # Join together aliases and keywords
  out$name <- reconstruct(rd$name)
  out$aliases <- unname(sapply(rd[names(rd) == "alias"], "[[", 1))
  # If the only alias is the name, then skip it
  if (identical(aliases, name)) {
    aliases <- NULL
  }
  out$keywords <- unname(sapply(rd[names(rd) == "keyword"], "[[", 1))

  # Pull apart arguments
  arguments <- rd$arguments
  arguments <- arguments[sapply(arguments, tag) != "TEXT"]
  out$params <- sapply(arguments, function(argument) {
    paste(argument[[1]], reconstruct(argument[[2]]))
  })
  
  out
}

# Create output --------------------------------------------------------------
create_roxygen <- function(info) {
  c(
    comment_line(info$title),
    comment_line(info$desc),
    comment_line(),
    comment_line(info$details),
    comment_line(),
    comment_tag("@param", info$params),
    comment_tag("@value", info$value),
    comment_tag("@keywords", paste(info$keywords, collapse = " ")),
    comment_tag("@aliases", paste(info$aliases, collapse = " ")),
    if (!is.null(info$examples)) {
      c(
        comment_line("@examples\n"), 
        paste("#' ", gsub("\n", "\n#' ", info$examples), sep = "")
      )
    },
    "\n"
  )
}

parse_and_save <- function(path) {
  parsed <- parse_file(path)
  roxygen <- create_roxygen(parsed)
  cat(paste(output, collapse = "\n"))
}
