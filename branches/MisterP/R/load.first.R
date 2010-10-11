.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "MisterP"))
  ver <- packageDescription("MisterP", lib = mylib)$Version
  builddate <- packageDescription("MisterP", lib = mylib)$Date
  cat(paste("\nMisterP (Version ", ver, ", built: ", builddate, ")\n", sep = ""))
}
