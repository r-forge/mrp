.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "mrp"))
  ver <- packageDescription("mrp", lib = mylib)$Version
  builddate <- packageDescription("mrp", lib = mylib)$Date
  packageStartupMessage(paste("\nmrp (Version ", ver, ", built: ", builddate, ")\n", sep = ""))
}
