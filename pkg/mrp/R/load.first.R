.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "MRP"))
  ver <- packageDescription("MRP", lib = mylib)$Version
  builddate <- packageDescription("MRP", lib = mylib)$Date
  packageStartupMessage(paste("\nMRP (Version ", ver, ", built: ", builddate, ")\n", sep = ""))
}
