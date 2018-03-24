rasterErrorCheck <- function(input) {
  if (extension(input) == ".img") {
    NULL
  } else {
    "Choose an image file"
  }
}