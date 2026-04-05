## Convert strings to a requested case style
convert_case <- function(x, case = 'sentence') {
  case <- match.arg(case, c('sentence', 'lower', 'upper', 'title'))
  
  get_sentence <- function(value) {
    value <- tolower(value)
    substr(value, 1, 1) <- toupper(substr(value, 1, 1))
    value
  }
  
  get_title <- function(value) {
    value <- tolower(value)
    words <- strsplit(value, "\\s+")[[1]]  # Get each word, split by spaces
    words <- sapply(words, get_sentence, USE.NAMES = FALSE)
    paste(words, collapse = " ")
  }
  
  result <- switch(case,
                   lower = tolower(x),
                   upper = toupper(x),
                   sentence = get_sentence(x),
                   title = sapply(x, get_title, USE.NAMES = FALSE)
  )

  result
}
