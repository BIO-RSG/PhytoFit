readYN <- function(pr) {
  if (interactive()) {
    n <- toupper(readline(prompt=pr))
  } else {
    cat(pr)
    con <- file("stdin")
    n <- toupper(readLines(con,1))
    close(con)
  }
  if(!(n=="Y" | n=="N")) {return()}
  return(n)
}
