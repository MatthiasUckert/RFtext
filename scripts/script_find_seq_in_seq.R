find_seq_in_seq <- function(.seq_find, .seq_base) {
  w <- seq_along(.seq_base)
  for (i in seq_along(.seq_find)) {
    w <- w[.seq_base[w + i - 1L] == .seq_find[i]]
    if (length(w) == 0) return(integer(0))
  }
  w <- w[!is.na(w)]
  return(w)
}


find_seq_in_seq(c(5:6), 1:25)
