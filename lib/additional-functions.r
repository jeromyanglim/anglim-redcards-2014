logit2p <-  function (x)  exp(x) / (1 + exp(x))

line_num_cat <- function(x){
    tmp <- unlist(strsplit(x, "\n"))
    cat(paste0(seq_len(length(tmp)), ": ", tmp, collapse = "\n"), "\n")
}
