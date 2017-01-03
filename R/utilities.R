readURL <- function(URL, n=-1L) {
    tries <- 0L
    msg <- character()
    while (tries < 3L) {
        URLdata <- tryCatch(readLines(URL, n), error=identity)
        if (!inherits(URLdata, "error"))
            break
        tries <- tries + 1L
    }
    if (tries == 3L)
        stop("failed to get URL after 3 tries:",
             "\n  url: ", URL,
             "\n  error: ", conditionMessage(URLdata))
    URLdata
}

mygetURL <- function(URL) {
    tries <- 0L
    msg <- character()
    while (tries < 3L) {
        URLdata <- tryCatch(getURL(URL, dirlistonly = TRUE), error=identity)
        if (!inherits(URLdata, "error"))
            break
        tries <- tries + 1L
    }
    if (tries == 3L)
        stop("failed to get URL after 3 tries:",
             "\n  url: ", URL,
             "\n  error: ", conditionMessage(URLdata))
    URLdata
}
