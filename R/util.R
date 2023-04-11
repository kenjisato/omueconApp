encode <- function(x) {
  x |>
    str_split_1("\n") |>
    serialize(connection = NULL) |>
    brotli_compress() |>
    base64encode() |>
    utils::URLencode(reserved = TRUE)
}

decode <- function(x) {
  x |>
    utils::URLdecode() |>
    base64decode() |>
    brotli_decompress() |>
    unserialize() |>
    str_flatten(collapse = "\n")
}


