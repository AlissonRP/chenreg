#' print.autochen


#' @param x  a model created by `auto_chen`
#' @param ... further arguments passed to or from other methods.
#' @rdname print.autochen
#' @export print.autochen
#' @export
print.autochen <-
  function(x, ...) {
    print(structure(x$results))
  }
