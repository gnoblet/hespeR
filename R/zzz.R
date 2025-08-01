.onLoad <- function(...) {
  S7::methods_register()
}

.onUnload <- function(...) {
  S7::methods_unregister()
}
