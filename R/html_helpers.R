#' vector to html bullet list
#'
#' @param vector string vector
#'
#' @return html bullet list
#' @export
#'
#' @examples
#' vectorBulletList(c("item 1", "item 2"))
#'
vectorBulletList <- function(vector) {
  if (length(vector > 1)) {
    paste0(
      "<ul><li>",
      paste0(
        paste0(vector, collpase = ""),
        collapse = "</li><li>"
      ),
      "</li></ul>"
    )
  }
}
