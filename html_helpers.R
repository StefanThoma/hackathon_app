vectorBulletList <- function(vector) {
  if(length(vector > 1)) {
    paste0("<ul><li>",
           paste0(
             paste0(vector, collpase = ""), collapse = "</li><li>"),
           "</li></ul>")
  }
}
