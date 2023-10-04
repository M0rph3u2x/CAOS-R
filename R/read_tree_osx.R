# 2a) Barcoder: Read tree ------------------------------------------------------
#' Title Read tree file (Linux/OSX)
#'
#' @param TREE.Path Path to tree file
#' @param Dataset_Names Name of datasets
#'
#' @return Return tree data
#' @export
#'
#' @examples read_tree_osx(TREE.Path, Dataset_Names)
read_tree_osx <- function(TREE.Path, Dataset_Names){
  tree = read.tree(file=TREE.Path)
  quartz() #Create plot in new window
  plotTree(tree,offset=1)
  tiplabels()
  nodelabels()
  png_path = paste("output_barcoder/",Dataset_Names, "/Tree_Preview.png", sep="")
  dev.copy(png, png_path)
  while (!is.null(dev.list()))  dev.off() #Save plot & close window

  return(tree)
}
