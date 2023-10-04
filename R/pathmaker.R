# Barcoder: Create output path/folder for loaded dataset ---------------------------------

#The function needs one, but can handle two path values.
#Output directory is the first default value that will be
#used in the R Script directory path
#' Title Create output path/folder for loaded dataset
#'
#' @param out.path Output directory
#' @param path1 Subfolder of output
#' @param path2 Subsubfolder of output
#'
#' @return Return output path
#' @export
#'
#' @examples pathmaker(out.path,Dataset_Names[i])
#' @examples pathmaker(out.path,Dataset_Names[i], paste("/",marker,"/caos",sep=""))
pathmaker <- function(out.path,path1,path2=NULL){
  if(!is.null(path2)){
    path <- paste(out.path, "/", path1, path2, sep="")
  }else{
    path <- paste(out.path, "/", path1, sep="")
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  return(path)
}

#-------------------------------------------------------------------------------
