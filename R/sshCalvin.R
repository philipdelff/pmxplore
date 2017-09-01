
#' @title ssh function from zilter to calvin
#' @description ssh function from zilter to calvin
#' @param cmd string of unix command to execute
#' @param dir wd where the cmd is executed, Default: './'
#' @param ... additional to system()
#' @return character string echoed from submitted unix command 
#' @details   
# The function requires passwordless access between calvin and zilter. 
# The character string "cmd" should be a linux command 
# as if you were using putty/other ssh program. 
# dir is the dir where you want to execute the command. 
#' @rdname ssh_calvin
#' @export 

ssh_calvin <- function(cmd, dir="./", ...) {
  system(paste0("ssh -q calvin.seml.astrazeneca.net \"cd $(pwd); cd ", dir ,
                "; module load psn nonmem && ", cmd, "\"" ), ...)
}