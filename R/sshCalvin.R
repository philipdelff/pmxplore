###################################################
# sshCalvin.R
# 
# Author: Helena Edlund
# Created on: 2017-03-23
# Purpose: Function to ssh Calvin from Zilter
# Dependencies: None
###################################################

sshCalvin <- function(cmd, dir="./", ...) {
  # The function requires passwordless access between calvin and zilter. 
  # The character string "cmd" should be a linux command 
  # as if you were using putty/other ssh program. 
  # dir is the dir where you want to execute the command. 
  system(paste0("ssh -q calvin.seml.astrazeneca.net \"cd $(pwd); cd ", dir ,
                "; module load psn nonmem && ", cmd, "\"" ), ...)
  }