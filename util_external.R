########################################
##  External programs for interactivity

util$excel <- function(d) {
  f = paste("/tmp/tmp.", round(runif(1)*1000),".csv",  sep='')
  # con = file(f, "w", encoding="MACROMAN")
  con = file(f, "w")
  write.csv(d, con, row.names=FALSE)
  close(con)
  # system(paste("open -a 'Microsoft Excel' ",f, sep=''))
  #system(paste("open -a '/Applications/Microsoft Office 2008/Microsoft Excel.app' ",f, sep=''))
  system(paste("open -a 'libreoffice' ",f, sep=''))
}

util$subl <- function(...) {
  system(paste("subl", ...))
}

util$vim <- function(...) {
  system(paste("vim",...))
}

util$ll <- function(...) {
  system(paste("ls","-l",...))
}

util$firefox <- function(...) {
  system(paste("firefox",...))
}

util$chrome <- function(...) {
  system(paste("google-chrome",...))
}

util$newwin <- function(x) {
  # Takes object printout into new file... dosink(OPEN=T) kinda subsumes this
  f = paste("/tmp/R_tmp.", round(runif(1)*100),".txt",  sep='')
  capture.output(print(x),file=f)
  # system("FILE_TO_VIEW=/tmp/tmp.txt /Applications/Utilities/Terminal.app/Contents/MacOS/Terminal /users/brendano/sw/bin/lame_viewer.sh")
  # system("DISPLAY=:0 /usr/X11R6/bin/xterm -geometry 80x60 -e less /tmp/tmp.txt &")
  system(paste("subl ",f," &", sep=''))
}
