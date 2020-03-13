import_nps <- function(path){
  require(data.table)
  require(stringr)
  p<-c(path)
  l<-list.files(p)
  for (i in 2:5){
    assign(paste("nps", str_sub(path, -2, -1), i,
                 sep="_"),
           data.frame())
    a=data.frame()
    for (j in 1:length(l)){
      if (substr(l[j], 14, 14) == i) {
        a = rbind(a,
                  fread(paste(p, l[j],
                              sep="/")))
      }
    }
    print(i)
    assign(paste("nps", str_sub(path, -2, -1), i,
                 sep="_"),
           a,
           envir = .GlobalEnv)
  }
}
