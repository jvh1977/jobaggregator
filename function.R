library("rvest")
library("dplyr")

i <- 00
n <- 500

while (i < n) {
  nam <- paste("myhtml", i, sep = "")
  compname <- paste("company", i, sep = "")
  titlename <- paste("jobtitle", i, sep = "")
  
  saveRDS(
    assign(titlename,
           as.matrix(html_text(
             html_nodes(assign(nam,
                               read_html(
                                 paste(
                                   "http://www.indeed.nl/vacatures?q=&l=Putten&radius=150&sort=date&limit=50&start=", i, sep = ''
                                 )
                               ))
                        ,".jobtitle")
           )))
    , paste("/Desktop/Test/Title/radius/","title",i,".RDS", sep = "")
  )
  
  
  saveRDS(
    assign(compname,
           as.matrix(html_text(
             html_nodes(assign(nam,
                               read_html(
                                 paste(
                                   "http://www.indeed.nl/vacatures?q=&l=Putten&radius=150&sort=date&limit=50&start=", i, sep = ''
                                 )
                               ))
                        ,  ".company")
           )))
    
    , paste("/Desktop/Test/Company/radius/","comp",i,".RDS", sep = "")
  )
  
  i = i + 10
}

Companyfiles <-   list.files(file.path("/Desktop/Test/Company/radius/"), full.names = TRUE)
Companydata <- lapply(Companyfiles, readRDS)
Companydata <- as.data.frame(do.call(rbind, Companydata))

Titlefiles <-   list.files(file.path("/Desktop/Test/Title/radius/"), full.names = TRUE)
Titledata <- lapply(Titlefiles, readRDS)
Titledata <- as.data.frame(do.call(rbind, Titledata))
aggregatedjobs <-   dplyr::bind_cols(as.data.frame(Titledata [1:2707,1]),Companydata)

