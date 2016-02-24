getIndeedJobs = function(searchterm, Pagination, includeSponsored=c("all", "sponsored", "unsponsored"), showProgress = TRUE, country , location) {

  if("rvest" %in% rownames(installed.packages()) == FALSE) {install.packages("rvest")}
  library("rvest") 
  
  mycountry <- country
  
  url <-  if(mycountry == "uk") {
      paste("http://www.indeed.co.uk")
    } else if (mycountry == "nl") {
      paste("http://www.indeed.nl")
    } else if (mycountry == "de") {
      paste("http://de.indeed.com")
    } else if (mycountry == "be") {
      paste("http://be.indeed.com")
    } else if (mycountry == "it") {
      paste("http://it.indeed.com")
    } else if (mycountry == "fr") {
      paste("http://www.indeed.fr")
    } else paste("http://www.indeed.com")
  
  urlloc <- gsub(" ", "+", location)
  term <- gsub(" ", "+", searchterm)
  
  
  if(showProgress) {
    print("scraping jobs...")
    iters = 0
    maxIters = Pagination*length(searchterm)
    pb = txtProgressBar(min = 0, max = maxIters, initial = 0, char = "#", style = 3) 
  }
  
  
  
  jobResults = {}
  for (term in searchterm) {
    allTitles = NULL
    allSummaries = NULL
    allcompany = NULL
    allsponsored = NULL
    
    for (i in 1:(Pagination)) {
      
      # The URL for the first page of results has a different structure than the rest
      if (i == 1 & includeSponsored != "sponsored") {
        indeed = read_html(paste0(url, "/jobs?q=",term ,"&l=",urlloc, collapse = NULL))
      } else if (i != 1 & includeSponsored != "sponsored"){
        indeed = read_html(paste0(url, "/jobs?q=",term ,"+&l=",urlloc, "&start=", (i*10)-10, collapse = NULL)) 
      } else {
        indeed = read_html(paste0(url, "/jobs?q=",term ,"+&l=",urlloc, "&start=", (i*10)-10, collapse = NULL))
      }
      
      # Grab the sponsored job titles
      sponsoredTitles = indeed %>% html_nodes(".jobtitle") %>% html_attr("title") %>% gsub("\n", "", .)
      
      #sponsored = indeed %>% html_nodes(".jobtitle") %>% html_attr("title") %>% gsub("\n", "", .)
      
      # Grab the unsponsored titles
      otherTitles = indeed %>% html_nodes(".jobtitle") %>% html_nodes("a") %>% html_attr("title") %>% gsub("\n", "", .)
      
      # Grap the job summaries (does not require separate sponsored and unsponsored pulls)
      tempSummaries = indeed %>% html_nodes(".summary") %>% html_text() %>% gsub("\n", "", .)
      
      # Grap the company (does not require separate sponsored and unsponsored pulls)
      tempcompany = indeed %>% html_nodes(".company") %>% html_text() %>% gsub("\n", "", .)
      
      
      
      if (includeSponsored == "all") {
        # Merge the sponsored and unsponsored titles, attempting to keep the order
        tempsponsored = sponsoredTitles
        tempsponsored[c(1:3,14:15)] <- "sponsored"
        tempsponsored[4:13]<- "unsponsored"
        
        sponsoredTitles[4:13] = otherTitles
        tempTitles = sponsoredTitles
        
      } else if (includeSponsored == "unsponsored") {
        tempTitles = otherTitles
        tempSummaries = tempSummaries[4:13]
        tempsponsored = sponsoredTitles[4:13]
        tempcompany = tempcompany[4:13]

      } else {
        tempsponsored = sponsoredTitles[c(1:3,14:15)]
        tempsponsored <- "sponsored"
        
        tempTitles <- sponsoredTitles[c(1:3,14:15)]
        tempSummaries <- tempSummaries[c(1:3,14:15)]
        tempcompany <- tempcompany[c(1:3,14:15)]
      }
      
      # Keep a running vector of the titles
      allTitles = append(allTitles, tempTitles)
      
      # All of the summaries (unsponsored and sponsored) can be pulled at once.
      allSummaries = append(allSummaries, tempSummaries)
      
      # check if all companies can be run?
      allcompany = append(allcompany, tempcompany)
      
      allsponsored = append(allsponsored, tempsponsored)
      
      if(showProgress) {
        iters = iters+1
        setTxtProgressBar(pb,iters)
      }
      
    } 
    if (includeSponsored == "all") {
      jobResults = data.frame(list(Company = allcompany, Titles = allTitles, Summaries = allSummaries, locations = location, searchterms = searchterm, issponsored = allsponsored))
#       jobResults$issponsored[3] <- "sponsored"
#       jobResults$issponsored[13] <- "unsponsored"
    } else if (includeSponsored == "unsponsored") {
      jobResults = data.frame(list(Company = allcompany, Titles = allTitles, Summaries = allSummaries, locations = location, searchterms = searchterm))
    } else {
      jobResults = data.frame(list(Company = allcompany, Titles = allTitles, Summaries = allSummaries, locations = location, searchterms = searchterm))
    }
    
  }
  return(jobResults)
}
