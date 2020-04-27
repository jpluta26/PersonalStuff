library(rvest)
library(dplyr)

# webscrape for scott
# try to find which NJ towns have an official budget


# ========================================================================================= #
# ============================= functions ================================================= #
# ========================================================================================= #

# ---------------------------- getTownURL ------------------------------------------------- #
# parse the wikipage to get the url for the town
getTownURL <- function( wiki.town  )
{
  # the town name will (hopefully) always be in the url
  # search by town name
  twn.wiki.dat <- read_html(wiki.town)
  twn.name <- tolower(strsplit(strsplit(wiki.town, "[/]")[[1]][5], "[,]")[[1]][1])
  twn.name <- gsub("_", "", twn.name)
  
  twn.wiki.tmp <- twn.wiki.dat %>% html_nodes("li") 
  tmp = as_list(twn.wiki.tmp[grepl(paste0(".", twn.name, "."), twn.wiki.tmp)])
  
 
  # search by the word "official"; wikipedia is standardized, so the town
  # url should always be attached to "official website" or "official site"
  ind = unlist(lapply(lapply(tmp, getLoc), isTRUE))
  
  # if nothing is found by town name, check nj.com or nj.gov.
  # if !any(ind) means no matches found
  if( !any(ind))
  {
    tmp = as_list(twn.wiki.tmp[grepl("nj.com", twn.wiki.tmp)])
    ind = unlist(lapply(lapply(tmp, getLoc), isTRUE))
  } 
  
  if( !any(ind))
  {
    tmp = as_list(twn.wiki.tmp[grepl("nj.gov", twn.wiki.tmp)])
    ind = unlist(lapply(lapply(tmp, getLoc), isTRUE))
  } 
  
  if( !any(ind))
  {
    tmp = as_list(twn.wiki.tmp[grepl("nj.us", twn.wiki.tmp)])
    ind = unlist(lapply(lapply(tmp, getLoc), isTRUE))
  } 
  
  if( !any(ind))
  {
    return(NULL)
  }
  
  dat = tmp[ind][[1]]
  
  # the tricky part- these objects are lists with multi-level dataframes
  # assume that the url is in the highest level df (could probably search each level
  # if need be). get each level name, and traverse to the highest level
  lst.objs <- strsplit(names(rapply(dat, class)), "[.]")[[1]]
  for( i in lst.objs)
  {
    dat <- dat[[i]]
  }
  
  # finally, fetch and return the url
  twn.url <- attr(dat, "href")
  
  return(twn.url)
}
# ------------------------------------------------------------------------------------------- #




# ------------------------------------- recursiveURL ----------------------------------------- #
# traverse links recursively, never really got this working
recursiveURL <- function( url, depth )
{
  if( depth > 5 )
  {
    return(url)
  }
  
  pg <- read_html(url) %>% html_nodes("a") %>% html_attr("href")
  
  
  if( pg == "" | is.na(pg) | length(pg) == 0)
  {
    return(NULL)
  } else
  {
    recursiveURL(paste0(url, pg), depth + 1)
  }

}
# ------------------------------------------------------------------------------------------- #


# ------------------------------------ readTownSite ----------------------------------------- #
# scan the site for a particular town and try to find the official budget
readTownSite <- function( twn.url )
{
  dat <- read_html(twn.url)
  dat.nodes <- dat %>% html_nodes("body") %>% html_nodes("a") %>% html_attr("href")
  
  out = list()
  for( i in dat.nodes[4])
  {
    out = list(out, recursiveURL( paste0(twn.url, i), 0))
  }
  
  
  files <- dat.nodes[grepl(".pdf", dat.nodes) | grepl(".xls", dat.nodes) | grepl(".xlsx", dat.nodes)]
  
  if( length(files) == 0)
  {
    return( "no files found" )
  }
  
  # the words could come in any order, check through each part
  files = files[grepl("friendly", files, ignore.case = TRUE) | grepl("ufb", files, ignore.case = TRUE)]
  files = files[grepl("2019", files)]
  if( !any(files) )
  {
        return("no files found")
  } else
  {
    return(files)
  }
}
# ------------------------------------------------------------------------------------------- #


# ------------------------------------ getLoc ---------------------------------------------------- #
# get index of element containing "official"- this is where the town
# url is located
getLoc <- function( lst.item )
{
  # regular expression enforce capitalization which i dnt want
  # can probably make this more efficient though
  # list of terms that the town website may be classified under
  search.list <- c("homepage", "official", "site", "website", 
                   "web site", "city of", "nj.com", "nj.gov",
                   "nj.us")
  
  for( i in search.list)
  {
    ind = grepl(i, lst.item[names(lst.item)], ignore.case = T)
    if(any(ind))
    {
      return(ind)
    }
  }
  
  return(NULL)
}
# ------------------------------------------------------------------------------------------- #
# =========================================================================================== #




# ========================================================================================= #
# ============================= main ====================================================== #
# ========================================================================================= #
# load in the list of municipalites from wikipedia
wiki.url <- "https://en.wikipedia.org/wiki/List_of_municipalities_in_New_Jersey"
wdat <- read_html(wiki.url)
wdat <- wdat %>% html_nodes("tbody")
wdat <- wdat[1]
wiki.nodes <- wdat %>% html_nodes("a") %>% html_attr("href")

twn.links <- wiki.nodes[ grepl("._New_Jersey$", wiki.nodes)]

# need to screen out county and township
twn.links <- twn.links[!grepl("Township", twn.links)]
twn.links <- twn.links[!grepl("County", twn.links)]

twn.names <- unlist(lapply(strsplit(twn.links, "[/]"), function(x) x[3]))
twn.names <- unlist(lapply(strsplit(twn.links, ","), function(x) x[1]))

# bernardsville, linden, clark should all have ufb

# get each of the town's individual wiki pages
twn.wiki.urls <- unique(paste0("https://en.wikipedia.org", twn.links))



for( i in 1:length(twn.wiki.urls))
{
  
  wiki.town <- twn.wiki.urls[i]
  twn.wiki.url <- getTownURL(twn.wiki.urls[i])
  file <- readTownSite( twn.wiki.url)
  print(paste0(i, ": ", twn.wiki.url, ", ", file))
  
}


twn.wiki.dat <- read_html(twn.wiki.url)
twn.wiki.nodes <- twn.wiki.dat %>% html_nodes("a") %>% html_attr("href")

twn.wiki.tmp <- twn.wiki.dat %>% html_nodes("li") 
x <- as_list(twn.wiki.tmp[grepl("Official", twn.wiki.tmp)])
twn.url <- attributes(x[[1]]$span$span$a)$href

url <- "http://www.vernontwp.com/"
dat <- read_html(url)

body_nodes <- dat %>% html_nodes("a") %>% html_attr("href")



ind <- grepl("*.pdf", body_nodes) | grepl("*.xls", body_nodes) | grepl("*.xlsx", body_nodes)
files <- body_nodes[ind]

ufbs <- files[grepl(".ufb.", files) | grepl("Friendly", files, ignore.case = TRUE)]
ufbs2019 <- ufbs[grepl(".19.", ufbs)]


if( length(ufbs2019) > 0 )
{
  print("ufb found")
  print(ufbs2019)
}
# ========================================================================================= #
