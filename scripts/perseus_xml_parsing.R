library(XML)
library(dplyr)
library(purrr)
library(stringr)

url <- "XML/Iliad_perseus_fixed.xml"
doc_p <- xmlTreeParse(url, useInternalNodes = T)

# get rootnode
rootnode <- xmlRoot(doc_p)
ns="http://www.tei-c.org/ns/1.0"

# get books
book_nodes <- getNodeSet(rootnode, 
                         "//tei:div[@subtype='Book']", 
                         namespaces = c(tei = ns))

# get line function
get_line_perseus <- function(book_node) {
  book_nr <- xmlGetAttr(book_node, "n")
  line_nodes <- xmlElementsByTagName(book_node, "l", recursive = T)
  lines <- map_chr(line_nodes, xmlValue)
  tbl <- tibble(title = "Iliad", book = as.numeric(book_nr), 
                line = lines)
}

# iterate
iliad_pers <- map_df(book_nodes, get_line_perseus)

save(iliad_pers, file = "./data/iliad_pers.Rdata")
