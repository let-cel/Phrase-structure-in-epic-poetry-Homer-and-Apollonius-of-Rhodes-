library(XML)
library(dplyr)
library(purrr)

url <- "./XML/Iliad_dior.xml"
doc_d <- xmlTreeParse(url, useInternalNodes = T)

# get rootnode
rootnode <- xmlRoot(doc_d)
ns="http://www.tei-c.org/ns/1.0"

# get lemma nodes
word_nodes <- getNodeSet(rootnode, 
                         "//word", 
                         namespaces = c(tei = ns))

word <-  map(word_nodes, xmlGetAttr, "form") %>% 
  as.character()
location <- map(word_nodes, ~xmlGetAttr(xmlParent(.), "location")) %>% 
  as.character()

lemma_nodes <- map(word_nodes, ~xmlChildren(.)$lemma)
pos <-  map(lemma_nodes, xmlGetAttr, "POS") %>% as.character()
entry <- map(lemma_nodes, xmlGetAttr, "entry") %>% as.character()

iliad_dior <- tibble(
  location = location,
  word = word, 
  entry = entry, 
  pos = pos
)

save(iliad_dior, file = "./data/iliad_dior.Rdata")
