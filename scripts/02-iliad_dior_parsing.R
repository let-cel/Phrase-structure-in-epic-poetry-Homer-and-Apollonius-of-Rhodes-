library(XML)
library(dplyr)
library(purrr)
library(stringr)

url <- "./XML/Iliad_dior.xml"
doc_d <- xmlTreeParse(url, useInternalNodes = T)

# get rootnode
rootnode <- xmlRoot(doc_d)
ns="http://www.tei-c.org/ns/1.0"

#  words & locations
word_nodes <- getNodeSet(rootnode, 
                         "//word", 
                         namespaces = c(tei = ns))
word <-  map(word_nodes, xmlGetAttr, "form") %>% 
  as.character()
location <- map(word_nodes, ~xmlGetAttr(xmlParent(.), "location")) %>% 
  as.character()

#  lemmas & pos
lemma_nodes <- map(word_nodes, ~xmlChildren(.)$lemma)
pos <-  map(lemma_nodes, xmlGetAttr, "POS") %>% as.character()
entry <- map(lemma_nodes, xmlGetAttr, "entry") %>% as.character()

# for each lemma, get analysis (NB first if several)
analysis_nodes <- map(lemma_nodes, ~xmlChildren(.)$analysis)
analysis <- map(analysis_nodes, safely(xmlGetAttr), "morph")
analysis_result <- analysis |> 
  transpose() |> 
  pluck("result") |> 
  as.character()

# collect all in tibble
iliad_dior <- tibble(
  location = location,
  word = word, 
  entry = entry, 
  pos = pos, 
  analysis = analysis_result # bind analysis here
)

# substitute part for verb if applicable
iliad_dior <- iliad_dior |> 
  mutate(pos = case_when(pos == "verb" & str_detect(analysis, "part") ~ "part",
                                 .default = pos)) |> 
  select(-analysis)
  
# save 
save(iliad_dior, file = "./data/iliad_dior.Rdata")
