library(tidytext)
library(tidyverse)
library(XML)
library(dplyr)
library(purrr)
library(stringr)
library(betacode)


##get perseus XML of Argo

url <- "XML/Argo_perseus_fixed.xml"
doc_p <- xmlTreeParse(url, useInternalNodes = T)

# get rootnode
rootnode <- xmlRoot(doc_p)
ns="http://www.tei-c.org/ns/1.0"

# get books
book_nodes <- getNodeSet(rootnode, 
                         "//tei:div[@subtype='book']", 
                         namespaces = c(tei = ns))

# get line function
get_line_perseus <- function(book_node) {
  book_nr <- xmlGetAttr(book_node, "n")
  line_nodes <- xmlElementsByTagName(book_node, "l", recursive = T)
  lines <- map_chr(line_nodes, xmlValue)
  tbl <- tibble(title = "Argo", book = as.numeric(book_nr), 
                line = lines)
}

Argo_pers <- map_df(book_nodes, get_line_perseus)


##Разделяем стихи на отдельные слова

Argo_pers <- Argo_pers %>%
  group_by(book) %>% 
  mutate(line_nr = row_number()) %>% 
  ungroup()

Argo_pers_tokens <- Argo_pers %>%
  mutate(line = str_replace_all(line, "·", " ")) %>% 
  unnest_tokens("word", "line")


##get dior XML of Argo

doc_d <- xmlTreeParse('XML/Argo_dior_fixed.xml', useInternalNodes = T)

# get rootnode
rootnode <- xmlRoot(doc_d)
ns="http://www.tei-c.org/ns/1.0"

# get lemma nodes
word_nodes <- getNodeSet(rootnode, 
                         "//word", 
                         namespaces = c(tei = ns))

word = map(word_nodes, xmlGetAttr, "form") %>% as.character()
location <- map(word_nodes, ~xmlGetAttr(xmlParent(.), "location")) %>% 
  as.character()


lemma_nodes <- map(word_nodes, ~xmlChildren(.)$lemma)
pos <-  map(lemma_nodes, xmlGetAttr, "POS") %>% 
  as.character()
entry <- map(lemma_nodes, xmlGetAttr, "entry") %>% 
  as.character()

Argo_dior <- tibble(
  location = location,
  word = word, 
  entry = entry, 
  pos = pos
)


##Разделяем слепившиеся со словами частицы в таблице Диорисиса

# substitute
ge_entries <-  Argo_dior %>% 
  filter(str_detect(word, "ge$"), pos == "NULL") %>% 
  distinct(word) %>% 
  pull(word)

g_entries <- Argo_dior %>% 
  filter(str_detect(word, "g'$"), pos == "NULL") %>% 
  distinct(word) %>% 
  pull(word)

length(c(ge_entries, g_entries))


# замена
article_ge_g <- c("h(/ge",   "th/nge", "ta/ge", "tou/sge", 
                  "to/nge", "ai(/ge",  "tw/ge",   "to/ge", 
                  "o(/sge", "ta/sge",  "tw=|ge",  "th=sge",
                  "toi/ge", "tai/ge", "to/ng'",  "to/g'", 
                  "toi/g'",  "tou/sg'", "tw/g'", "th/ng'",
                  "ai(/g'")
pronoun_ge_g <- c("soi/ge", "se/ge", "soi/g'")

length(c(article_ge_g, pronoun_ge_g))

# mutate
Argo_dior_corr <- Argo_dior %>% 
  mutate(pos = case_when(word %in% article_ge_g ~ "article_particle",
                         word %in% pronoun_ge_g ~ "pronoun_particle",
                         .default = pos),
  ) %>% 
  separate_longer_delim(pos, delim = "_")


##добавляем проставленные вручную части речи

corrected_pos <- read_delim("Argo_added_POS.txt", delim = ";") %>%
  select(-n) %>%
  rename(word_u = word, pos_corr = pos)

Argo_dior_corr2 <- Argo_dior_corr %>% 
  mutate(word_u = betacode_to_unicode(Argo_dior_corr$word, strict = F) )

Argo_dior_corr3 <- Argo_dior_corr2 %>%
  left_join(corrected_pos)

Argo_dior_corr4 <- Argo_dior_corr3 %>%
  mutate(pos = case_when(pos == "NULL" ~ pos_corr, .default = pos)) %>%
  select(-word_u, -pos_corr)

##Соединяем две таблицы и избавляемся от NA-строк
Argo_joined <- Argo_pers_tokens %>%
  unite(line, c("book", "line_nr")) %>%
  mutate(pos = Argo_dior_corr4$pos,
         word_d = Argo_dior_corr4$word) %>%
  mutate(pos = case_when(is.na(pos) ~ "NULL", .default = pos))

##get part of speech

Argo_pos <- Argo_joined %>%
  group_by(line) %>%
  mutate(pos_line = str_c(pos, collapse = " ")) %>%
  distinct(title, line, pos_line)

