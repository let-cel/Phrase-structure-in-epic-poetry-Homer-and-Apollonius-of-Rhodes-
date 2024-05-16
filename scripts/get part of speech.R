load("./data/iliad_dior.Rdata")
load("./data/iliad_pers.Rdata")

library(tidytext)
library(tidyverse)

# tokenize perseus
iliad_pers <- iliad_pers %>%
  group_by(book) %>% 
  mutate(line_nr = row_number()) %>% 
  ungroup()

pers_tokens <- iliad_pers %>%
  mutate(line = str_replace_all(line, "·", " ")) %>% 
  unnest_tokens("word", "line")

# #perseus stat
# pers_stat <- pers_tokens %>%
#   group_by(book) %>%
#   summarise(n_pers = n()) %>%
#   rename(pers = book)
# 
# # diorisis stat
# dior_stat <- iliad_dior %>% 
#   mutate(book = as.numeric(str_remove(location, "\\.\\d{1,}"))) %>% 
#   group_by(book) %>%
#   summarise(n_dior = n()) %>%
#   rename(dior = book)

# side by side
# compare_data <- dior_stat %>%
#   bind_cols(pers_stat)

# join
iliad_joined <- pers_tokens %>% 
 mutate(pos = iliad_dior$pos,
        word_d = iliad_dior$word)

save(iliad_joined, file = "./data/iliad_joined.Rdata")

# collapse pos
iliad_pos <- iliad_joined %>% 
  group_by(book, line_nr) %>% 
  mutate(pos_line = str_c(pos, collapse = " ")) %>% 
  distinct(book, line_nr, pos_line)

save(iliad_pos, file = "./data/iliad_pos.Rdata")

# 
