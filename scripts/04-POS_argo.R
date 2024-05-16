load("./data/Argo_dior_tokens.Rdata")
load("./data/Argo_pers_tokens.Rdata")


#get rid of NA-lines in Argonautica

x <- c("a", "b", NA)
str_c(x, collapse = " ")

argo_joined <- pers_tokens %>%
  unite(line, c("book", "line_nr")) %>%
  mutate(pos = Argo_fixed$pos) %>%
  mutate(pos = case_when(is.na(pos) ~ "NULL", .default = pos))

#part os speech in Argo

argo_pos_lines <- argo_joined %>%
  group_by(line) %>%
  mutate(pos_line = str_c(pos, collapse = " ")) %>%
  distinct(title, line, pos_line)