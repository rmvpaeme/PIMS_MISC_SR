library(jsonlite)
library(tidyverse)
library(bib2df)

theme_point<-theme_classic()+theme(strip.background = element_blank())

theme_bar<-theme_classic()+theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),strip.background = element_blank(),axis.ticks.x = element_blank(),axis.line.x = element_blank())

theme_boxplot<-theme_classic()+theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),strip.background = element_blank(),axis.ticks.x = element_blank(),axis.title.x = element_blank(),axis.line.x = element_blank(),legend.position = "none")

json <- fromJSON("20200630_collection_json.php.json")
# source https://connect.biorxiv.org/relate/content/181
json <- as_tibble(json[["rels"]])
ggplot(json, aes(x = as.Date(rel_date), fill = rel_site)) + geom_bar() + theme_point + labs(title = "all covid papers")

search_term <- "^PIMS$|^MIS$|^multisystemic inflammat$|^inflammatory disease$|^systemic inflammat$|^cytokine release$|^kawasaki$|^vasculitis$|^toxic shock$|^shock$"

search_term_a <- "(PIMS|MIS|multisystemic inflammat|inflammatory disease|systemic inflammat|cytokine release|kawasaki|vasculitis|toxic shock|shock)"
search_term_b <- "(child|adolescen|teen|pediatric|infant|newborn)"
  
abs <- json %>% filter(str_detect(rel_abs, regex(search_term_a, ignore_case = TRUE)) & str_detect(rel_abs, regex(search_term_b, ignore_case = TRUE)))
titles <- json %>% filter(str_detect(rel_title, regex(search_term_a, ignore_case = TRUE)) & str_detect(rel_abs, regex(search_term_b, ignore_case = TRUE)))

full <- rbind(abs,titles)

full <- full %>% distinct(rel_doi, .keep_all = TRUE)
full <- full %>% select(-rel_num_authors)
colnames(full) <- c("TITLE", "DOI", "NOTE", "ABSTRACT", "AUTHOR", "DATE", "JOURNAL")

full <- full %>% unnest(AUTHOR) 
full <- full[!duplicated(full$TITLE),]
full$FIRST_AUTHOR <- sapply(strsplit(as.character(full$author_name), " "), tail, 1)
full$DATE <- as.Date(full$DATE)

ggplot(full, aes(x = as.Date(DATE), fill = JOURNAL)) + geom_bar() + theme_point + labs(title = "PIMS papers")

df2bib(full, file = "20200630_covid_full.bib")
