
library(jsonlite)
library(tidyverse)
library(bib2df)

theme_point<-theme_classic()+theme(strip.background = element_blank())

theme_bar<-theme_classic()+theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),strip.background = element_blank(),axis.ticks.x = element_blank(),axis.line.x = element_blank())

theme_boxplot<-theme_classic()+theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),strip.background = element_blank(),axis.ticks.x = element_blank(),axis.title.x = element_blank(),axis.line.x = element_blank(),legend.position = "none")


baseurl <- "https://api.biorxiv.org/covid19/"
pages <- list()
for(i in seq(from = 0, to = 7607, by = 30)){
  json_data <- fromJSON(paste0(baseurl, i))
  message("Retrieving page ", i)
  pages[[i+1]] <- json_data$collection
}
preprint_papers <- rbind_pages(pages)
preprint_papers <- as_tibble(preprint_papers)

ggplot(preprint_papers, aes(x = as.Date(rel_date), fill = rel_site)) + geom_bar() + theme_point + labs(title = "all covid papers")

search_term <- "^PIMS$|^MIS$|^multisystemic inflammat$|^inflammatory disease$|^systemic inflammat$|^cytokine release$|^kawasaki$|^vasculitis$|^toxic shock$|^shock$"

search_term_a <- "(PIMS|MIS|multisystemic inflammat|inflammatory disease|systemic inflammat|cytokine release|kawasaki|vasculitis|toxic shock|shock)"
search_term_b <- "(child|adolescen|teen|pediatric|infant|newborn)"

abs <- preprint_papers %>% filter(str_detect(rel_abs, regex(search_term_a, ignore_case = TRUE)) & str_detect(rel_abs, regex(search_term_b, ignore_case = TRUE)))
titles <- preprint_papers %>% filter(str_detect(rel_title, regex(search_term_a, ignore_case = TRUE)) & str_detect(rel_abs, regex(search_term_b, ignore_case = TRUE)))

full <- rbind(abs,titles)

full <- full %>% distinct(rel_doi, .keep_all = TRUE)
full <- full %>% select(-c(rel_num_authors, rel_link, version, license, category, rel_abs, type))
colnames(full) <- c("DOI", "TITLE", "DATE", "JOURNAL", "AUTHOR")

full <- full %>% unnest(AUTHOR) 
full <- full[!duplicated(full$TITLE),]
full$FIRST_AUTHOR <- sapply(strsplit(as.character(full$author_name), " "), tail, 1)
full$DATE <- as.Date(full$DATE)

ggplot(full, aes(x = as.Date(DATE), fill = JOURNAL)) + geom_bar() + theme_point + labs(title = "PIMS papers")

df2bib(full, file = "20200814_covid_full.bib")
