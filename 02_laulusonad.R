# Aastahiti tulemuste visualiseerimine ja anlüüs

library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(tidyr)
library(wordcloud)
library(ggplot2)
library(ggrepel)
library(directlabels)
library(zoo)
library(extrafont)
library(viridis)
library(ggthemes)

# lae veebist kraabitud andmed
load("output/andmete_laadimine.RData")

# lae pythonis eeltöödeldud andmed
# tehtud tekstianalüüs, millega tuvastatud iga laulu üksikud sõnad
laulusonad_lemmadega <- read_csv("python/laulusonad_lemmadega.csv")

# lemmadega laulusõnad ja laulude metaandmed ühes tabelis koos
laulusonad_kokku <- laulusonad_lemmadega %>%
    mutate(sonad = str_to_lower(str_extract(sonad, "[A-Za-z].*")),
           sonad = str_trim(sonad)) %>%
    left_join(lood_sonadega %>%
                  mutate(sidumiseks = str_sub(sonad, start = 1, end = 10),
                         sidumiseks = str_to_lower(str_extract(sidumiseks, "[A-z].*")),
                         sidumiseks = str_trim(sidumiseks)) %>%
                  filter(!is.na(sidumiseks)),
              by = c("sonad" = "sidumiseks"))

# visualiseerimiseks ainult teatud sõnad
laulusonad_sonapilveks <- laulusonad_kokku %>%
    filter(keel == "eesti", 
           !postag_descriptions %in% c("lausemärk", "sidesõna"),
           !str_detect(lemmas, "[0-9]"),
           (postag_descriptions %in% c("nimisõna", "tegusõna") | 
            str_detect(postag_descriptions, "omaduss"))) %>%
    # jaga andmed visualiseerimiseks kahte perioodi
    mutate(aasta_grupp = cut(as.numeric(aasta), breaks = c(-Inf, 2005, 2015), 
                             labels = c("1994-2005", "2006-2015"))) %>%
    group_by(lugu) %>%
    distinct(lemmas, .keep_all = TRUE) %>%
    ungroup()

# nimisõnade esinemissagedus erinevated lauludes
sonapilv_nimisonad <- laulusonad_sonapilveks %>%
    group_by(lemmas, postag_descriptions) %>%
    tally() %>%
    ungroup() %>%
    filter(postag_descriptions == "nimisõna")

# nimisõnade sõnapilv    
wordcloud(sonapilv_nimisonad$lemmas, sonapilv_nimisonad$n, scale = c(4, 0.2),
          min.freq = 5, max.words = 200, rot.per = 0.2,
          random.order = FALSE)

# omadussõnade esinemissagedus erinevates lauludes
sonapilv_omadussonad <- laulusonad_sonapilveks %>%
    group_by(lemmas, postag_descriptions) %>%
    tally() %>%
    ungroup() %>%
    filter(!postag_descriptions %in% c("nimisõna", "tegusõna"))

# omadussõnade sõnapilv
wordcloud(sonapilv_omadussonad$lemmas, sonapilv_omadussonad$n, scale = c(4, 0.2),
          min.freq = 5, max.words = 200, rot.per = 0.2,
          random.order = FALSE)

# top 20 omadus- ja tegusõna 20 aasta jooksul
top_20_sona <- laulusonad_sonapilveks %>%
    filter(!postag_descriptions %in% c("nimisõna", "tegusõna") |
               postag_descriptions == "nimisõna") %>%
    group_by(lemmas) %>%
    tally() %>%
    arrange(desc(n)) %>%
    head(20) %>%
    .$lemmas

# 1994-20015 vs 2006-2015 top esinemissagedusega laulusõnad järjekorras
laulusonade_bumpchart <- laulusonad_sonapilveks %>%
    filter(lemmas %in% c(top_20_sona)) %>%
    group_by(lemmas, aasta_grupp) %>%
    tally() %>%
    ungroup() %>%
    group_by(aasta_grupp) %>%
    arrange(n) %>%
    mutate(jrk = row_number()) %>%
    ungroup() %>%
    group_by(lemmas) %>%
    arrange(aasta_grupp)

# graafikul top laulusõnade võrdlus
bump <- laulusonade_bumpchart %>%
    ggplot(aes(x = aasta_grupp, y = jrk, color = lemmas, group = lemmas)) +
    geom_line(size = 1.2) +
    labs(title = "Populaarsemad sõnad lauludes 1994-2005 vs 2006-2015",
         subtitle = "Top 20 omadus- ja tegusõna on järjestatud selle järgi, mitmes erinevas laulus see esineb") +
    coord_cartesian(xlim = c(1.45, 1.55)) +
    theme_classic() +
    scale_y_continuous(breaks = c(1, 5, 10, 15, 20), 
                       labels = c("20", "15", "10", "5", "1")) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank()) 

# lisa sõnade labelid
bump %>% 
    direct.label(method = "first.qp") %>%
    direct.label(method = "last.qp")

# laulude lõikes unikaalsete eestikeelsete sõnade arv
laulusonade_arv <- laulusonad_kokku %>%
    filter(keel == "eesti", 
           postag_descriptions !="lausemärk",
           !str_detect(lemmas, "[0-9]"),
           # välista poolikute sõnadega laul
           lugu_toodeldud != "Chalice - Mulle meeldib see") %>%
    group_by(aasta, lugu_toodeldud) %>%
    summarise(sonu = n_distinct(lemmas)) %>%
    ungroup() %>%
    arrange(desc(sonu)) %>%
    mutate(max_min = ifelse(row_number() <= 3, "max", 
                            ifelse(row_number() == 446, "min", "muu")))

# laulud, mille son kõige rohkem/vähem unikaalseid sõnu
laul_max_min_sonadega <- bind_rows(head(laulusonade_arv, 3), 
                                   tail(laulusonade_arv, 1))

# graafik unikaalsete sõnade arvu kohta lauludes
laulusonade_arv %>%
    ggplot(aes(x = aasta, y = sonu, group = max_min, color = max_min)) +
    # geom_boxplot(colour = "Red", alpha = 0.5) +
    # geom_point(colour = "Red", alpha = 0.5) +
    geom_jitter(alpha = 0.4, size = 3, shape = 16) +
    geom_text_repel(data = laul_max_min_sonadega, 
                    aes(x = aasta, y = sonu, 
                        label = str_c(lugu_toodeldud, " (", sonu, " sõna)", sep = "")),
                    size = 2.5, color = "Black") +
    scale_x_discrete(breaks = c(1995, 2000, 2005, 2010, 2015)) +
    scale_color_manual(values = c("#e34a33", "#e34a33", "#2b8cbe")) +
    labs(title = "Unikaalsete sõnade arv lauludes",
         subtitle = "Iga punkt tähistab ühte laulu") +
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")

# top 25 artisti laulude arvu järgi edetabelis
top_artistid <- lood_sonadega %>%
    group_by(artist) %>%
    summarise(n = n_distinct(str_to_lower(track))) %>%
    arrange(desc(n)) %>%
    head(25)

# heatmapi tegemiseks andmetöötlus
heat <- lood_sonadega %>%
    inner_join(top_artistid %>% select(artist)) %>%
    distinct(artist, track, .keep_all = TRUE) %>%
    group_by(artist, aasta) %>%
    tally() %>%
    ungroup() %>%
    complete(artist, aasta, fill = list(n = 0)) %>%
    group_by(artist) %>%
    mutate(n_kokku = sum(n)) %>%
    ungroup() %>%
    mutate(artist = str_c(artist, " (", n_kokku, ")", sep =""),
           artist = as.factor(artist),
           artist = reorder(artist, n_kokku))

# top40 laulud artistide ja aastate lõikes (hetamap)
ggplot(heat, aes(x = aasta, y = artist, fill = n)) + 
    geom_tile(color = "white", size = 0.1) +
    scale_fill_gradient(low = "white", high = "#2b8cbe") +
    coord_equal() +
    scale_x_discrete(breaks = c(1995, 2000, 2005, 2010, 2015)) +
    labs(title = "Top40 laulud artistide ja aastate lõikes",
         subtitle = "25 populaarsemat artisti, sulgudes edetabelis olevate laulude arv kokku",
         fill = "laule") +
    theme_tufte(base_family="Helvetica") +
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.title=element_text(size=8),
          legend.text=element_text(size=7))

# häälte arv kokku artistide lõikes
haalte_arv_kokku <- lood_sonadega %>%
    group_by(artist) %>%
    summarise(vastanuid = sum(vastanuid, na.rm = TRUE)) %>%
    arrange(desc(vastanuid)) %>%
    head(20) 

# häälte arv graafikul
haalte_arv_kokku %>%
    ggplot(aes(x = reorder(artist, vastanuid), y = vastanuid)) +
    geom_bar(stat = "identity", fill = "#2b8cbe", alpha = 0.5) +
    geom_text(data = head(haalte_arv_kokku, 5) , aes(x = reorder(artist, vastanuid), y = vastanuid, label = vastanuid),
              size = 3, hjust = -0.1) +
    scale_y_continuous(limits = c(0, 25000), 
                       breaks = seq(from = 0, to = 30000, by = 5000)) +
    coord_flip() +
    labs(title = "Häälte arv kokku artistide lõikes",
         subtitle = "Arvesse on võetud kogu 21-aastane periood",
         y = "hääli") +
    theme_tufte(base_family = "Lucida Sans") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_text(size = 10))