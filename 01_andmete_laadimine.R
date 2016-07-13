# Lae R2 aastahiti tulemused ja laulusõnad

library(rvest)
library(dplyr)
library(stringr)
library(purrr)

# lae veebist kraabitud andmed
# load("output/andmete_laadimine.RData")

# R2 aastahiti nimekirja url-id 1994 - 2015
r2_aastahitt_url <- str_c("http://r2.err.ee/aastahitt/ajalugureg.php?aasta=", 
      seq(from = 1994, to = 2015, by = 1))

# funktsioon laeb aastahiti tabeli ühe aasta kohta
aastahitid <- function(x){
    r2_aastahitt <- read_html(x, encoding = "UTF-8")
    
    lood <- r2_aastahitt %>%
        html_nodes("td:nth-child(1) td+ td") %>%
        html_text() %>%
        repair_encoding() %>%
        data_frame() %>%
        mutate(url = x)
        
    return(lood)
}

# kõik aastahiti tabelid ühte tabelisse kokku
r2_aastahitid_raw <- map_df(r2_aastahitt_url, aastahitid)

# lugude nimede töötlus
r2_aastahitid <- r2_aastahitid_raw %>%
    mutate(aasta = str_sub(url, start = -4)) %>%
    select(lugu = 1, aasta) %>%
    group_by(aasta) %>%
    mutate(koht = row_number(),  # loo koht aasta tabelis
           lugu_toodeldud = ifelse(str_detect(lugu, "^-"), 
                                   str_replace(lugu, "^-[0-9]{2}.", ""), lugu),
           lugu_toodeldud = str_replace(lugu_toodeldud, "Vaiko Eplik Ja Eliit",
                                        "Vaiko Eplik"),
           # vastuste arv
           vastanuid = str_extract_all(lugu, "[^\\(] *([0-9]{1,4}) *\\)$"),
           vastanuid = as.numeric(str_replace(vastanuid, "\\)", "")),
           # lugude pealkirjade töötlemine
           lugu_toodeldud = str_replace_all(lugu_toodeldud, "\\( *[0-9]{2,4} *\\)$", ""),
           lugu_toodeldud = str_trim(lugu_toodeldud),
           # pealkirjade puhastamine ja sellest omakorda sõnade kraapimiseks url
           lugu_url = str_to_lower(lugu_toodeldud),
           lugu_url = str_trim(lugu_url),
           lugu_url = str_replace_all(lugu_url, " ", "_"),
           lugu_url = str_replace_all(lugu_url, "_-_", "-"),
           lugu_url = str_replace_all(lugu_url, "ö", "o"),
           lugu_url = str_replace_all(lugu_url, "õ", "o"),
           lugu_url = str_replace_all(lugu_url, "ä", "a"),
           lugu_url = str_replace_all(lugu_url, "ü", "u"),
           lugu_url = str_replace_all(lugu_url, "\\.", "_"),
           lugu_url = str_replace_all(lugu_url, "\\@_the_sun", ""),
           lugu_url = str_replace_all(lugu_url, "\\&_the_sun", ""),
           lugu_url = str_replace_all(lugu_url, "_-", "-"),
           lugu_url = str_replace_all(lugu_url, "__", "_"),
           lugu_url = str_replace_all(lugu_url, "_&", ""),
           lugu_url = str_replace_all(lugu_url, ",|\\?|\\!|'", ""),
           lugu_url_2 = str_c("http://sasslantis.ee/lyrics-", lugu_url),
           artist = str_extract(lugu_toodeldud, "^.*(?=( - ))"),
           artist = ifelse(str_detect(artist, "Terminaator"), "Terminaator", artist),
           artist = str_trim(artist),
           track = str_extract(lugu_toodeldud, "( - ).*$"),
           track = str_replace(track, " - ", ""),
           track = str_trim(track)) %>%
    ungroup() %>%
    select(aasta, koht, vastanuid, lugu, lugu_toodeldud, artist, track,
           lugu_url, lugu_url_2)

# funktsioon kraabib url laulusõnad ja paneb tulemused ühte tabelisse kokku
laulusonad <- function(x){
    tryCatch(
        {
            Sys.sleep(sample(seq(1, 3, by = 0.001), 1))  # random paus 1-3 sek
            sonad <- read_html(x, encoding = "UTF-8")
            sonad %>%
                html_nodes("style+ table") %>%
                html_text() %>%
                repair_encoding() %>%  # paranda utf-8 encoding
                gsub("\\/\\*.*$", "", .) %>%  # kustuta üleliigne tekst pärast sõnasid
                gsub("^.*\\*", "", .) %>%  # kustuta üleliigne tekst enne sõnasid
                gsub("^.*            \n            \n            ", "", .) %>%
                data_frame() %>%
                mutate(url = x)
        }, error = function(e) NULL  # kui link ei tööta, siis NULL väärtus
    )
}

# kraabi kõigi aastahiti lugude sõnad
aastahiti_sonad_raw <- map_df(r2_aastahitid %>%
                           # sample_n(3) %>%
                           .$lugu_url_2, laulusonad)

# sõnade tabeli väike töötlus
aastahiti_sonad <- aastahiti_sonad_raw %>%
    select(sonad = 1, url) %>%
    mutate(keel = ifelse(str_detect(str_to_lower(sonad), "ö|ü|õ|ä"), "eesti", "inglise"))

# lugude tabelile sõnad juurde
lood_sonadega <- r2_aastahitid %>%
    left_join(aastahiti_sonad, by = c("lugu_url_2" = "url")) %>%
    distinct()

# salvesta andmed
save(aastahiti_sonad_raw, aastahiti_sonad, r2_aastahitid, r2_aastahitid_raw, 
     lood_sonadega, file = "output/andmete_laadimine.RData")

# salvesta csv pythonis Eesti keele tekstianalüüsiks
lood_sonadega %>%
    write.csv2("output/lood_sonadega.csv", row.names = FALSE, fileEncoding = "utf-8")