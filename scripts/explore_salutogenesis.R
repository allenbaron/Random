# Quickly explore prior work in Salutogenesis
# Executed: 2021-07-07

library(tidyverse)
library(rentrez)
library(DO.utils)
library(tidytext)


# Identify relevant publications ------------------------------------------

pm_search <- rentrez::entrez_search(
    db = "pubmed",
    term = "salutogenesis OR salutogenic AND 2018:2021[PDAT]",
    use_history = TRUE
)



# Get summaries (incl.  titles) -------------------------------------------

dl_chunk_start <- seq(1, pm_search$count, by = 500)

pm_summary <- purrr::map(
    dl_chunk_start,
    ~ rentrez::entrez_summary(
        db = "pubmed",
        web_history = pm_search$web_history,
        retmax = 500,
        retstart = .x
    )
)

saveRDS(pm_summary, "data/salutogenesis_pm_summary.rds")

pm_summary <- unlist(pm_summary, recursive = FALSE)



# Get abstracts -----------------------------------------------------------

# see https://github.com/ropensci/rentrez/issues/100 for details on rettype
pm_abstract <- purrr::map(
    dl_chunk_start,
    ~ rentrez::entrez_fetch(
        db = "pubmed",
        web_history = pm_search$web_history,
        rettype = "abstract",
        retmax = 500,
        retstart = .x
    )
)

write_lines_with_divider <- function(char_vctr, header_txt, file) {
    # create section header
    section_header <- paste0(
        "# ", header_txt, " ",
        paste0(rep("-", 30), collapse = "")
    )
    
    # combine header with text
    section <- c(section_header, "", char_vctr, "", "")
    
    # write to file
    readr::write_lines(section, file, append = TRUE)
}

create_list_header <- function(i) {
    paste0("List item ", i)
}

write_abstract <- function(pubmed_abstract, file) {
    if (rlang::is_list(pubmed_abstract)) {
        i <- seq_along(pubmed_abstract)
        purrr::walk(
            i,
            ~ write_lines_with_divider(
                pubmed_abstract[[.x]],
                create_list_header(.x),
                file
            )
        )
    } else if (is.character(pubmed_abstract)) {
        readr::write_lines(pubmed_abstract, file)
    } else {
        stop("Not sure how to handle object of class ", class(pubmed_abstract))
    }
}

write_abstract(pm_abstract, "data/salutogenesis_2018-21_abstract.txt")

saveRDS(pm_abstract_xml, "data/salutogenesis_abstracts_xml.rds")


# Get relevant text for mining --------------------------------------------

pm_text <- purrr::map_dfr(
    pm_summary,
    tidy_pubmed_summary
)



# TF-IDF ------------------------------------------------------------------


