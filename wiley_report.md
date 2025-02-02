Wiley Approach
================
Najko Jahn
10/28/2019

Required R libraries
--------------------

``` r
library(tidyverse) # collection of packages, see also <https://r4ds.had.co.nz/>
```

    ## ── Attaching packages ──────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.3
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ─────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(jsonlite) # json tools to import and export nested data
```

    ## 
    ## Attaching package: 'jsonlite'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten

``` r
library(rcrossref) # interface to Crossref API
library(stringi) # string manipulation
```

Journal information
-------------------

The starting point for the ESAC market watch report is to obtain information about the journal portfolio of a publisher. Many publishers provide detailled information about their portfolio.

Information about Wiley open access options can be found here:

<https://authorservices.wiley.com/author-resources/Journal-Authors/open-access/article-publication-charges.html>

-   fully open access <https://authorservices.wiley.com/asset/Wiley-Journal-APCs-Open-Access.xlsx>
-   hybrid open access <https://authorservices.wiley.com/asset/Wiley-Journal-APCs-OnlineOpen.xlsx>

Download the files:

``` r
download.file(url = "https://authorservices.wiley.com/asset/Wiley-Journal-APCs-Open-Access.xlsx", 
              destfile = "data/wiley_full_oa_journal_list.xlsx")

download.file(url = "https://authorservices.wiley.com/asset/Wiley-Journal-APCs-OnlineOpen.xlsx", 
              destfile = "data/wiley_hybrid_oa_journal_list.xlsx")
```

Prepare the journal list:

``` r
full_oa <- readxl::read_xlsx("data/wiley_full_oa_journal_list.xlsx", skip = 3)
```

    ## New names:
    ## * `` -> ...6
    ## * `` -> ...7
    ## * `` -> ...9
    ## * `` -> ...10
    ## * `` -> ...12
    ## * … and 1 more problem

``` r
full_oa_short <- full_oa %>%
  select(journal_title = Journal, jn_code = `Journal Code`, e_issn = `Online ISSN`) %>%
  mutate(oa_mode = "full oa") %>%
  filter(!is.na(journal_title)) 
```

``` r
hybrid_oa <- readxl::read_xlsx("data/wiley_hybrid_oa_journal_list.xlsx", skip = 3)
```

    ## New names:
    ## * `` -> ...5
    ## * `` -> ...6
    ## * `` -> ...8
    ## * `` -> ...9

``` r
hybrid_oa_short <- hybrid_oa %>% 
  select(journal_title = `Journal Title`, jn_code = `Journal Code`, e_issn = `Online\r\nISSN`) %>%
  mutate(oa_mode = "hybrid oa") %>%
  filter(!is.na(jn_code)) 
jn_wiley <- bind_rows(hybrid_oa_short, full_oa_short)
```

Wiley journal list contains just Online ISSN. To increase the recall when calling the Crossref API by ISSN, we add other ISSN variants to the journal dataset from the ISSN-L journal list.

<https://www.issn.org/services/online-services/access-to-issn-l-table/>

Download: <http://www.issn.org/wp-content/uploads/2014/03/issnltables.zip>

``` r
issn_l <- readr::read_tsv("data/20200119.ISSN-to-ISSN-L.txt")
```

    ## Parsed with column specification:
    ## cols(
    ##   ISSN = col_character(),
    ##   `ISSN-L` = col_character()
    ## )

``` r
issn_match <- jn_wiley %>%
  left_join(issn_l, by = c("e_issn" = "ISSN")) 
issn_variants <-
  issn_match %>% 
  left_join(issn_l, by = c("ISSN-L")) %>%
  distinct(jn_code, e_issn, `ISSN-L`, ISSN) %>%
  gather(e_issn, `ISSN-L`, ISSN, key = "issn_type", value = issn) %>%
  distinct()
```

Per every ISSN-L, obtain all ISSN variants and prepare Crossref calls

### Publication volume per journal from Crossref

Crossref is a DOI registration agency for scholarly works. It not only mints DOIs for journal articles, but also provides metadata, which makes it a great data source for the ESAC market watch report.

Its API, which can be accessed with the R package [`rcrossref`](https://cran.r-project.org/package=rcrossref), returns article-level metadata and [facet counts](https://github.com/CrossRef/rest-api-doc#facet-counts) for a specific query. In the following, we will make use of facet counts to obtain

-   the yearly publication volume per journal
-   and licence URLs

by all ISSNs per journal.

First, we create a list wit hqueries that we want to send to Crossref

``` r
issns_list <-
  purrr::map(unique(issn_variants$jn_code), function(x) {
    jn <- x
    issns <- issn_variants %>%
      filter(jn_code == jn) %>%
      distinct(issn) %>%
      .$issn
    names(issns) <- rep("issn", length(issns))
    issns
  })
```

Next, we call the Crossref API.

**Note that we just query 10 journals for the sake of this demonstration. Calling Crossref API for around 2000 journals takes around 1-2 hours if you have registered for the polite pool: <https://github.com/ropensci/rcrossref#register-for-the-polite-pool>**

``` r
jn_facets <- purrr::map(issns_list, .f = purrr::safely(function(x) {
  tt <- rcrossref::cr_works(
    filter = c(
      x,
      from_pub_date = "2015-01-01",
      until_pub_date = "2019-12-31",
      type = "journal-article"
    ),
    # being explicit about facets improves API performance!
    facet = "license:*,published:*",
    # less api traffic
    select = "DOI"
  )
  #' Parse the relevant information
  #' - `issn` - issns  found in open apc data set
  #' - `year_published` - published volume per year (Earliest year of publication)
  #' - `license_refs` - facet counts for license URIs of work
  if (!is.null(tt)) {
    tibble::tibble(
      issn = list(x),
      year_published = list(tt$facets$published),
      license_refs = list(tt$facets$license)
    )
  } else {
    NULL
  }
}))
```

`purrr::safely()` was used to catch potential errors. To get the publication volume:

``` r
jn_df <- jn_facets %>%
  map_df("result")
```

Publication volume

``` r
pub_volume <- jn_df %>% 
  select(-license_refs) %>%
  unnest(year_published) %>% 
  unnest(issn) %>%
  inner_join(issn_variants, by = "issn") %>%
  select(year = .id, articles = V1, jn_code) %>%
  distinct() %>%
  inner_join(jn_wiley, by = "jn_code")
pub_volume %>%
  head()
```

    ## # A tibble: 6 x 6
    ##   year  articles jn_code journal_title               e_issn    oa_mode  
    ##   <chr>    <int> <chr>   <chr>                       <chr>     <chr>    
    ## 1 2019        36 ABAC    Abacus                      1467-6281 hybrid oa
    ## 2 2016        33 ABAC    Abacus                      1467-6281 hybrid oa
    ## 3 2017        25 ABAC    Abacus                      1467-6281 hybrid oa
    ## 4 2015        22 ABAC    Abacus                      1467-6281 hybrid oa
    ## 5 2018        20 ABAC    Abacus                      1467-6281 hybrid oa
    ## 6 2015       264 ACEM    Academic Emergency Medicine 1553-2712 hybrid oa

### Open Access articles

While we can infer that all articles from fully open access journal are openly available, identifying open access articles in subscription journals, i.e. hybrid open access, requires another step.

Hybrid open access articles are made available under an open content license like Creative Commons.

First, we check, if a subscription journal publsihed articles under an Creative Commons license

``` r
license_jns <- jn_df %>% 
  select(-year_published) %>%
  unnest(license_refs) %>% 
  unnest(issn) %>%
    inner_join(issn_variants, by = "issn") %>%
  select(license_ref = .id, articles = V1, jn_code, issn) %>%
  distinct() %>%
  inner_join(jn_wiley, by = "jn_code") %>%
  mutate(hybrid_license = ifelse(grepl(
    "creativecommons",
    license_ref
  ), TRUE, FALSE))  %>%
  filter(hybrid_license == TRUE, oa_mode == "hybrid oa") %>%
  nest(issn = c(issn))
```

We now know, whether and which open licenses were used by the journal. As a next step we want to validate that these licenses were not issued for delayed open access articles by additionally using the self-explanatory filter `license.url` and `license.delay`. We also obtain metadata for these hybrid open access articles stored as list-column. Parsed metadata fields are defined in `cr_md_fields`

``` r
cr_md_fields <- c("URL", "member", "created", "license", 
                   "container-title", "issued", "approved", 
                  "indexed", "accepted", "DOI", "funder", "published-print", 
                  "subject", "published-online", "link", "type", "publisher", 
                  "issn-type", "deposited", "content-created")
cr_license <- purrr::map2(license_jns$license_ref, license_jns$issn,
                          .f = purrr::safely(function(x, y) {
                            u <- x
                            issn <- unlist(y)
                            names(issn) <-rep("issn", length(issn))
                            tmp <- rcrossref::cr_works(filter = c(issn, 
                                                                  license.url = u, 
                                                                  type = "journal-article",
                                                                  from_pub_date = "2015-01-01", 
                                                                  until_pub_date = "2019-12-31"),
                                                       cursor = "*", cursor_max = 5000L, 
                                                       limit = 1000L,
                                                       select = cr_md_fields) 
                            tibble::tibble(
                              issns =  list(issn),
                              license_ref = u,
                              md = list(tmp$data)
                            )
                          }))
#' into one data frame!
cr_license_df <- cr_license %>% 
  purrr::map_df("result") 
#' export results as nested json. For re-importing it into R, use the stream_in function
dplyr::bind_rows(cr_license_df) %>% 
  jsonlite::stream_out(file("data/hybrid_license_md.json"))
```

    ## opening file output connection.

    ## 
    Processed 500 rows...
    Processed 1000 rows...
    Processed 1500 rows...
    Processed 2000 rows...
    Processed 2500 rows...
    Processed 3000 rows...
    Complete! Processed total of 3220 rows.

    ## closing file output connection.

Exclude delayed OA

``` r
#' get delayed oa  articles (<= 31 days)
immediate_dois <- cr_license_df %>%
  unnest(md) %>% 
  select(doi, license) %>% 
  unnest(license) %>% 
  filter(grepl("creativecommons", URL), delay.in.days <= 31)
```

Some data transformation to obtain hybrid oa figures per journal and year from the article-level data.

``` r
license_df <- cr_license_df %>% 
  unnest(md) %>% 
  filter(doi %in% immediate_dois$doi) %>% # only immediate DOIs
  unnest(issns) %>%
  inner_join(license_jns %>% unnest(issn), by = c("issns" = "issn")) %>%
  mutate(year = str_extract(issued, "[0-9]{4}")) %>%
  group_by(year, jn_code) %>%
  summarise(hybrid_oa_articles = n_distinct(doi))
license_df %>%
  group_by(year) %>%
  summarise(n = sum(hybrid_oa_articles))
```

    ## # A tibble: 5 x 2
    ##   year      n
    ##   <chr> <int>
    ## 1 2015   1694
    ## 2 2016   4102
    ## 3 2017   7434
    ## 4 2018  10215
    ## 5 2019  13233

Finally, add the overall publication volume and export

``` r
inner_join(license_df, pub_volume, by = c("jn_code", "year")) %>%
  write_csv("data/wiley_hybrid_final.csv")
```
