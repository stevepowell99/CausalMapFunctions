---
title: "Causal Map Functions Vignette"
author: "Steve Powell"
date: "24/09/2021"
output:
  html_document:
    toc: true
    toc_float: true
    toc_collapsed: true
    toc_depth: 3
    number_sections: false
    theme: lumen
---




## Loading example datasets

(After loading CausalMapFunctions library)


The package ships with some example datasets, at the moment just these:

- example2
- quip_example

which you can also view in Causal Map on the web.

You can load the files like this:


```r
example2 <- load_premap("example2") %>% pipe_coerce_mapfile()#### should not be necessary TODO 

example2
```

```
## Factors: # A tibble: 12 x 23
##   label  id    factor_note factor_id0 factor_id factor_memo factor_map_id betweenness
##   <chr>  <chr> <lgl>            <dbl>     <int> <chr>               <int>       <dbl>
## 1 Bette~ 6     NA                   1         1 ""                      1           2
## 2 Coast~ 11    NA                   2         2 ""                      1           2
## # ... with 10 more rows, and 15 more variables: betweenness_rank <dbl>,
## #   in_degree <dbl>, out_degree <dbl>, frequency <dbl>, size <dbl>, found <int>,
## #   found_type <chr>, map_id <int>, driver_score <dbl>, outcome_score <dbl>,
## #   driver_rank <int>, outcome_rank <int>, is_opposable <lgl>,
## #   top_level_label <chr>, top_level_frequency <dbl>
## Links: # A tibble: 9 x 61
##   from_label  to_label  statement_id quote   simple_bundle  bundle   from    to note 
##   <chr>       <chr>            <int> <chr>   <chr>          <chr>   <int> <int> <lgl>
## 1 "High rain~ "Floodin~            1 Recent~ "High rainfal~ "High ~     9     7 NA   
## 2 "Flooding ~ "Damage ~            1 The fl~ "Flooding \U0~ "Flood~     7     4 NA   
## # ... with 7 more rows, and 52 more variables: link_id0 <int>, link_id <int>,
## #   weight <int>, strength <int>, certainty <int>, from_flipped <lgl>,
## #   to_flipped <lgl>, link_label <chr>, hashtag <chr>, link_memo <chr>,
## #   link_map_id <int>, s.text <chr>, s.statement_note <lgl>, s.#QuestionID <lgl>,
## #   s.#SourceID <lgl>, s.statement_memo <chr>, s.source_id <chr>,
## #   s.question_id <chr>, s.statement_map_id <dbl>, s.factor_id <int>,
## #   s.r.source_memo <chr>, s.r.source_map_id <dbl>, s.q.question_text <chr>,
## #   s.q.question_memo <chr>, s.q.question_map_id <dbl>, s.label <dbl>,
## #   frequency <int>, capacity <dbl>, width <dbl>, hashtags <chr>,
## #   actualisation <int>, map_id <int>, text <chr>, statement_note <lgl>,
## #   #QuestionID <lgl>, #SourceID <lgl>, statement_memo <chr>,
## #   statement_map_id <dbl>, factor_id <int>, r.source_memo <chr>,
## #   r.source_map_id <dbl>, q.question_text <chr>, q.question_memo <chr>,
## #   q.question_map_id <dbl>, label <dbl>, source_id <chr>, source_memo <chr>,
## #   source_map_id <dbl>, question_id <chr>, question_text <chr>,
## #   question_memo <chr>, question_map_id <dbl>
## Statements: # A tibble: 2 x 21
##   statement_id text           statement_note `#QuestionID` `#SourceID` statement_memo
##          <dbl> <chr>          <lgl>          <lgl>         <lgl>       <chr>         
## 1            1 Welcome to th~ NA             NA            NA          st memo       
## 2            2 Rising sea le~ NA             NA            NA          <NA>          
## # ... with 15 more variables: statement_map_id <dbl>, factor_id <int>,
## #   r.source_memo <chr>, r.source_map_id <dbl>, q.question_text <chr>,
## #   q.question_memo <chr>, q.question_map_id <dbl>, label <dbl>, source_id <chr>,
## #   source_memo <chr>, source_map_id <dbl>, question_id <chr>, question_text <chr>,
## #   question_memo <chr>, question_map_id <dbl>
## Sources: # A tibble: 1 x 3
##   source_id source_memo source_map_id
##   <chr>     <chr>               <dbl>
## 1 1         ooh                     1
## Questions: # A tibble: 1 x 4
##   question_id question_text   question_memo question_map_id
##   <chr>       <chr>           <chr>                   <dbl>
## 1 1           global question q memo                      1
## Settings: # A tibble: 1 x 3
##   setting_id        value map_id
##   <chr>             <chr> <chr> 
## 1 background_colour <NA>  1
```

```r
example2 %>% summary
```

```
## $colnames
## $colnames$factors
##  [1] "label"               "id"                  "factor_note"        
##  [4] "factor_id0"          "factor_id"           "factor_memo"        
##  [7] "factor_map_id"       "betweenness"         "betweenness_rank"   
## [10] "in_degree"           "out_degree"          "frequency"          
## [13] "size"                "found"               "found_type"         
## [16] "map_id"              "driver_score"        "outcome_score"      
## [19] "driver_rank"         "outcome_rank"        "is_opposable"       
## [22] "top_level_label"     "top_level_frequency"
## 
## $colnames$links
##  [1] "from_label"          "to_label"            "statement_id"       
##  [4] "quote"               "simple_bundle"       "bundle"             
##  [7] "from"                "to"                  "note"               
## [10] "link_id0"            "link_id"             "weight"             
## [13] "strength"            "certainty"           "from_flipped"       
## [16] "to_flipped"          "link_label"          "hashtag"            
## [19] "link_memo"           "link_map_id"         "s.text"             
## [22] "s.statement_note"    "s.#QuestionID"       "s.#SourceID"        
## [25] "s.statement_memo"    "s.source_id"         "s.question_id"      
## [28] "s.statement_map_id"  "s.factor_id"         "s.r.source_memo"    
## [31] "s.r.source_map_id"   "s.q.question_text"   "s.q.question_memo"  
## [34] "s.q.question_map_id" "s.label"             "frequency"          
## [37] "capacity"            "width"               "hashtags"           
## [40] "actualisation"       "map_id"              "text"               
## [43] "statement_note"      "#QuestionID"         "#SourceID"          
## [46] "statement_memo"      "statement_map_id"    "factor_id"          
## [49] "r.source_memo"       "r.source_map_id"     "q.question_text"    
## [52] "q.question_memo"     "q.question_map_id"   "label"              
## [55] "source_id"           "source_memo"         "source_map_id"      
## [58] "question_id"         "question_text"       "question_memo"      
## [61] "question_map_id"    
## 
## $colnames$statements
##  [1] "statement_id"      "text"              "statement_note"    "#QuestionID"      
##  [5] "#SourceID"         "statement_memo"    "statement_map_id"  "factor_id"        
##  [9] "r.source_memo"     "r.source_map_id"   "q.question_text"   "q.question_memo"  
## [13] "q.question_map_id" "label"             "source_id"         "source_memo"      
## [17] "source_map_id"     "question_id"       "question_text"     "question_memo"    
## [21] "question_map_id"  
## 
## $colnames$sources
## [1] "source_id"     "source_memo"   "source_map_id"
## 
## $colnames$questions
## [1] "question_id"     "question_text"   "question_memo"   "question_map_id"
## 
## $colnames$settings
## [1] "setting_id" "value"      "map_id"    
## 
## 
## $`Number of rows`
## # A tibble: 1 x 6
##   factors links statements sources questions settings
##     <int> <int>      <int>   <int>     <int>    <int>
## 1      12     9          2       1         1        1
```

Visualise them like this:


```r
example2 %>% make_interactive_map
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

You can also load up an Excel file:


```r
# system.file("extdata", "quip-lorem", package = "CausalMapFunctions") %>% 
#   get_mapfile_from_excel()
```

The file should have the standard Causal Map [format](guide.causalmap.app/core-tables.html): you can see an example by downloading any of the files in Causal Map on the web.


`pipe_coerce_mapfile` will also process a file with no factors and from_label and to_label columns as a named edgelist.


If you filter the factors of a mapfile, e.g. show only factors with labels beginning xyz,

- also the links are filtered (removing links to removed factors)
- the statements are not touched

If you filter the links of a mapfile, e.g. show only links with hashtags containing xyz,

- the factors are not filtered (but using a different command you can remove any factors which no longer have any links)
- the statements are not touched

If you filter the statements of a mapfile, e.g. show only statements with texts containing xyz,

- also the links are filtered (removing links to removed statements)
- the factors are filtered




```r
  # ll <- quip_example
  # ee <- example2
  ee <- load_premap("example2")%>% pipe_coerce_mapfile 
  tt <- load_premap("cm1/tearfund-sl")%>% pipe_coerce_mapfile
  ll <- load_premap("quip-coded")%>% pipe_coerce_mapfile
```

## Basic examples

### Accessing the data 

One column in one table


```r
ll %>%
    pipe_find_factors(value="economic") %>%
    .$factors %>%
    .$label %>% 
  knitr::kable()
```



|x                                                  |
|:--------------------------------------------------|
|(E) Poor economy                                   |
|(IEA) Poverty                                      |
|(BF) Started, expanded or invested in business [P] |
|(BF) Stopped/reduced piece work 'ganyu' [P]        |
|(IEA) Increased income [P]                         |
|(IEA) Increased purchasing power [P]               |
|(IEA) Increased savings/loans [P]                  |
|(IEA) Increased financial knowledge [P]            |
|(RW) Improved gender equality in household [P]     |
|(IEA) Increased economic independence [P]          |
|(IEA) No longer borrows from community members [P] |
|(RW) Increased resilience [P]                      |
|(E) Economic migration [N]                         |
|(RW) Reduction in household size                   |
|(RW) Moved to live with relative                   |

### Merging two maps 


```r
merge_mapfile(ee,tt %>% pipe_select_factors(top=8)) %>% 
  pipe_color_factors(field="map_id") %>% 
  pipe_color_links(field="map_id",fun="unique") %>% 
  make_interactive_map
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

Note warning if factor labels are shared



```r
load_premap("example2") %>%
  pipe_coerce_mapfile %>% 
  pipe_merge_mapfile("example2") %>% 
  pipe_color_factors(field="map_id") %>% 
  pipe_color_links(field="map_id",fun="unique") %>% 
  make_interactive_map
```

```
## Warning in merge_mapfile(graf, map2): Factor labels are shared!
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

### Editing maps directly

There is no guarantee that the resulting map is still a standard mapfile.


```r
ee$factors$label[1] <- "Label changed"
ee %>% make_interactive_map
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

### Editing maps with pipe_update_mapfile

There is no guarantee that the resulting map is still a standard mapfile.


```r
ee %>% 
  pipe_update_mapfile(factors = ee$factors %>% mutate(label="one")) %>% 
  make_interactive_map
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

Coercing to a standard mapfile:


```r
ee %>% 
  pipe_update_mapfile(factors = ee$factors %>% mutate(label="one")) %>% 
  pipe_coerce_mapfile() %>% 
  make_interactive_map
```

```
## Warning: Unreplaced values treated as NA as .x is not compatible. Please specify
## replacements exhaustively or supply .default

## Warning: Unreplaced values treated as NA as .x is not compatible. Please specify
## replacements exhaustively or supply .default
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)


### Interactive and Print maps


```r
  ee %>%
    pipe_label_links("link_id") %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

```r
  ee %>%
    pipe_label_links("link_id") %>%
    make_print_map()
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-2.png)

```r
  ee %>%
    pipe_set_print(grv_layout="circo") %>%
    make_print_map()
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-3.png)

### Selecting and finding 

factors links and statements


```r
ll %>%
    pipe_select_factors(5) %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)



```r
ll %>%
    pipe_select_links(5) %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)


```r
ll %>%
    pipe_find_statements(field="statement_id",value=5,operator="equals") %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)


```r
ll %>%
    pipe_find_factors(value="economic") %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)




```r
ll %>%
    pipe_select_factors(5) %>%
    pipe_find_factors(value="economic") %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

No result


```r
ll %>%
    pipe_find_factors(value="asdfasdfasdf") %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)




```r
ll %>%
    pipe_find_links(field="from_label",value="economic",operator="contains") %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)

### Numerical comparison


```r
ll %>%
    pipe_find_statements(field="statement_id",value=20,operator="less") %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)

No result


```r
ll %>%
    pipe_find_statements(field="statement_id",value=20000000,operator="greater") %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)

## Conditional formatting


```r
ll %>%
    pipe_select_factors(5) %>%
    pipe_scale_factors(field="frequency") %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)


```r
ll %>%
    pipe_select_factors(5) %>%
    pipe_color_factors(field="frequency") %>%
    pipe_color_borders(field="betweenness") %>%
    pipe_wrap_factors(5) %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png)


```r
ee %>%
    pipe_label_links("from_label",fun = "unique") %>%
    pipe_wrap_links(6) %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png)

## Remove brackets


```r
ll %>%
    pipe_select_factors(5) %>%
    pipe_remove_brackets() %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)

## Bundle factors


```r
ll %>%
    pipe_bundle_factors(value = "IEA") %>%
    pipe_select_factors(5) %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png)

## Format links without bundling


```r
ee %>%
  pipe_color_links("link_id",fun = "mean") %>%
  make_interactive_map
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-1.png)

```r
ee %>%
  pipe_scale_links("link_id",fun = "mean") %>%
  make_interactive_map
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-2.png)


## Bundling links

Note the defaults for `bundle_links` and `label_links`:


```r
ll %>%
    pipe_find_factors(value="economic") %>%
    pipe_select_factors(5) %>%
    pipe_bundle_links() %>%
    pipe_label_links() %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)


```r
ll %>%
    pipe_find_factors(value="economic") %>%
    pipe_select_factors(5) %>%
    pipe_bundle_links() %>%
    pipe_label_links() %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)

Note the default for `bundle_links` is equivalent to simple_bundle:


```r
ll %>%
    pipe_find_factors(value="economic") %>%
    pipe_select_factors(5) %>%
    pipe_bundle_links(group="simple_bundle") %>%
    pipe_label_links() %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-29](figure/unnamed-chunk-29-1.png)

Group and label by sex and scale by count:


```r
ll %>%
    pipe_select_factors(5) %>%
    pipe_bundle_links(group="1. Sex") %>%
    pipe_scale_links("link_id",fun = "count") %>%
    pipe_label_links("1. Sex",fun = "unique") %>%
    pipe_color_links("1. Sex",fun = "unique") %>%
    make_interactive_map
```

```
## Warning in min(vec, na.rm = T): no non-missing arguments to min; returning Inf
```

```
## Warning in max(vec, na.rm = T): no non-missing arguments to max; returning -Inf
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30-1.png)


```r
ll %>%
    pipe_select_links(16) %>%
    pipe_bundle_links(group="1. Sex") %>%
    pipe_label_links("link_id",fun = "count") %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-31](figure/unnamed-chunk-31-1.png)


Group by sex and scale and colour by count:


```r
ll %>%
    pipe_select_factors(5) %>%
    pipe_bundle_links(group="1. Sex") %>%
    pipe_color_links("link_id",fun = "count") %>%
    pipe_scale_links("link_id",fun = "count") %>%
    pipe_label_links("link_id",fun = "count") %>%
    make_interactive_map
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning in min(vec, na.rm = T): no non-missing arguments to min; returning Inf
```

```
## Warning in max(vec, na.rm = T): no non-missing arguments to max; returning -Inf
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)


## Nested maps


```r
tt %>%
    pipe_zoom_factors(1) %>%
    pipe_select_factors(5) %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-33-1.png)



```r
tt %>%
    pipe_zoom_factors(1) %>%
    pipe_bundle_links() %>%
    pipe_label_links() %>%
    pipe_scale_links() %>%
    make_print_map
```

![plot of chunk unnamed-chunk-34](figure/unnamed-chunk-34-1.png)


```r
tt %>%
    pipe_zoom_factors(1) %>%
    make_print_map
```

![plot of chunk unnamed-chunk-35](figure/unnamed-chunk-35-1.png)


## Nested maps with opposites

Note colours are not provided in Interactive view


```r
tt %>%
    pipe_zoom_factors(1) %>%
    pipe_combine_opposites() %>%
    pipe_select_links(3) %>%
    make_interactive_map
```

![plot of chunk unnamed-chunk-36](figure/unnamed-chunk-36-1.png)


## Path tracing


```r
cat("### Single\n")  
```

```
## ### Single
```

```r
ee %>%    
  pipe_trace_paths(from = "Funds",to="area",length = 5) %>% 
  make_interactive_map
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-37-1.png)

```r
cat("### Failing; no paths\n")  
```

```
## ### Failing; no paths
```

```r
ee %>%    
  pipe_trace_paths(from = "xx",to="yy",length = 5) %>% 
  make_interactive_map
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-37-2.png)

```r
cat("### Multiple\n")  
```

```
## ### Multiple
```

```r
ee %>%    
  pipe_trace_paths(from = "High",to="Damage",length = 5) %>% 
  make_interactive_map
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-37-3.png)

```r
tt %>%
  pipe_trace_paths(from = "Capabilities",to="[OP3]",length = 2) %>% 
  make_interactive_map
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-37-4.png)

```r
ee %>%    
  pipe_trace_paths(from = "Funds",to="area",length = 5) %>% 
  make_print_map()
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-37-5.png)

```r
tt %>%
    pipe_zoom_factors(1) %>%
    pipe_combine_opposites() %>%
    make_print_map
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-37-6.png)

### Robustness


```r
ee %>%    
  pipe_trace_robustness(from = "High",to="Damage",length = 5) %>% 
  pipe_wrap_factors() %>% 
  make_print_map()
```

```
## Joining, by = "label"
```

![plot of chunk unnamed-chunk-38](figure/unnamed-chunk-38-1.png)

```r
tt %>%
  pipe_trace_robustness(from = "Capabilities",to="[OP3]",length = 2) %>% 
  pipe_wrap_factors() %>% 
  make_print_map()
```

```
## Joining, by = "label"
```

![plot of chunk unnamed-chunk-38](figure/unnamed-chunk-38-2.png)

```r
tt %>%
  pipe_trace_robustness(from = "Capabilities",to="[OP3",length = 2) %>% 
  pipe_wrap_factors() %>% 
  make_print_map()
```

```
## Joining, by = "label"
```

![plot of chunk unnamed-chunk-38](figure/unnamed-chunk-38-3.png)

```r
ee %>%    
  pipe_trace_robustness(from = "High",to="Damage",length = 5) %>% 
  get_robustness()
```

```
## Joining, by = "label"
```

```
## # A tibble: 3 x 2
##   row_names            `High rainfall \U0001f327`
##   <chr>                                     <dbl>
## 1 All targets                                   0
## 2 Damage to Businesses                          1
## 3 Damage to Property                            1
```

```r
tt %>%
  pipe_trace_robustness(from = "Capabilities",to="[OP3]",length = 2) %>% 
  get_robustness()
```

```
## Joining, by = "label"
```

```
## # A tibble: 1 x 6
##   row_names    `All origins` `Capabilities; [P1~ `Capabilities; [P~ `Capabilities; [~
##   <chr>                <dbl>               <dbl>              <dbl>             <dbl>
## 1 Outcomes; [~             0                   2                  2                 2
## # ... with 1 more variable:
## #   Capabilities; [P18] Expertise/knowledge for holistic wellbeing <dbl>
```

```r
tt %>%
  pipe_trace_robustness(from = "Capabilities",to="[OP3",length = 2) %>% 
  get_robustness()
```

```
## Joining, by = "label"
```

```
## # A tibble: 5 x 7
##   row_names    `All origins` `~Capabilities; [N~ `Capabilities; [~ `Capabilities; [P~
##   <chr>                <dbl>               <dbl>             <dbl>              <dbl>
## 1 All targets              0                   0                 0                  0
## 2 Outcomes; [~             0                   0                 0                  0
## 3 Outcomes; [~             0                   1                 1                  1
## 4 Outcomes; [~             0                   0                 0                  1
## 5 Outcomes; [~             0                   1                 2                  2
## # ... with 2 more variables: Capabilities; [P15] CCMP: Envisioning the Church <dbl>,
## #   Capabilities; [P18] Expertise/knowledge for holistic wellbeing <dbl>
```

```r
tt %>%
  pipe_trace_robustness(from = "Capabilities; [P13",to="[OP3]",length = 2) %>% 
  get_robustness()
```

```
## Joining, by = "label"
```

```
## # A tibble: 1 x 2
##   row_names                                 `Capabilities; [P13] Acquistion of educa~
##   <chr>                                                                         <dbl>
## 1 Outcomes; [OP3] Diversification of livel~                                         2
```

Is the second one correct? 


```r
if(F){
graf <- tt1
graf$factors %>% filter(factor_id %notin% graf$links$from & factor_id %notin% graf$links$to)  %>% nrow

graf$links %>% filter(from %notin% graf$factors$factor_id & to %notin% graf$factors$factor_id)  %>% nrow

graf$links %>% filter(statement_id %notin% graf$statements$statement_id) %>% nrow
  }
```
