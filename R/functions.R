# a premap is any list of none or any or all of five tibbles with the five special names
# a mapfile is a premap which follows a special format
# - must have certain columns of certain types (suitable for the joins)
# - in particular has a primary key with distinct values; not necc consequetive integers
# - merges tables according to the joins
# - and has additional calculated columns
# - note that some funs like zoom, bundle factor etc may have different ids from previous mapfile
# - there should never be a need to apply pipe_coerce_mapfile twice

library(igraph)
library(configr)
library(DiagrammeR)
library(visNetwork)
library(tidyverse)
library(scales)
library(htmltools)
library(paws)
library(DT)
library(jsonlite)
library(DBI)
library(shiny)


# constants ---------------------------------------------------------------
safe_limit <- 200
contrary_color <- "#f26d04"
ordinary_color <- "#058488"


operator_list=c("=", "less", "greater", "notcontains", "notequals", "notequal", "equals", "equal", "contains", "starts", "ends", "start", "end")
buck <- "causalmap"
table_list <- c("factors","links","statements","sources","questions","settings") #has to be in Viewer as well


s3 <- paws::s3()

empty_visnetwork <- visNetwork(
  nodes = data.frame(id=1) %>% filter(F)
  )

standard_factors <- function(){
  tibble(label="blank factor",
         factor_memo="",map_id=1L,size=1L,factor_id=1L,is_flipped=F)
}
standard_links <- function(){tibble(
  link_id=1L,
  statement_id=1L,
  source_id=1L,
  question_id=1L,
  from=1L,
  to=1L,
  quote="",
  # frequency=1L,
  capacity=1,
  weight=1L,
  actualisation=1L,
  strength=1L,
  certainty=1L,
  from_flipped=F,
  to_flipped=F,
  link_label="",
  from_label="",
  simple_bundle="",
  to_label="",
  hashtags="",
  link_memo="",
  map_id=1L
)
}
standard_statements <- function()tibble(statement_id=1,text="example statement",statement_memo="",source_id="1",question_id="1",statement_map_id=1)
standard_sources <- function()tibble(source_id="1",source_memo="example source",source_map_id=1)
standard_questions <- function()tibble(question_id="1",question_text="example question",question_memo="",question_map_id=1)
standard_settings <- function()tibble(setting_id="background_colour",value="",map_id=1)

standard_table <- function(tab){
  do.call(paste0("standard_",tab),list())
}


# Utility functions -------------------------------------------------------



# # these are not generics so must be called directly
# nrow.cm <- function (graf){graf %>% map(nrow)}
# colnames.cm <- function (graf){graf %>% map(colnames)}
#

#' Add attribute
#'
#' @param graf A mapfile
#' @param value A value to add as attribute
#' @param attr The attribute to add to
#'
#' @return The mapfile `graf` but with an additional attribute `attr` with value `value`
#' @export
#'
#' @examples
add_attribute <- function(graf,value,attr){
  attr(graf,attr) <- value
  graf
}

finalise <- function(graf,value){
  #message("finalise")
  # simply reattaches the info list back to the graf. so you may need to update info during a function.
  # attr(graf,"info") <- value
  graf
}


# lcollapse <- function(x)map(x,~paste0(.,collapse=":"))%>% unlist(recursive=F)


#' Add class
#'
#' @param x Object to add a class to
#' @param cls Class to be added
#'
#' @export
#' @return `x` with class `cls` added
#' @examples
add_class <- function(x,cls="mapfile"){
  class(x) <- c(cls,class(x)) %>% unique
  x
}





# Loading maps --------------------------------------------------------------

s3file_exists <- function(object,buck){
  !is.null(safely(~s3$head_object(Key=object,Bucket=buck))()$result)
}

# note this is our own function , not from aws.s3 package
s3readRDS <- function(object,bucket,version=NULL,s3confun=s3){
  s3confun$get_object(bucket,Key=object, VersionId = version)$Body %>% rawConnection() %>%
    gzcon %>%
    (function(con) {on.exit(close(con)); readRDS(con)})
}

#' Title
#'
#' @param path The path of an .xslx file to load. The .xlsx file can have worksheets called `statements`, `links`, `factors`, `sources` and `questions`.
#'
#' @return
#' @export
#'
#' @examples
get_mapfile_from_excel <- function(path){

  readxl::excel_sheets(path %>% str_replace_all("\\\\","/")) %>%
    keep(. %in% table_list) %>%
    set_names %>%
    map(~readxl::read_excel(path,sheet = .)) %>%
    add_class
}
get_mapfile_from_s3 <- function(path){
  message("Trying cm2 file")
  if(!s3file_exists(object=basename(path),buck=dirname(path))) return()
  message("Loaded cm2 file")
  s3readRDS(object=basename(path),bucket=dirname(path))
}

#' Title
#'
#' @param tab
#' @param proj
#' @param connection
#'
#' @return
#' @export
#'
#' @examples
get_project_table <- function(tab="data",proj=sess$project,connection=conn){
  tbl(connection,local(tab)) %>%
    filter(project==local(proj)) %>%
    collect %>%
    mutate_all(to_logical)
}
#' Title
#'
#' @param tab
#' @param connection
#'
#' @return
#' @export
#'
#' @examples
get_whole_table <- function(tab,connection=conn){
  tbl(connection,local(tab)) %>% collect %>% mutate_all(to_logical)
}

#' Make map from links
#'
#' @param links
#' @param switch
#'
#' @return
#' @export
#'
#' @examples
make_map_from_links <- function(links,switch=F){
  # browser()
  if(nrow(links)==0) return()
  links <-
    links %>%
    filter(from!="" & to!="") %>%
    filter(!is.na(from) & !is.na(to)) %>%
    select(from,to,everything(),-project) %>%
    suppressMessages %>%
    filter(!is.na(from) & !is.na(to))
  factors=tibble(label=c(links$from,links$to) %>% unique)

  links$from <-
    recode(links$from,!!!(row_index(factors) %>% set_names(factors$label)))
  links$to <-
    recode(links$to,!!!(row_index(factors) %>% set_names(factors$label)))

  ## note these are switched around at present
  if(switch){
    links <- links %>%
      rename(from=to,to=from)
  }

  factors$id <- row_index(factors)

  return(
    list(
      factors = factors, #%>% factors_table,
      links = links, #%>% links_table,
      statements = NULL,               ########## STILL NEED TO GET SOURCES ETC
      sources = NULL,#tibble(source_id=links$source_id %>% unique),
      questions = NULL,
      settings = NULL
    )
  )

}
get_map_tables_from_sql <- function(path,connection){
  # browser()
  # vsettings <- get_whole_table("ss2settings",connection) %>% filter(project==path) # we need this anyway
  vdata <- get_project_table("ss2answers",path,connection)
  if(nrow(vdata)==0) return()
# if(F) { vmeta <- get_project_table("meta",path,connection)
#   vsentiment <- get_project_table("sentiment",path,connection)
#
#
#   r <- vsettings$boxes %>%
#     as.character()
#   if(r=="") recodes <- NULL else recodes <- fromJSON(r)   #TODO actual recodes
# }
  # browser()
  links <- req(vdata) %>%
    select(from,to,everything(),source_id=uid,-project) %>%
    suppressMessages %>%
    filter(!is.na(from) & !is.na(to))

  # if(F)if(nrow(vsentiment)>0)
  #   links <- links %>%   left_join_safe(vsentiment %>% group_by(session_token) %>% select(-project) %>% summarise_all(last),by="session_token") %>%
  #   suppressMessages

  factors=tibble(label=c(links$from,links$to) %>% unique)

  links$from <-
    recode(links$from,!!!(row_index(factors) %>% set_names(factors$label)))
  links$to <-
    recode(links$to,!!!(row_index(factors) %>% set_names(factors$label)))

  ## note these are switched around at present
  links <- links %>%
    rename(from=to,to=from)


  factors$id <- row_index(factors)

  return(
    list(
      factors = factors, #%>% factors_table,
      links = links, #%>% links_table,
      statements = NULL,               ########## STILL NEED TO GET SOURCES ETC
      sources = tibble(source_id=links$source_id %>% unique),
      questions = NULL,
      settings = NULL
    )
  )

}
get_map_tables_from_s3_pieces <- function(path){

  s3bucket <- dirname(dirname(path))
  root <- str_remove(path,"^" %>% paste0(s3bucket,"/"))

  factors = NULL
  links = NULL
  statements = NULL
  sources = NULL
  questions = NULL
  settings = NULL

  # browser()
  message("Trying cm1 file")
  # browser()
  pathx <- paste0(root,"/factors");
  if(s3file_exists(pathx,s3bucket))factors <- s3readRDS(object=pathx,bucket=s3bucket) %>% mutate_all(~str_remove_all(.,"\n")) else return()
  pathx <- paste0(root,"/links");
  if(s3file_exists(pathx,s3bucket))links <- s3readRDS(object=pathx,bucket=s3bucket) %>% mutate_all(~str_remove_all(.,"\n"))
  pathx <- paste0(root,"/statements");
  if(s3file_exists(pathx,s3bucket))statements <- s3readRDS(object=pathx,bucket=s3bucket) %>% mutate_all(~str_remove_all(.,"\n"))
  pathx <- paste0(root,"/statements_extra");
  if(s3file_exists(pathx,s3bucket))statements_extra <- s3readRDS(object=pathx,bucket=s3bucket) %>% mutate_all(~str_remove_all(.,"\n"))
  # browser()
  if(!is.null(statements))statements <-  join_statements_to_meta(statements,statements_extra) %>% select(statement_id,everything())
  # graf <- create_map(factors,links)

  if("#SourceID" %in% colnames(statements)){
    statements <- statements %>% select(-any_of("source_id")) %>% rename(source_id=`#SourceID`)
    tmpb <- statements %>%
      select(-any_of("timestamp")) %>%
      group_by(`source_id`) %>%
      summarise(across(everything(),~length(unique(.)))) %>%
      ungroup %>%
      select(-`source_id`)
    source_cols <- colnames(tmpb)[tmpb %>% summarise_all(~min(.)==1&max(.)==1) %>% unlist]
    # browser()
    #
    sources <- statements[,c("source_id",source_cols)] %>%
      group_by(`source_id`) %>%
      summarise_all(first)
    #
    #
    #
  }

  if("#QuestionID" %in% colnames(statements)){
    statements <- statements %>% select(-any_of("question_id")) %>% rename(question_id=`#QuestionID`)
    tmpa <- statements %>%
      select(-any_of("timestamp")) %>%
      group_by(`question_id`) %>%
      summarise(across(everything(),~length(unique(.)))) %>%
      ungroup %>%
      select(-`question_id`)
    question_cols <- colnames(tmpa)[tmpa %>% summarise_all(~min(.)==1&max(.)==1) %>% unlist]
    questions <- statements[,c("question_id",question_cols)] %>%
      group_by(`question_id`) %>%
      summarise_all(first)

  }
  statements <-
    statements %>% select(-colnames(sources),-colnames(questions),any_of("source_id"),any_of("question_id"))
  #
  # browser()
  # attr(graf,"statements") <- statements_with_meta
  message("Loaded cm1 file")
  list(
    factors = factors,
    links = links,
    statements = statements,               ########## STILL NEED TO GET SOURCES ETC
    sources = sources,
    questions = questions,
    settings = settings
  )

}
join_statements_to_meta <- function(statements,meta){
  # browser()
  meta %>%
    select(statement_id,key,value) %>%
    unique %>%
    spread(key,value,convert=T) %>%
    select(-contains("statement_note")) %>%
    left_join_safe(statements,.,by="statement_id") %>%
    suppressMessages
}



# internal general utilities-----------------------------------------------------------------------------

message <- message # alias
# return_notify <- function(tex){
#   message(tex,3)
#   return()
# }



bind_rows_safe <- function(x,y,...){
  if(is.null(x) & is.null(y))return(NULL)
  if(is.null(x))return(y)
  if(is.null(y))return(x)


  x <- x %>% select(where(~!is_list(.))) # drop any list columns
  y <- y %>% select(where(~!is_list(.)))

  by=intersect(colnames(x),colnames(y))
  if(is.null(by))return()
  for(i in seq_along(by)){
    # message(by[i])
    # if(by[i]=="before_id") browser()
    y[,by[i]] <- coerceValue(unlist(y[,by[i]]),unlist(x[,by[i]]))
  }
  bind_rows(x,y,...)

}


#' Left join safe
#'
#' @param x
#' @param y
#' @param by
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
left_join_safe <- function(x,y,by=NULL,winner="y",...){
  # browser()h
  if(is.null(by))by=intersect(colnames(x),colnames(y))
  if(winner=="y")x=x %>% select(-intersect(colnames(x),colnames(y)),by) else  # so the second table takes precedence
    y=y %>% select(-intersect(colnames(x),colnames(y)),by)  # so the second table takes precedence
  for(i in seq_along(by)){
    y[,by[i]] <- coerceValue(unlist(y[,by[i]]),unlist(x[,by[i]]))
  }
  left_join(x,y,by,...)
}



replace_null <- function(x,replacement=0){
  if(is.null(x)) replacement else x
}
replace_Inf <- function(x,replacement=0){
  # browser()
  ifelse(is.infinite(x),replacement , x)
}
replace_inf <- replace_Inf #alias
replace_zero <- function(x,replacement=0){
  if(length(x)==0) replacement else x
}
replace_zero_rows <- function(x,replacement=NULL){
  if(nrow(x)==0) replacement else x
}

# left_join_safe_safe <- function(x,y,by,...){
#   browser()
#
#   left_join_safe(x,y %>% select(colnames(.) %>% setdiff(colnames(x)) %>% c(by)),...)
# }

xc <- function(x, sep = " ") {
  str_split(x, sep)[[1]]
}

`%notin%` <- Negate(`%in%`)

#' Escape Regex
#'
#' @param string
#'
#' @return
#' @export
#'
#' @examples
escapeRegex <- function(string){ #from hmisc
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",
       string)
}



# major functions and pipes but not for use with parser -------------------------------------------------------------





#' Load mapfile
#'
#' @param path
#' @param connection
#'
#' @return
#' @export
#'
#' @examples
load_mapfile <- function(path=NULL,connection=conn){
  graf <- NULL
  factors <- NULL
  links <- NULL
  statements <- NULL
  sources <- NULL
  questions <- NULL
  settings <- NULL
  newtables <- NULL
  # browser()
  if(!is.null(path)){
    if(str_detect(path,"xlsx$")){
      type <- "excel"

    } else
    if(!(str_detect(path,"/"))){
      type <- "unknown"

    } else {
      type <- path %>% str_match("^.*?/") %>% str_remove("/")
      path <- path %>% str_remove("^.*?/")
    }#
  } else type <- "individual"

  if(type=="file") graf <- readRDS(path) else
    if(type=="standard"){
      tmp <- safely(get)(path)
      if(tmp$result %>% is.null) return(NULL) else graf <- tmp$result
      message("Loaded standard file")

    } else if(type=="excel"){
      graf <- get_mapfile_from_excel(path = path)
      if(is.null(graf)) return(NULL)
      message("Loaded excel file")
    } else if(type=="sql"){
      graf <- make_map_from_links(get_project_table("ss2answers",path,connection))
      # graf <- get_map_tables_from_sql(path,connection=connection)
      if(is.null(graf)) return(NULL)
      message("Loaded sql file")
    } else  if(type=="cm2"){
      graf <- get_mapfile_from_s3(path %>% paste0("cm2data/",.)) %>% as.list  #as list because of tidygraph format
      if(is.null(graf)) return(NULL)
      if(is.null(graf$factors) & !is.null(graf$nodes)) graf$factors <- graf$nodes
      if(is.null(graf$links) & !is.null(graf$edges)) graf$links <- graf$edges
    } else if(type=="cm1"){
      graf <- get_map_tables_from_s3_pieces(path %>% paste0("causalmap/app-sync/",.))
      if(is.null(graf))return(NULL)
    } else if(type=="unknown"){
      message("Trying to load file, guessing origin")

      graf <- get_mapfile_from_s3(path %>% paste0("cm2data/",.))
      if(is.null(graf)) {
        graf <- get_map_tables_from_s3_pieces(path %>% paste0("causalmap/app-sync/",.))

      }

    }

  # browser()
  if(is.null(graf))graf <- pipe_update_mapfile()

  message("Loading map")
  # browser()
  return(
    graf %>%
      pipe_coerce_mapfile() %>%
      pipe_update_mapfile(.,links=add_before_and_after_ids_to_links(.$links)) # note this is the only thing which needs to be added on initial load
  )



}
#' Update mapfile
#' @inheritParams parse_commands
#' @param factors
#' @param links
#' @param statements
#' @param sources
#' @param questions
#' @param settings
#' @param clean
#'
#' @return A tidy map. If both factors and links are NULL, and empty map is returned.
#' @export
#'
#' @examples
pipe_update_mapfile <- function(
    tables=NULL,
    factors=NULL,
    links=NULL,
    statements=NULL,
    sources=NULL,
    questions=NULL,
    settings=NULL
    # ,
    # clean=T,
    # all=T
){
  # browser()

  message("pipe update")
  list(
    factors=factors %>% replace_null(tables$factors) %>% replace_null(standard_factors()),# %>% replace_zero_rows(standard_factors()),
    links=links %>% replace_null(tables$links) %>% replace_null(standard_links()) ,# %>% replace_zero_rows(standard_links()),
    statements=statements %>% replace_null(tables$statements) %>% replace_null(standard_statements()) ,# %>% replace_zero_rows(standard_statements()),
    sources=sources %>% replace_null(tables$sources) %>% replace_null(standard_sources()) ,# %>% replace_zero_rows(standard_sources()),
    questions=questions %>% replace_null(tables$questions) %>% replace_null(standard_questions()) ,# %>% replace_zero_rows(standard_questions()),
    settings=settings %>% replace_null(tables$settings) %>% replace_null(standard_settings()) # %>% replace_zero_rows(standard_settings())
  )

}


dismantle_mapfile <- function(graf){
  walk(names(graf),function(x)assign(x
                           ,
                           # graf[[x]] %>% replace_null(standard_table(x))
                           graf[[x]] %>% replace_null(standard_table(x)) %>% replace_zero_rows(standard_table(x))
                           ,
                           envir = .GlobalEnv))
}


#' Coerce mapfile
#'
#' @param tables
#' we assume that the initial map has standard format
#' @return A clean map in which all issues have been resolved.
#' @export
#'
#' @examples
pipe_coerce_mapfile <- function(tables){

  # say()
# browser()

  # enable creating map from edgelist
  if(is.null(tables$factors) &
     !is.null(tables$links)){
    if("from_label" %in% colnames(tables$links) &
       "to_label" %in% colnames(tables$links)){
      tables <- factors_links_from_named_edgelist(tables$links)
    }
  }

  dismantle_mapfile(tables)

  # factors <- tables$factors %>% replace_null(standard_factors()) %>% replace_zero_rows(standard_factors())
  # links <- tables$links %>% replace_null(standard_links()) %>% replace_zero_rows(standard_links())
  # statements <- tables$statements %>% replace_null(standard_statements()) %>% replace_zero_rows(standard_statements())
  # sources <- tables$sources %>% replace_null(standard_sources()) %>% replace_zero_rows(standard_sources())
  # questions <- tables$questions %>% replace_null(standard_questions()) %>% replace_zero_rows(standard_questions())
  # settings <- tables$settings %>% replace_null(standard_settings()) %>% replace_zero_rows(standard_settings())
  # tmp <- pipe_remove_orphaned_links(list(factors=factors,links=links))
  # factors <- tmp$factors
  # links <- tmp$links












  flow <- attr(links,"flow")

  if("link_id" %notin% colnames(links)) links <-  links %>%
        mutate(link_id=row_number())

    links <-  links %>%
      select(-any_of("frequency")) %>%
      add_column(.name_repair="minimal",!!!standard_links())   %>%
      select(which(!duplicated(colnames(.)))) %>%
      select(-starts_with("..."))

    links <- links %>% select(-any_of(c("link_id.1","statement_id.2","from.2","to.2","quote.2","frequency.1","weight.2","actualisation.2","strength.2","certainty.2","from_flipped.1","to_flipped.1","link_label.1","from_label.1","to_label.1","hashtags.2","link_memo.1","link_map_id.1","link_id.2","statement_id.3","from.3","to.3","quote.3","frequency.2","weight.3","actualisation.3","strength.3","certainty.3","from_flipped.2","to_flipped.2","link_label.2","from_label.2","to_label.2","hashtags.3","link_memo.2","link_map_id.2","statement_id.1","from.1","to.1","quote.1","weight.1","actualisation.1","strength.1","certainty.1","hashtags.1")))#FIXME TODO  this is just legacy/transition




    links[,colnames(standard_links())] <- map(colnames(standard_links()),
                                              ~coerceValue(links[[.]],standard_links()[[.]]))
    links$link_label[is.na(links$link_label)] <- ""
    if("width" %in% colnames(links))links$width[is.na(links$width)] <- 0.2
    links <- links %>%
      filter(!is.na(from) & !is.na(to) & !is.na(statement_id) & !is.na(link_id))%>%
      distinct(link_id,.keep_all = T)





    if("factor_id" %notin% colnames(factors))  factors <-  factors %>%
        mutate(factor_id=row_number())
    if(suppressWarnings(is.infinite(max(as.numeric(factors$factor_id)))))  factors <-  factors %>%
        mutate(factor_id=row_number())




    factors <-  factors %>%
      # select(-starts_with("color")) %>%
      add_column(.name_repair="minimal",!!!standard_factors())   %>%
      select(which(!duplicated(colnames(.)))) %>%
      select(-starts_with("..."))

    factors[,colnames(standard_factors())] <- map(colnames(standard_factors()),
                                                  ~coerceValue(factors[[.]],standard_factors()[[.]]))

    ## trim label-------------------------------------------------
    factors$label <- str_trim(factors$label)
    factors$size <- replace_na(factors$size,1)

    # browser()
    # ensure distinct label and id-----------------------------------------------
    if(nrow(factors)>0)factors <-
      factors %>%
      mutate(factor_id=ifelse(is.na(factor_id),max(factors$factor_id,na.rm=T)+row_number(),factor_id)) %>%
      filter(!is.na(label) & !is.na(factor_id))%>%
      distinct(factor_id,.keep_all = T) %>%
      distinct(label,.keep_all = T)



    if("statement_id" %notin% colnames(statements)) statements <-  statements %>%
        mutate(statement_id=row_number())

    statements <-  statements %>%
      # select(-starts_with("color")) %>%
      add_column(.name_repair="minimal",!!!standard_statements()) %>%
      select(which(!duplicated(colnames(.)))) %>%
      select(-starts_with("..."))

    statements[,colnames(standard_statements())] <- map(colnames(standard_statements()),
                                                        ~coerceValue(statements[[.]],standard_statements()[[.]]))
    statements <- statements %>%
      filter(!is.na(statement_id))%>%
      distinct(statement_id,.keep_all = T)


    if("source_id" %notin% colnames(sources)) sources <-  sources %>%
        mutate(source_id=row_number() %>% as.character)
    sources <-  sources %>%
      # select(-starts_with("color")) %>%
      add_column(.name_repair="minimal",!!!standard_sources()) %>%
      select(which(!duplicated(colnames(.)))) %>%
      select(-starts_with("..."))

    sources[,colnames(standard_sources())] <- map(colnames(standard_sources()),
                                                  ~coerceValue(sources[[.]],standard_sources()[[.]]))
    sources <- sources %>%
      filter(!is.na(source_id))%>%
      distinct(source_id,.keep_all = T)


    if("question_id" %notin% colnames(questions)) questions <-  questions %>%
        mutate(question_id=row_number() %>% as.character)

    questions <-  questions %>%
      # select(-starts_with("color")) %>%
      add_column(.name_repair="minimal",!!!standard_questions()) %>%
      select(which(!duplicated(colnames(.)))) %>%
      select(-starts_with("..."))

    questions[,colnames(standard_questions())] <- map(colnames(standard_questions()),
                                                      ~coerceValue(questions[[.]],standard_questions()[[.]]))
    questions <- questions %>%
      filter(!is.na(question_id))%>%
      distinct(question_id,.keep_all = T)




  ################### needs completing as above
  settings <- settings %>% replace_null(empty_tibble) %>% add_column(.name_repair="minimal",!!!standard_settings()) %>% select(any_of(colnames(standard_settings())))
  settings <- settings %>% mutate_all(as.character)



  ## add missing statements

  # browser()
  if(!all(links$statement_id %in% statements$statement_id)){
    message("link statementids not in statements")

  }

  # browser()

  statements <- statements %>%
    left_join_safe(sources,by="source_id") %>% suppressMessages %>%
    left_join_safe(questions,by="question_id") %>% suppressMessages

  ### this can make source_id disappear from links if they are not properly coded in statement table!!!!! #FIXME
  links <- links %>%
    left_join_safe(statements, by="statement_id") %>%
    suppressMessages


  attr(links,"flow") <- flow


  # FROM DELTA
  if(!identical(statements$statement_id,1:nrow(statements))){
    # browser()
    # browser()
    if("statement_id" %in% colnames(statements))
    {
      statements$statement_id <- as.numeric(statements$statement_id)
      statements$statement_id <- replace_na(statements$statement_id,Inf)
    }
    if(!is.null(links))links$statement_id <- recode(links$statement_id,!!!(row_index(statements) %>% set_names(statements$statement_id)))
    statements <-  statements %>%
      mutate(statement_id=row_number())
  }


  factors <- factors[,colnames(factors)!=""]
  graf <- pipe_update_mapfile(factors=factors,links=links,statements=statements,sources=sources,questions=questions,settings=settings) %>%
    pipe_remove_orphaned_links()

  graf %>% pipe_recalculate_all()
}







add_before_and_after_ids_to_links <- function(links){
  # for show_continuity, need to store all the before and after link ids in each link.

  before_ids <-
    links %>% select(-any_of("before_id")) %>% select("before_id"=link_id,"from"=to) %>%
    group_by(from) %>%
    summarise(before_id=list(before_id))

  after_ids <-
    links %>% select(-any_of("after_id")) %>% select("after_id"=link_id,"to"=from) %>%
    group_by(to) %>%
    summarise(after_id=list(after_id))


  links <-
    links %>%
    select(-any_of("before_id")) %>%
    select(-any_of("after_id")) %>%
    left_join(before_ids,by="from") %>%
    left_join(after_ids,by="to")
  links
}

add_simple_bundle_to_links <- function(links){
  links %>%
    unite(simple_bundle,from_label,to_label,remove = F,sep = " / ") %>%
    select(from_label,to_label,statement_id,quote,everything()) %>%
    group_by(simple_bundle) %>%
    mutate(simple_frequency=n())%>%
    group_by(simple_bundle) %>%
    mutate(source_frequency=length(unique(source_id))) %>%
    ungroup

}


#' Fix factors columns
#'
#' @inheritParams parse_commands
#'
#' @return A mapfile with a additional columns.
#' @export
#'
#'
#' @examples
fix_columns_factors <- function(factors){
  # browser()
  message("fix col factors")
  if(!("color.background" %in% colnames(factors))) factors <- factors %>% mutate(color.background="#ffffff")
  if(!("is_flipped" %in% colnames(factors))) factors <- factors %>% mutate(is_flipped=F)
  if(!("font.color" %in% colnames(factors))) factors <- factors %>% mutate(font.color="#000000")
  if(!("color.border" %in% colnames(factors))) factors <- factors %>% mutate(color.border="#ffffff")
  if(!("size" %in% colnames(factors))) factors <- factors %>% mutate(size=1L)
  if(!("cluster" %in% colnames(factors))) factors <- factors %>% mutate(cluster="")
  if(!("found" %in% colnames(factors))) factors <- factors %>% mutate(found=1L)
  if(!("found_type" %in% colnames(factors))) factors <- factors %>% mutate(found_type="")

  factors

}

# recalculate_links <- function(factors,links){
#   # browser()
#   message("recalc links")
#   links <-
#   links
# }

#' Fix links columns
#'
#' @inheritParams parse_commands
#'
#' @return A mapfile with a additional columns.
#' @export
#'
#'
#' @examples
fix_columns_links <- function(links){

  if(!("color" %in% colnames(links))) links <- links %>% mutate(color=ordinary_color)
  # if(!("frequency" %in% colnames(links))) links <- links %>% mutate(frequency=1L)
  if(!("capacity" %in% colnames(links))) links <- links %>% mutate(capacity=1L)
  if(!("label" %in% colnames(links))) links <- links %>% mutate(label="")
  if(!("width" %in% colnames(links))) links <- links %>% mutate(width=.2)
  if(!("flipped_bundle" %in% colnames(links))) links <- links %>% mutate(flipped_bundle=simple_bundle)
  #if(!("link_id0" %in% colnames(links))) links <- links %>% mutate(link_id0=1L)  # if(!("link_id0" %in% colnames(links))) links <- links %>% mutate(link_id0=1L)
  links
}


create_factor_quickfields <- function(factors){
  # browser()
  quickfields <-
    factors$label %>%
    str_match_all(.,"([:alnum:]*)\\:[:alnum:]") %>%
    map(function(x)x[,2]) %>% unlist %>%
    na.omit %>%
    unique %>%
    keep(.!="")
  if(length(quickfields)>0){
    for(dim in quickfields){
      if(dim %notin% colnames(factors))factors[,dim] <- {
        factors$label %>%
          str_match(.,paste0(dim,"\\:([:alnum:]*)")) %>% `[`(,2) %>%
          as_numeric_if_all()

      }
    }
  }
  # browser()
  factors

}


create_link_quickfields <- function(links){
  # browser()
  quickfields <-
    links$hashtags %>%
    str_match_all(.,"([:alnum:]*)\\:[:alnum:]") %>%
    map(function(x)x[,2]) %>% unlist %>%
    na.omit %>%
    unique

  if(length(quickfields)>0){
    for(dim in quickfields){
      if(dim %notin% colnames(links))links[,dim] <- {
        links$hashtags %>%
          str_match(.,paste0(dim,"\\:([:alnum:]*)")) %>% `[`(,2) %>%
          as_numeric_if_all()

      }
    }
  }
  links

}

#' Fix columns
#'
#' @inheritParams parse_commands
#'
#' @return A mapfile with a additional columns.
#' @export
#'
#'
#' @examples
#' ## PROBABLY DON'T NEED THESE NOW
pipe_recalculate_all <- function(graf){
  message("recalc all")
  graf %>%
    pipe_recalculate_factors  %>%
    pipe_recalculate_links

}
#' Title
#'
#' @param graf
#'
#' @return A mapfile whose factors table contains the following fields.
#' betweenness: the number of paths going through the factor.
#' betweenness_rank: the rank of the betweenness.
#' in_degree: the number of from_before links.
#' out_degree: the number of outgoing links.
#' role: the number of from_before links minus the number of from_before links. High values are drivers, low values are outcomes
#' frequency: the number of links.
#' driver_score: how strongly is this factor a driver?
#' outcome_score: how strongly is this factor an outcome?
#' driver_rank: rank of driver_score.
#' outcome_rank: rank of outcome_score.
#' is_opposable: does the factor label contain a ~.
#' zoom_level: number of ; separators in factor label, plus 1.
#' top_level_label: the label of the factor's ultimate parent in the hierarchy, if any.
#' top_level_frequency: the number of links to and from the top level factor.
#' @export
#'
#' @examples
pipe_recalculate_factors <- function(graf){
  # browser()
  message("pipe recalc factors")


  graf$factors <- graf$factors[,colnames(graf$factors)!=""]


  graf %>%
    pipe_update_mapfile(
      factors = add_metrics_to_factors(graf$factors,graf$links)%>%
        create_factor_quickfields
    )  %>%
    pipe_add_factor_source_counts() %>%
    finalise(info)
}
#' Title
#'
#' @param graf
#'
#' @return
#' @export
#'
#' @examples
pipe_recalculate_links <- function(graf){

# browser()
  graf %>%
    pipe_update_mapfile(
      links = graf$links %>%
        add_labels_to_links(graf$factors) %>%
        create_link_quickfields() %>%
        add_simple_bundle_to_links()

    ) %>% finalise(info)
}





# update_join <- function(old,new){
#   old %>%
#     mutate(.rn=row_number()) %>%
#     anti_join(new) %>%
#     bind_rows_safe(new) %>%
#     arrange(.rn) %>%
#     select(-.rn)
# }


#' Update map
#' @inheritParams parse_commands
#' @param factors
#' @param links
#' @param statements
#' @param sources
#' @param questions
#' @param settings
#' @param clean
#'
#' @return A tidy map. If both factors and links are NULL, and empty map is returned.
#' @export
#'
#' @examples
# update_map <- function(map,
#                        factors=NULL,
#                        links=NULL,
#                        statements=NULL,
#                        sources=NULL,
#                        questions=NULL,
#                        settings=NULL,
#                        tables=list(
#                          factors=NULL,
#                          links=NULL,
#                          statements=NULL,
#                          sources=NULL,
#                          questions=NULL,
#                          settings=NULL
#                        ),
#                        clean=T,
#                        all=T){
#   # browser()
#   if(!is.null(tables)){
#     if(is.null(factors))       factors <- tables$factors
#     if(is.null(links))       links <- tables$links
#     if(is.null(statements))  statements <- tables$statements
#     if(is.null(sources))     sources <- tables$sources
#     if(is.null(questions))   questions <- tables$questions
#     if(is.null(settings))    settings <- tables$settings
#   }
#   if(is.null(factors) & !is.null(map))factors <- factors_table(map)
#   if(!all) {
#     if(".rn" %notin% colnames(factors)) return(map)
#
#     # factors <-
#     #   factors_table(map) %>%
#     #   update_join(factors)
#   }
#   if(is.null(factors))     factors <- map$factors
#   if(is.null(links))       links <- map$links
#   if(is.null(statements))  statements <- map$statements
#   if(is.null(sources))     sources <- map$sources
#   if(is.null(questions))   questions <- map$questions
#   if(is.null(settings))    settings <- map$settings
#
#   # browser()
#   assemble_mapfile(
#     factors=factors,
#     links=links,
#     # links=links %>%  filter(from %in% (factors %>% row_index)) %>% filter(to %in% (factors %>% row_index)),
#     statements=statements,
#     sources=sources,
#     questions=questions,
#     settings=settings
#
#   )
#
# }

is_in_hierarchy <- function(labels){
  tops <-
    labels %>%
    str_match(".*?;") %>%
    keep(str_detect(.,";$")) %>%
    str_remove(.,";$")

str_detect(labels,";") | labels %in% tops
}

#' Title
#'
#' @param factors
#' @param links
#'
#' @return
#' @export
#'
#' @examples
add_metrics_to_factors <- function(factors,links){
  ig <- make_igraph(factors,links)


  factors$betweenness <- igraph::centr_betw(ig)$res %>% round(2)
  factors$betweenness_rank <- factors$betweenness %>% rank(ties.method="max")
  factors$betweenness_rank_reversed <- 1+max(factors$betweenness_rank,na.rm=T)-factors$betweenness_rank
  factors$in_degree=ig %>% igraph::degree(mode = "in")
  factors$out_degree=ig %>% igraph::degree(mode = "out")
  factors$role <- factors$in_degree-factors$out_degree
  factors$frequency <- factors$in_degree+factors$out_degree
  factors$frequency_rank <- factors$frequency %>% rank(ties.method="max")
  factors$frequency_rank_reversed <- 1+max(factors$frequency_rank,na.rm=T)-factors$frequency_rank
  factors$driver_score=factors$out_degree-factors$in_degree*2 %>% suppressWarnings()
  factors$outcome_score=factors$in_degree-factors$out_degree*2 %>% suppressWarnings()
  factors$driver_rank=factors$driver_score %>% rank(ties.method = "max") %>% suppressWarnings()
  factors$driver_rank_reversed <- 1+max(factors$driver_rank,na.rm=T)-factors$driver_rank
  factors$outcome_rank=factors$outcome_score %>% rank(ties.method = "max") %>% suppressWarnings()
  factors$outcome_rank_reversed <- 1+max(factors$outcome_rank,na.rm=T)-factors$outcome_rank
  factors$is_opposable=str_detect(factors$label,"^~")
  factors$zoom_level=str_count(factors$label,";")+1

  factors$is_in_hierarchy=factors$label %>% is_in_hierarchy()

  # browser()
  if(any(str_detect(factors$label,";")))factors <-
    factors %>%
    # filter(is_in_hierarchy) %>%
    select(-starts_with("level_")) %>%
    separate(label,remove=F,sep=";",into=paste0("level_",1:(factors$label %>% str_count(";") %>% max %>% `+`(1)),"_label"),fill="right",extra="drop")


  if(F&nrow(factors)>0){
    factors$top_level_label=zoom_inner(factors$label)
    factors <- factors %>%
      group_by(top_level_label) %>%
      mutate(top_level_frequency=sum(frequency)) %>%
      ungroup
  }

  return(factors)

}
add_labels_to_links <- function(links,factors){
  # browser()
  if(nrow(links)>0 & nrow(factors)>0){
    links %>% mutate(from_label= recode(as.numeric(from),!!!factors$label %>% set_names(factors$factor_id))) %>%
      mutate(to_label= recode(as.numeric(to),!!!factors$label %>% set_names(factors$factor_id)))
  } else {
    links %>% mutate(from_label= "") %>%
      mutate(to_label= "")
  }}



pipe_add_factor_source_counts <- function(mapfile){
  # browser()

  info <-   make_info(mapfile,as.list(match.call()))

  # tmp <-
  tmp <-   mapfile %>%
    make_mentions_tabl() %>%
    select(factor_id,link_id,direction,any_of("statement_id")) %>%
    filter(!is.na(link_id))

  if(nrow(tmp)==0) res <- mapfile$factors %>% mutate(from_source_count=0,to_source_count=0,`source_count`=0) else
    res <-
    tmp %>%
    left_join_safe(mapfile$statements %>% select(any_of(c("statement_id","source_id")))) %>%
    group_by(factor_id,direction) %>%
    # this is where the overlap stuff should fit in!!
    summarise(
      n__=length(unique(source_id))
    )%>%
    pivot_wider(names_from=2,values_from=3,values_fill = 0) %>%
    select(from_source_count=consequence,to_source_count=influence,`source_count`=either,factor_id) %>%
    left_join_safe(mapfile$factors %>% select(!contains("source_count")),.) %>%
    mutate(`source_count`=replace_na(`source_count`,0)) %>%
    mutate(from_source_count=replace_na(from_source_count,0)) %>%
    mutate(to_source_count=replace_na(to_source_count,0))

  res %>%
    pipe_update_mapfile(mapfile,factors=.) %>% finalise(info)
  # browser()
}

factors_links_from_named_edgelist <- function(links){
  tmp <- c(links$from_label,links$to_label) %>% unique

  factors <- tibble(label=tmp,factor_id=seq_along(tmp))
  links$from <- recode(links$from_label,!!!(factors$factor_id %>% set_names(factors$label)))
  links$to <- recode(links$to_label,!!!(factors$factor_id %>% set_names(factors$label)))

  return(list(factors=factors,links=links))
}

# just for the cases when calculating robustness where map2 only has factors and links
merge_factors_links <- function(map1,map2){
  maxfactorid <- max(as.numeric(map1$factors$factor_id))
  # browser()
  pipe_update_mapfile(

    factors=map1$factors %>%
      bind_rows_safe(map2$factors %>%
                       mutate(factor_id=factor_id+maxfactorid)
      ),
    links=map1$links  %>%
      bind_rows_safe(map2$links %>%
                       mutate(from=from+maxfactorid,to=to+maxfactorid)
      ),
    statements=map1$statements  ,
    sources=map1$sources ,
    questions=map1$questions,
    settings=map1$settings
  ) %>% pipe_compact_mapfile()



  # %>%
  #   pipe_clean_map(tables=.)

}
#' Merge map
#' @inheritParams parse_commands
#' @param graf
#' @param path
#' @description This also has a wrapper, pipe_merge_mapfile
#' @return A tidy map. The column *_map_id is set to reflect the id of the map.
#' @export
#'
#' @examples
merge_mapfile <- function(map1,map2){
  # map1 <- map1# %>% pipe_clean_map() #
  # map2 <- graf2# %>% pipe_clean_map() #  clean map will put the important vars to integer.

  # browser()
  maxfactorid <- max(as.numeric(map1$factors$factor_id))
  maxstatementid <- max(as.numeric(map1$statements$statement_id))

  if("map_id" %notin% colnames(map1$factors))map1$factors$map_id <- 1
  if("map_id" %notin% colnames(map1$links))map1$links$map_id <- 1
  if("map_id" %notin% colnames(map1$statements))map1$statements$map_id <- 1
  if("map_id" %notin% colnames(map1$sources))map1$sources$map_id <- 1
  if("map_id" %notin% colnames(map1$questions))map1$questions$map_id <- 1


  maxmapid <- max(as.numeric(map1$factors$map_id))

  if(any(map1$factors$label %in% map2$factors$label)    |
     any(map2$factors$label %in% map1$factors$label)) warning("Factor labels are shared!")

  # browser()
  pipe_update_mapfile(

    factors=map1$factors %>%
      bind_rows_safe(map2$factors %>%
                       mutate(map_id=maxmapid+1) %>%
                       mutate(factor_id=factor_id+maxfactorid)
      ),
    links=map1$links  %>%
      bind_rows_safe(map2$links %>%
                       mutate(map_id=maxmapid+1) %>%
                       mutate(from=from+maxfactorid,to=to+maxfactorid) %>%
                       mutate(statement_id=statement_id+maxstatementid)
      ) %>%
      mutate(link_id=row_number()),
    statements=map1$statements  %>%
      bind_rows_safe(map2$statements %>%
                       mutate(map_id=maxmapid+1) %>%
                       mutate(statement_id=statement_id+maxstatementid)
      ),
    sources=map1$sources %>%
      bind_rows_safe(map2$sources %>%
                       mutate(map_id=maxmapid+1)
      ),
    questions=map1$questions %>%
      bind_rows_safe(map2$questions %>%
                       mutate(map_id=maxmapid+1)
      ),
    settings=map1$settings %>%
      bind_rows_safe(map2$settings)
  ) %>% pipe_compact_mapfile()



  # %>%
  #   pipe_clean_map(tables=.)

}

# add_original_ids <- function(graf){
#
#   # browser()
#   graf %>%
#     update_map(
#       .,
#       if(nrow(.$factors)>0)factors <- .$factors %>% mutate(factor_id0=row_number()),
#       if(nrow(.$links)>0)links <- .$links %>% mutate(link_id0=row_number())
#     )
# }





make_igraph_from_links <- function(links){
  links %>% select(from,to) %>% as.matrix()%>% graph_from_edgelist()
}
#' Make igraph (from factors and links)
#'
#' @param factors
#' @param links
#' @param use_labels
#'
#' @return
#' @export
#'
#' @examples
make_igraph <- function(factors,links,use_labels=F){
  # never use this to try to recreate a full map it is too intolerant of extra columsn, just for calculations!

  # browser()
  if(!use_labels){
    factors <- factors %>% select(factor_id,everything())
    links <- links %>% select(from,to,everything()) %>% filter(from %in% factors$factor_id & to %in% factors$factor_id)

  } else {
    factors <- factors %>% select(label,everything())
    links <- links %>% select(from_label,to_label,everything()) %>% filter(from_label %in% factors$label & to_label %in% factors$label)

  }

  # browser()
  graph_from_data_frame(links[,1:2], directed = TRUE, vertices = factors[,1])

}

#'
#' @inheritParams parse_commands
#'
#' @return A mapfile with a additional columns.
#' @export
#'
#'
#' @examples
pipe_remove_orphaned_links <- function(graf){




  factors <- graf$factors
  links <- graf$links
  if(is.null(factors))return(graf)
  if(is.null(links))return(graf)
  if("factor_id" %notin% colnames(factors))return(graf)
  if("from" %notin% colnames(links))return(graf)
  if("to" %notin% colnames(links))return(graf)


  links <- links %>% filter(from %in% factors$factor_id & to %in% factors$factor_id)

  ############ !!!!!!!!!!!!!!!!!!!
  graf <-
    graf %>%
    pipe_update_mapfile(factors=factors,links=links)
  return(graf)
}




pipe_normalise_factors_links <- function(graf){

  factors <- graf$factors
  links <- graf$links
  if(is.null(factors))return(graf)
  if(is.null(links))return(graf)
  if("factor_id" %notin% colnames(factors))return(graf)
  if("from" %notin% colnames(links))return(graf)
  if("to" %notin% colnames(links))return(graf)

  if(identical(factors$factor_id,seq_along(factors$factor_id)))return(graf)


  factors <- factors %>% mutate(oldid__=factor_id,factor_id=1:nrow(factors))
  recodes <- factors$factor_id %>% set_names(factors$oldid__)
  links <- links %>% mutate(from=recode(from,!!!recodes))
  links <- links %>% mutate(to=recode(to,!!!recodes))

  factors <- factors %>% select(-oldid__)

  graf %>%
    pipe_update_mapfile(factors=factors,links=links)

}


#
#
# normalise_id <- function(main,referring,keyname,referring_keyname1=keyname,referring_keyname2=NULL){
#   if(nrow(main)==0)return(list(main=main,referring=referring))
#   if(is.null(main[,keyname])){message("keyname not in main table")}
#   if(is.null(referring[,referring_keyname1])){message("keyname not in referring table")}
#   # browser()
#   if(is.null(main$label))main$label <- main$factor_id
#   # if(length(unique(main[,keyname]))!=nrow(main))
#   main$.old_key <- main[,keyname] %>% unlist
#   main[,keyname] <- 1:nrow(main)
#   recodes=main[,keyname] %>% unlist %>% as.list
#   names(recodes)=main$.old_key %>% unlist
#
#   referring[,keyname] <- referring[,referring_keyname1]
#   referring[,keyname] <- dplyr::recode(referring[,keyname] %>% unlist,!!!recodes)
#   referring[,referring_keyname1] <- referring[,keyname]
#   if(referring_keyname1!=keyname) referring[,keyname] <- NULL
#
#   if(!is.null(referring_keyname2)){
#     referring[,keyname] <- referring[,referring_keyname2]
#     referring[,keyname] <- dplyr::recode(referring[,keyname] %>% unlist,!!!recodes)
#     referring[,referring_keyname2] <- referring[,keyname]
#     if(referring_keyname2!=keyname) referring[,keyname] <- NULL
#   }
#
#
#   return(list(main=main %>% select(-.old_key) %>% mutate(factor_id=row_number()),referring=referring))## factor_id wtf !FIXME
# }

# if factors are duplicates, compact them together
compact_factors_links <- function(factors,links){
  if(factors$label %>% table %>% max %>% `>`(1)){
    message("Some factor labels are duplicates; compacting")
    # browser()
    # browser()
    factors <-
      factors %>%
      group_by(label) %>%
      mutate(new_id=cur_group_id(),
             yesfreq=if_else(as.logical(is_flipped),frequency,0),
             frequency=sum(frequency),
             in_degree=sum(in_degree),
             out_degree=sum(out_degree)
      )

    new_id <- factors$new_id

    links$from <-
      links$from %>% recode(!!!new_id %>% set_names(factors$factor_id))
    links$to <-
      links$to %>% recode(!!!new_id %>% set_names(factors$factor_id))
    # browser()
    factors <-
      factors %>%
      summarise(across(yesfreq,sum),
                across(everything(),first)
      ) %>%
      ungroup %>%
      mutate(is_flipped=(yesfreq/frequency) %>% replace_na(0)) %>%

      mutate(factor_id=new_id
             # ,
             # color.border= div_gradient_pal(ordinary_color,"white",contrary_color)(is_flipped)
      ) %>%
      select(-new_id)

    ## if all werent flipped there is no need to have colour border
    if(sum(factors$is_flipped)==0){
      factors <- factors %>% select(-is_flipped)

    }

  }
  return(list(factors=factors,links=links))
}
#using compact map instead
# pipe_compact_factors_links <- function(mapfile){
#   tmp <-
#     compact_factors_links(mapfile$factors,mapfile$links)
#   assemble_mapfile(
#     tmp$factors,
#     tmp$links,
#     mapfile$statements,
#     mapfile$sources,
#     mapfile$questions
#   )
# }

pipe_compact_mapfile <- function(graf){
  factors <- graf$factors
  links <- graf$links
  tmp <- compact_factors_links(factors,links)
  links <- tmp$links %>% add_simple_bundle_to_links
  pipe_update_mapfile(graf,factors=tmp$factors,links=links)

}


# minor functions ----------------------------------------------------

## Formatting and calculations ------------------------------------------


#' Title
#'
#' @param vec
#'
#' @return
#' @export
#'
#' @examples
as_numeric_if_all <- function(vec){
  if(all(is.na(vec))) return(vec)
  if(!any(is.na(as.numeric(na.omit(vec)) %>% suppressWarnings))) suppressWarnings (as.numeric(vec)) else
    vec
}

full_function_name <- function(df,nam){

  if(is_grouped_df(df) & nam=="unique") "collapse_unique"
  else if(!is_grouped_df(df) & (nam=="unique" | nam=="literal")) "identity"
  else if(nam=="initials")"initials"
  else if(nam=="mean")"getmean"
  else if(nam=="sum")"getsum"
  else if(nam=="median")"getmedian"
  else if(nam=="mode")"getmode"
  else if(nam=="percent")"count_unique"
  else if(nam=="proportion")"count_unique"
  else if(nam=="surprise")"count_unique"
  else if(nam=="count")"count_unique"
  else if(nam=="equals")"=="
  else nam

}

#' Title
#'
#' @param id
#' @param label
#'
#' @return
#' @export
#'
#' @examples
js_button <- function(id,label){
  as.character(shiny::actionButton(inputId = id, label = label,class="linky")) %>% HTML
}

collapse_unique <- function(vec){
  paste0(unique(vec),collapse="; ")# %>% as_numeric_if_all
}
collapse_unique_5 <- function(vec){
  if(length(vec)<6)paste0(unique(vec),collapse="; ") else p("link_quotes","Multiple quotes") %>% as.character # %>% as_numeric_if_all
}

# fun_map <- function(vec,fun){
#   map(vec,~exec(fun,.)) %>% unlist
#
# }
first_map <- function(vec,fun){
  # browser()
  res <- map(vec,first) %>% unlist
  if(is.null(res)) return(vec %>% map(~NA)) else return(vec)

}

literal <- function(lis){
  # browser()

  paste0(lis %>% keep(.!=""),collapse = "; ")
}
initials <- function(lis){
  # browser()
  oldlen <- length(unique(lis))
  nch <- 1
  new <- str_sub(lis,1,nch)

  # shorten so still unique
  while(
    length(unique(str_sub(lis,1,nch)))!=oldlen
  ){
    nch <- nch+1
    new <- str_sub(lis,1,nch)
  }

  oldlen <- length(unique(new))
  # now strip any non-unique leading chars
  # nch <- 1

  # if(length(unique(str_sub(new,5)))==oldlen)return(new)

  if(length(unique(new))==1) return(new)
  # browser()

  # strip identical leading chars
  while(length(unique(str_sub(new,2)))==oldlen
        &
        length(unique(str_sub(new,1,1)))==1 # only strip leading chars if they are the same
  ){

    # if(min(nchar(new))<4) return(new)


    new <- str_sub(new,2)
  }

  new
}


# mode average / most frequent
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmean <- function(v) {mean(as.numeric(v),na.rm=T) %>% round(digits = 2)}
getsum <- function(v) {sum(as.numeric(v),na.rm=T) %>% round(digits = 2)}
getmedian <- function(v) {median(as.numeric(v),na.rm=T) %>% round(digits = 2)}

## from DT package
coerceValue <- function (val, old)
{
  # old=unlist(old)
  if (is.integer(old))
    return(as.integer(val))
  if (is.numeric(old))
    return(as.numeric(val))
  if (is.character(old))
    return(as.character(val))
  if (inherits(old, "Date"))
    return(as.Date(val))
  if (inherits(old, c("POSIXlt", "POSIXct"))) {
    val = strptime(val, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (inherits(old, "POSIXlt"))
      return(val)
    return(as.POSIXct(val))
  }
  if (is.factor(old)) {
    i = val %in% levels(old)
    if (all(i))
      return(val)
    warning("New value(s) \"", paste(val[!i], collapse = ", "),
            "\" not in the original factor levels: \"",
            paste(levels(old), collapse = ", "), "\"; will be coerced to NA.")
    val[!i] = NA
    return(val)
  }
  # warning("The data type is not supported: ", classes(old))
  val
}
# if the vector includes zero, zero should be the midpoint of the result
rescale_with_zero <- function(vec){
  if(
    # max(vec,na.rm=T)>0 &
    min(vec,na.rm=T)<0
  ){
    newvec <- vec %>% abs %>% rescale
    (newvec*(ifelse(vec>0,1,-1))/2)+.5
  }
  else rescale(vec)
}
div_pal_n <- function(vec,lo,hi,mid,pal=1){
  if(min(vec,na.rm=T)<0){
    lo="#4B0092"
    hi="#1AFF1A" # these are colorblind friendly
    mid="#eeeeee"
  } else
  if(pal!=1){
    lo=brewer_pal_n(3,pal)[1]#"#4B0092"
    hi=brewer_pal_n(3,pal)[3]#"#1AFF1A" # these are colorblind friendly
    mid=brewer_pal_n(3,pal)[2]#"#eeeeee"

  }
  div_gradient_pal(low=lo,high=hi,mid=mid)(rescale_with_zero(vec)) %>% alpha(.95)
}
viridis_pal_n <- function(vec){
  vec <- vec %>% as.factor %>% as.numeric
  viridis_pal()(length(unique(vec)))[vec] %>% alpha(.95)
}
#' Title
#'
#' @param vec
#'
#' @return
#' @export
#'
#' @examples
brewer_pal_n <- function(vec,pal){
  vec <- vec %>% as.factor %>% as.numeric
  scales::brewer_pal("qual",palette = pal)(length(unique(vec)))[vec] %>% alpha(.95)
}
create_colors <- function(vec,lo="#FCFDBF",hi="#5F187F",mid="#D3436E",type,field="frequency",fun=NULL,pal=1){
  # browser()
  vec <- as_numeric_if_all(vec)
  if((all(vec %in% 0:1))) res <-ifelse(vec,hi,lo)
      else
    if(class(vec)=="character") res <- brewer_pal_n(vec,pal = as.numeric(pal)) else
    if(lo %in% xc("white gray lightgray")) res <- colour_ramp(c(lo,hi))(rescale(vec)) else
      res <- div_pal_n(vec,lo=lo,hi=hi,mid=mid,pal=pal)

    attr(res,type) <-   list(table=tibble(vec,res) %>% unique,field=field,fun=fun)
    res
}


create_sizes <- function(vec,type,field="frequency",fun=NULL){
  # browser()
  if(is.na(vec) %>% all){
    res <- rep(1,length(vec))
    attr(res,type) <-   list(table=tibble(vec,res) %>% unique,field=field,fun=fun)
  } else {
    mn <- min(vec,na.rm=T)
    mx <- max(vec,na.rm=T)
    res <- scales::rescale(as.numeric(vec),from = {if(mn>0) c(0,mx) else c(mn,mx)},to=c(0.1,1))
    attr(res,type) <-   list(table=tibble(vec,res) %>% unique,field=field,fun=fun)
  }
  res
}

#' Count unique
#'
#' @param vec
#'
#' @return
#' @export
#'
#' @examples
count_unique <- function(vec){
  length(unique(vec %>% keep(.!="")))
}

proportion_false <- function(lis) {
  lis %>% map(~sum(unlist(.))/length(unlist(.)) ) %>% unlist
  # lis %>% map(~sum(unlist(.))/length(.)) %>% unlist
}


color_combined_factors <- function(factors){
  # factors %>%
  #   group_by(label) %>%
  #   mutate(yesfreq=if_else(is_flipped,frequency,0),sumfreq=sum(frequency),is_flipped=yesfreq/sumfreq) %>%
  #   mutate(color.border= div_gradient_pal(ordinary_color,"white",contrary_color)(is_flipped)) %>%
  #   ungroup %>%
  #   select(-yesfreq,-sumfreq)
  # browser()

}
color_combined_links <- function(links){
  links %>% mutate(
    from_color = case_when(
      from_flipped  ~  contrary_color,
      T ~  ordinary_color
    )) %>%
    mutate(
      to_color = case_when(
        to_flipped  ~  contrary_color,
        T ~  ordinary_color
      )) %>%
    mutate(
      color=paste0(from_color,";0.5:",to_color)
    )


}

## Others ------------------------------------------------------------------


has_statements <- function(graf){
  !is.null(graf %>% statements_table)
}


get_all_link_ids <- function(links){
  links %>% select(from,to) %>% unlist %>% unique %>% sort
}

add_statements <- function(graf,statements){
  attr(graf,"statements") <- statements
  graf
}


#' Row index
#'
#' @export
#'
#' @examples
row_index <- function(df)1:nrow(df)
#' Row row
#'
#' @export
#'
#' @examples
row_row <- function(df)row_index(df) %>% map(~df[.,])



to_logical <- function(vec){
  if(vec %>% unique %>% `%in%`(0:1) %>% all) as.logical(vec) else vec
}

from_logical <- function(vec){
  if(vec %>% unique %>% `%in%`(c(F,T)) %>% all) as.numeric(vec) else vec
}


empty_tibble <- tibble(nothing=0)




factor_colnames <- function(graf)graf %>% factors_table %>% colnames
link_colnames <- function(graf)graf %>% links_table %>%  colnames
#
# print_more <- function(graf,n=99){
#   graf$factors %>% as_tibble %>% print(n=n)
#   graf$links %>% as_tibble %>% print(n=n)
# }




zoom_inner <- function(string,lev=1,char=";"){
  # return(string
  # browser()
  string %>%
    map(~str_split(.,char) %>%
          `[[`(1) %>%
          `[`(1:lev) %>%
          keep(!is.na(.)) %>%
          paste0(collapse=char)) %>% unlist
}

relocation_index <- function(vec){
  vec %>% map(function(y)(y==unique(vec)) %>% which %>% min) %>% unlist
}
flip_inner_component <- function(tex,flipchar="~"){
  if_else(str_detect(tex,paste0("^ *",flipchar)),str_remove(tex,paste0("^ *",flipchar)),paste0("~",tex))
}
flip_inner <- function(tex,flipchar="~",sepchar=";"){
  tex %>%
    str_split(sepchar) %>%
    `[[`(1) %>%
    str_trim %>%
    flip_inner_component(flipchar=flipchar) %>%
    paste0(collapse="; ")
}
flip_vector <- function(tex,flipchar="~",sepchar=";"){
  lapply(tex,function(x)flip_inner(x,flipchar=flipchar,sepchar=sepchar)) %>%
    unlist(recursive=F)
}
flip_fix_vector <- function(tex,flipchar="~",sepchar=";"){  # to get always one space between sep and flip
  tex %>%
    str_replace_all(paste0(sepchar," *",flipchar),paste0(sepchar,flipchar)) %>%
    str_replace_all(paste0(sepchar,flipchar," *"),paste0(sepchar,flipchar))
}








cluster_fun <- function(labs,tex){
  ifelse(str_detect(labs,tex),tex,"")
}

calculate_robustness_inner <- function(graf){

  if("found_from" %notin% factor_colnames(graf)) {warning("No found_from column");return(NA)}
  if("found_to" %notin% factor_colnames(graf)) {warning("No found_to column");return(NA)}

  if(nrow(factors_table(graf))==0) {warning("No paths");return(NA)}
  # graf <- graf %>% pipe_bundle_links() #%>% pipe_clean_map()

  # browser()
  graf$links <- graf$links %>%
    group_by(from_label,to_label) %>%
    summarise(capacity=n())


  from_vec <- factors_table(graf) %>% filter(found_from) %>% pull(label)
  to_vec <- factors_table(graf) %>% filter(found_to) %>% pull(label)

  # newnodes <- tibble(
  #   factor_id=c(from_vec,"_super_origin_"))

  fromnewedges <- tibble(
    from_label="_super_origin_",
    to_label=from_vec,
    capacity=Inf
  )
  # newgraf <- assemble_mapfile(newnodes,newedges) %>%
  #   pipe_remove_orphaned_links()



  tonewedges <- tibble(
    to_label="_super_sink_",
    from_label=to_vec,
    capacity=Inf
  )
  # browser()

  new <-
    graf$links %>% select(from_label,to_label,capacity) %>%
    bind_rows(fromnewedges) %>%
    bind_rows(tonewedges) %>%
    factors_links_from_named_edgelist()

  ## need to go back and fetch found_from and to
  graf$factors <-
    new$factors %>% left_join(graf$factors %>% select(label,found_from,found_to)) %>%
    mutate(capacity=+Inf)



  ig <- make_igraph(graf$factors,new$links,use_labels = T)

  origin <- V(ig)[graf$factors$label=="_super_origin_"]
  sink <- V(ig)[graf$factors$label=="_super_sink_"]
  res <- ig %>%
    max_flow(source=origin, target=sink,capacity=new$links$capacity)
  origins <- V(ig)[graf$factors$found_from %>% replace_na(F)]
  sinks <- V(ig)[graf$factors$found_to %>% replace_na(F)]

  if(length(sinks)>1){
    sinkvec <- c(sink,sinks)

    rn <- (graf %>% factors_table %>% filter(found_to) %>% pull(label)) %>% c("All targets",.)
  }else {
    sinkvec <- sinks
    rn <- graf %>% factors_table %>% filter(found_to) %>% pull(label)
  }
  if(length(origins)>1){
    originvec <- c(origin,origins)
    cn <- (graf %>% factors_table %>% filter(found_from) %>% pull(label)) %>% c("All origins",.)

  }
  else {
    cn <- (graf %>% factors_table %>% filter(found_from) %>% pull(label))
    originvec <- origins
  }
  # graf %>% distance_table()
  ## actually this is stupid because you don't need to calculate flow, you can just
  ## look at the distances. However flow is hardly any slower, so why not.
  ## Here is the same thing with distances - it isn't any faster.
  #   if(quick){
  # # browser()
  #   all_flows <-
  #     sinkvec %>% map(function(y)(originvec %>% map(function(x) if(x %in% sinks) Inf else shortest_paths(graf,x,y,mode="out")$vpath %>% pluck(1) %>% length %>% `>`(1))) %>% unlist) %>%
  #     do.call("rbind",.) %>%
  #     as_tibble %>%
  #     mutate_all(as.numeric)
  # }
  #     else
# browser()
if((length(sinkvec)*length(originvec))>1000){message("Tracing paths too large");return(NA)}
  all_flows <-
    sinkvec %>% map(function(y)(
      originvec %>% map(function(x) if(x %in% sinks) Inf else max_flow(ig,x,y,capacity=new$links$capacity)$value)) %>% unlist) %>%
    do.call("rbind",.) %>%
    as_tibble

  all_flows[all_flows==Inf] <- NA

  # note if you don't check for not in sinks, R hangs

  colnames(all_flows) <- cn

  all_flows <- mutate(all_flows, row_names = rn) %>%
    select(row_names,everything())

  return(all_flows)


}

unwrap <- function(str){
  str_replace_all(str,"\n"," ")
}

fill_cols <- function(df,colnames){
  df[,setdiff(colnames, colnames(df))]=0
  df
}

fill_rows <- function(df,rownames){
  df[setdiff(rownames, rownames(df)),]=0
  df
}

make_empty_graf <- function(graf){
  graf %>% pipe_update_mapfile(factors=graf$factors %>% filter(F))
}

# deconstructs then reconstructs the groups same as they were, including the group nominators which have
# already been calculated; simply adds the group baseline to each group
get_percentages <- function(links,field){
  # browser()
  groupvars <- group_vars(links)
  groupvar <- groupvars %>% keep(.!="from" & .!="to")

  links %>%
    ungroup() %>%
    group_by(!!(sym(groupvar))) %>%
    mutate(group_baseline=length(unique(!!(sym(field))))) %>%
    ungroup() %>%
    group_by(!!!(syms(groupvars)))
}

### different from percentages as the group nominators are going to get overwritten; it is the std residuals only which matter
get_surprises <- function(links,field){
  groupvars <- group_vars(links)
  groupvar <- groupvars %>% keep(.!="from" & .!="to")



  stats_by_group <-
    links %>%
    select(from,to,!!(sym(groupvar)),!!(sym(field))) %>%
    # need to fill in missing combinations
    ungroup() %>%
    complete(from,to,!!(sym(groupvar))) %>%
    group_by(!!(sym(groupvar))) %>%
    mutate(group_baseline=length(unique(!!(sym(field))))) %>%
    ungroup() %>%
    group_by(!!!(syms(groupvars))) %>%
    summarise(baseline=first(group_baseline),
              actual=length(unique(!!(sym(field)))),
              nonactual=baseline-actual) %>%
    group_by(from,to) %>%
    mutate(stdres=get_stdres(actual,nonactual)) %>%
    select(!!!(syms(groupvars)),stdres)
  # browser()

  links %>%
    left_join(stats_by_group,by=groupvars)
  # ungroup() %>%

}

get_field <- function(links,field,idlist){
  links %>%
    filter(link_id %in% unlist(idlist)) %>%
    pull(UQ(sym(field))) %>%
    unlist
  # %>%
  #   replace_zero("not found")

}

make_arrowhead=function(vec,dir="forwards"){
  vec <- as.numeric(vec)
  vec %>%
    map(~{
      case_when(
        .==0 ~ "obox",
        .==1 ~ "box",
        .>.5 ~ "lbox",
        .<=.5 ~ "olbox",
        T    ~ "",
      ) %>%
        {if(dir=="forwards") paste0("veenonenone",.) else paste0("nonenone",.)}
    }) %>%
    as.character() %>%
    replace_na("")
}

get_stdres <- function(actual,nonactual,sig=1){
  # browser()
  matrix <- rbind(actual,nonactual) %>% as.matrix(nrow=2)
  if(any(matrix<0)) stop("neg values in stdres")
  matrix[matrix<0] <- 0   ###################### NONONONO
  # browser()
  ch <- chisq.test(matrix,simulate.p.value = T)
  if(ch$p.value<sig)(ch$stdres %>% as.matrix(nrow=2))[1,] else rep(0,ncol(matrix))

}


find_fun <- function(df,field=NULL,value,operator=NULL,what){
  if(is.null(field) & is.null(operator)){
    field="label"
    operator="contains"
  }
  value_original <- value

  if(is.character(df[[field]])){
    # if(field %in% xc("label text from_label to_label")){
    # browser()
    df[[field]] <- replace_na(df[[field]],"")

    value <- value %>% make_search %>% tolower()
  } else if(is.integer(df[[field]])){

    value <- value %>% as.integer
  } else if(is.numeric(df[[field]])){

    value <- value %>% as.numeric
  }

  if(field %notin% colnames(df)) {message("No such field");return(df)}


  if(operator=="contains"){df <- df %>%  mutate(found=str_detect(tolower(unwrap(UQ(sym(field))) %>% replace_na("")),value %>% paste0(collapse="|")))} else
    if(operator=="notcontains"){df <- df %>%  mutate(found=!str_detect(tolower(unwrap(UQ(sym(field))) %>% replace_na("")),value %>% paste0(collapse="|")))} else
      if(operator %in% xc("= equals equal")){df <- df %>%  mutate(found=(make_search(tolower(unwrap(UQ(sym(field))))) %in% value))} else
        if(operator %in% xc("notequals notequal")){df <- df %>%  mutate(found=(tolower(unwrap(UQ(sym(field)))) %notin% value))} else
          if(operator %in% xc("greater")){df <- df %>%  mutate(found=(as.numeric(UQ(sym(field)))>max(as.numeric(value),na.rm=T)))} else
            if(operator %in% xc("less")){df <- df %>%  mutate(found=(as.numeric(UQ(sym(field)))<min(as.numeric(value),na.rm=T)))} else
              if(operator %in% xc("starts start")){df <- df %>%  mutate(found=str_detect(tolower(unwrap(UQ(sym(field)))),paste0("^",value %>% paste0(collapse="|"))))} else
                if(operator %in% xc("ends end")){df <- df %>%  mutate(found=str_detect(tolower(unwrap(UQ(sym(field)))),paste0(value %>% paste0(collapse="|"),"$")))}


  df

}

# exported mapfile utilities ---------------------------------------------------------



#' Extracting tibbles from A mapfile
#'
#' @inheritParams parse_commands
#' @description These three functions extract tables of factors, links or statements from a mapfile.
#' @return A tibble.
#' @name tibbles
NULL
#> NULL

#' @rdname tibbles
#' @export
#'
factors_table <- function(graf)graf$factors

#' @rdname tibbles
#' @export
#'
links_table <- function(graf)graf$links

#' @rdname tibbles
#' @export
#'
statements_table <- function(graf,filter=T){
  graf$statements %>%
    {if(is.null(.)) NULL else
    {if(filter) filter(.,statement_id %in% links_table(graf)$statement_id) else . }}
}
#' @rdname tibbles
#' @export
#'
sources_table <- function(graf,filter=T){
  graf$sources  %>%
    {if(is.null(.)) NULL else
    {if(filter) filter(.,source_id %in% statements_table(graf)$source_id) else . }}
}
#' @rdname tibbles
#' @export
#'
questions_table <- function(graf,filter=T){
  graf$questions %>%
    {if(is.null(.)) NULL else
    {if(filter) filter(.,question_id %in% statements_table(graf)$question_id) else . }}
}
#' @rdname tibbles
#' @export
#'
settings_table <- function(graf){
  graf$settings
}

#' Get table
#'
#' @export
#'
get_table <- function(graf,table_name){
  do.call(paste0(table_name,"_table"),list(graf))
}
#' Get standard table
#'
#' @export
#'
get_standard_table <- function(table_name){
  do.call(paste0("standard_",table_name),list())
}

#' @rdname tibbles
#' @export
#'
links_table_full <- links_table

#' Make mentions table
#'
#' @param graf
#'
#' @return
#' @export
#'
#' @examples
make_mentions_tabl <- function(graf){

  graf$factors <- graf$factors[,colnames(graf$factors)!=""]
  # browser()
  graf$links <- add_labels_to_links(graf$links,factors=graf$factors)
  influence <- graf$links %>% mutate(factor_id=from %>% as.integer,label=from_label,direction="influence")
  consequence <- graf$links %>% mutate(factor_id=to %>% as.integer,label=to_label,direction="consequence")

  either_from <- influence %>% mutate(direction="either")
  either_to <- consequence %>% mutate(direction="either")
  both <- bind_rows(consequence,influence,either_from,either_to)
  graf %>%
    .$factors %>%
    mutate(label=str_replace_all(label,"\n"," ")) %>%
    select(-any_of("id")) %>%
    left_join_safe(both %>% select(-label) ,by=xc("factor_id"),suffix=xc(".factors .links")) %>%
    mutate(mentions="any") %>%  ## this is actually just a hack so we can use this field in the Mentions table
    select(link_id,label,direction,mentions,everything())
}


#' Timestamp
#'
#' @export
#'
time_stamp <- function(){
  Sys.time() %>% format("%Y %m %d %H-%M-%S") %>% str_replace_all(":","-")
}


#' Make search
#'
#' @param x Some text
#' @return The same text but trimmed and with regex escaped
#' @export
#'
make_search <- function(x)x %>% escapeRegex %>% str_trim
# make_search <- function(x)x %>% escapeRegex %>% str_replace_all(" OR ","|") %>% str_trim

# Parser ------------------------------------------------------------------

add_call <- function(graf,lis){
  # browser()
  # attr(graf,"call") <- c(attr(graf,"call"),lis)
  attr(graf,"call") <- c(attr(graf,"call"),lis %>% (function(x)list(x) %>% set_names(x[[1]])))
  graf
}
make_info <- function(graf,lis){
  # return(NULL)
  # message("make info")
  # takes the list - which will usuall be the current call, names it, and adds to existing graf info

  # c(attr(graf,"info"),lis %>% (function(x)list(x) %>% set_names(x[[1]])))
  graf

}

#' Parse line
#'
#' The engine for parse_commands
#' @param graf A mapfile representing a causal map.
#' @param line A line of text to be parsed
#' @return A list containing the function name and a list of parameters.
#' @export
parse_line <- function(line1,graf){
  # browser()
  # message(line1)
  if(str_trim(line1)=="")return()
  fun <- word(line1, 1,2, sep=" ")
  if(is.na(fun)){message("No such function");return()}
  if(!exists(str_replace(fun," ","_") %>% paste0("pipe_",.))){message("No such function");return(NULL)}

  body <-
    str_remove(line1,fun) %>%
    str_trim


  # case: just text nothing else
  if(fun %in% c("find factors") & !str_detect(body,operator_list %>% keep(.!="=") %>% paste0(collapse="|"))){

    # browser()
    updown <- body %>% str_match("(up *([0-9]+) *)*( down *([0-9]+))* *$")
    up <- updown[,3]  %>% as.numeric %>% replace_na(0)
    down <- updown[,5]  %>% as.numeric %>% replace_na(0)
    body <- body %>% str_remove("(up *[0-9]+ *)*( down *[0-9]+)* *$")
    vals=list(
      graf=graf,
      field="label",
      value=body ,
      up=up,
      down=down,

      operator="contains"
    )

  }  else
    if(fun %in% c("find links") & !str_detect(body,operator_list %>% keep(.!="=") %>% paste0(collapse="|"))){

      updown <- body %>% str_match("(up *([0-9]+) *)*( down *([0-9]+))* *$")
      body <- body %>% str_remove("(up *[0-9]+ *)*( down *[0-9]+)* *$")
      vals=list(
        graf=graf,
        field="quote",
        value=body ,

        operator="contains"
      )

    }  else
      if(fun %in% c("find statements") & !str_detect(body,operator_list %>% keep(.!="=") %>% paste0(collapse="|"))){

        updown <- body %>% str_match("(up *([0-9]+) *)*( down *([0-9]+))* *$")
        up <- updown[,3] %>% as.numeric %>% replace_na(0)
        down <- updown[,5] %>% as.numeric %>% replace_na(0)
        body <- body %>% str_remove("(up *[0-9]+ *)*( down *[0-9]+)* *$")
        vals=list(
          graf=graf,
          field="text",
          value=body,

          operator="contains"
        )

      }  else
        # case: field operator value
        if(fun %in% c("find links","find factors") & !str_detect(body,"=")){

          operator <- str_match(body,operator_list %>% keep(.!="=")) %>% na.omit %>% first
          vals=list(
            graf=graf,
            field=body %>% str_extract(paste0("^.*",operator)) %>% str_remove(operator) %>% str_trim,
            value=body %>% str_extract(paste0(operator,".*$")) %>% str_remove(operator) %>% str_trim,


            operator=operator
          )
        }
  # else
  # if(fun %in% c("hide factors") ){
  #   fun <- "find factors"
  #
  #   vals=list(
  #     graf=graf,
  #     field="label",
  #     value=body ,
  #
  #     operator="notcontains"
  #   )
  #
  # }
  else {
    # browser()
    body <-
      body %>%
      str_replace_all(" *=","=") %>%
      str_trim

    vals <-
      body %>%
      str_split("[^ ]*=") %>%
      pluck(1) %>%
      `[`(-1) %>%
      str_trim %>%
      as.list

    fields <-
      body %>%
      str_extract_all("[^ ]*=") %>%
      `[[`(1) %>%
      str_trim %>%
      str_remove_all("=$")

    if(length(fields)!=length(vals)){message("Wrong number of values");return(graf %>% filter(F))}

    names(vals) <- fields
    vals$value <- str_split(vals$value," OR ") %>% unlist
    vals$graf=graf

  }
  fun <- fun %>% str_replace(" ","_") %>% paste0("pipe_",.)
  return(list(fun=fun,vals=vals))
}

#' Parse commands
#'
#' A parser which breaks a text input into individual commands and sends each
#' command to one of the family of pipe_* functions.
#'
#' @param graf A mapfile representing a causal map.
#' A mapfile is a tidygraph, which consists of a table of edges linked to a table of nodes,
#' with an optional additional table of statements.
#' In this package, nodes are called `factors` and edges are called `links.`
#' @param tex A set of commands to parse, separated by linebreaks if there is more than one command.
#' Each line starts with two words corresponding to the name of the pipe function to be applied,
#' e.g. `color links` calls the function `color_links`.
#' The function name is followed by field=value pairs corresponding to the arguments of the function such as `top=10`.
#' Lines beginning with a hash # are treated as comments and ignored.
#'
#' This parser also provides some abbreviated formats.
#' `find links FIELD OPERATOR VALUE` is parsed as `find links field=FIELD operator=OPERATOR value=VALUE`.
#' `find factors FIELD OPERATOR VALUE` is parsed as `find factors field=FIELD operator=OPERATOR value=VALUE`.
#' `search factors TEXT ...` is parsed as `search factors field=label value=TEXT operator=contains`.
#' `search links TEXT ...` is parsed as `search links field=label value=TEXT operator=contains`.
#' `search statements TEXT ...` is parsed as `search statements field=text value=TEXT operator=contains`.
#'
#' @return A mapfile, the result of successively applying the commands to the input graph.
#' @export
#' @examples
#'cashTransferMap %>% parse_commands("select factors top=10 \n color factors field=n") %>% make_vn()
parse_commands <- function(graf=NULL,tex){
  # browser()
  message("parsing")
  tex <- tex %>% replace_null("") %>% str_split("\n") %>% `[[`(1) %>% str_trim() %>% keep(!str_detect(.,"^#"))
  if(length(tex)>1)tex <- tex %>% keep(.!="")

  if(is.null(graf)){
    graf <- tex[1] %>% str_remove("^ *load *map *") %>% load_map()
    if(length(tex)>1)tex <- tex[-1] else tex <- ""

  }
  if(tex[[1]]=="") graf <- graf else {

    for(line in tex){
      # browser()
      if(!str_detect(line,"^#")){tmp <- parse_line(line,graf)

      graf <- possibly(~do.call(tmp$fun,tmp$vals),otherwise=graf)()
      }
    }
  }
  graf
}


# main pipe functions ----------------------------------------------------



#' Find factors
#'
#' @param field Field (column, variable) to search
#' @param value Value to search for
#' @param operator c('contains','notcontains','=','notequals','greater','less','starts','ends').
#' How to search.
#' @param up integer. Default is 0.
#' @param down integer. Default is 0.
#' @description Fields may be from the original data and/or fields created by the app, e.g. `n` (frequency).
#' When field is 'label', 'value' can contain a vector of search terms separated by ' OR '.
#'
#' Text searches are case-insensitive.
#' @return
#' A mapfile containing only matching factors; if `up`!=0 then also factors this number of steps
#' upstream of the matching factors are also included and likewise for `down`!=0.
#' The links are filtered correspondingly to return only the "ego network" i.e. links between the returned factors.

#'
#' If operator and field are both NULL, the value is treated as a simple search string for the field `label`.
#' @export
#'
#' @examples
#' pipe_find_factors(example2,field="label",value="Flood",operator="contains")
#' pipe_find_factors(example2,value="Flood") %>% make_interactive_map
#' pipe_find_factors(example2,value="Flood")
#' pipe_find_factors(example2,field="id",value=5,operator="greater")
pipe_find_factors <- function(graf,field="label",value,operator="contains",up=1,down=1,remove_isolated=F,highlight_only=F){

    message("find factors")

    df <- graf$factors %>% find_fun(field,value,operator)


  if(df$found %>% sum %>% `==`(0)) {
    graf <- (pipe_update_mapfile(graf,factors=graf$factors %>% filter(F),links=graf$links %>% filter(F)))
    return( graf  )
  }



  if(operator=="notcontains" | operator=="notequals"){
    graf <- pipe_update_mapfile(graf,factors=df %>% filter(found))
    return(graf )

  }

  graf <- pipe_update_mapfile(graf,factors=df)

  if(highlight_only){
    graf <- (pipe_update_mapfile(graf,factors=df,links=graf$links))
    return(graf )

  }
    # browser()

  ig <- make_igraph(graf$factors,graf$links)
  trace_after_vec <- ig %>% igraph::distances(to=graf %>% .$factors %>% pull(found),mode="in") %>% apply(1,min) %>% `<=`(down)
  trace_before_vec <- ig %>% igraph::distances(to=graf %>% .$factors %>% pull(found),mode="out") %>% apply(1,min) %>% `<=`(up)
  # browser()
  graf <- {if(any(trace_before_vec)|any(trace_after_vec)){
    graf %>% pipe_update_mapfile(
      factors=graf$factors %>% filter(found|trace_before_vec|trace_after_vec)
      ) %>%
      pipe_remove_isolated_links() %>%
      {if(remove_isolated) pipe_remove_isolated(.) else .}} else{
        graf %>% filter(F)
      }}

  return(graf)
}

#' Find links
#'
#' @inheritParams pipe_find_factors
#' @return A mapfile containing only matching links. Factors are not removed, so this function may return maps with isolated factors,
#' i.e. factors with no links.
#' @export
#'
#' @examples
pipe_find_links <- function(graf,field=NULL,value,operator="contains",remove_isolated=T,highlight_only=F){
  # browser()


  graf$links <- graf$links %>% find_fun(field,value,operator)
  if(!highlight_only) graf$links <- graf$links %>% filter(found)

  if(remove_isolated) graf <- pipe_remove_isolated(graf)

  pipe_recalculate_all(graf)


}

#' Find statements
#'
#' @inheritParams pipe_find_factors
#' @return A mapfile filtered by statements.
#' @export
#'
#' @examples
pipe_find_statements <- function(graf,field,value,operator="=",remove_isolated=T){

  statements <- graf$statements %>% find_fun(field,value,operator)  %>%
    filter(found)
# browser()
  links <- graf$links %>%  filter(statement_id %in% statements$statement_id)

  graf <- (graf %>%
             pipe_update_mapfile(
               links=links,
               statements=statements
             ) %>%
             {if(remove_isolated) pipe_remove_isolated(.) else .}
  ) %>% pipe_normalise_factors_links
  pipe_recalculate_all(graf)



}


#' Select links
#'
#' @inheritParams parse_commands
#' @param top Bundle the links and select only the `top` links in terms of their frequency
#' @param bottom Bundle the links and select only the `bottom` links in terms of their frequency
#' @param all
#' @param is_proportion
#'
#' @return
#' @export
#'
#' @examples
#' select links top=20 should keep all the links within the 20 fattest bundles/sets of links with the most links in each bundle.
#' But previously the algorithm actually combined them into 20 individual links (i.e. create radically fewer actual links)
#' and just remembered the frequency.
#' Now, it keeps the individual links (so a map with select links top =3 might still have 3000 actual links if there were 1000 from A to B
#' and 1000 from B to C and 1000 from C to D. By default, the Interactive and
#' Print maps would indeed combine these into three thick pipes for performance sake, but there would still be 3000 links there somewhere.
pipe_select_links <- function(graf,top=NULL,bottom=NULL){


  links <- graf$links %>%
    unite(bundle,from,to,remove = F,sep = " / ") %>%
    group_by(bundle) %>%
    mutate(.f=n())%>%
    ungroup %>%
    arrange(desc(.f),bundle) %>%
    mutate(.index=(lag(bundle,default="")!=bundle) %>% cumsum()) %>%  #makes a column which increments for each new group
    filter(.index<=as.numeric(top)) %>%
    select(-.index)

  #   graf <- graf %>%
  #     pipe_bundle_links()
  # # browser()
  #   links <- graf$links %>%
  #     arrange(desc(frequency)) %>%
  #     {if(!is.null(top))slice(.,1:top) else slice(.,(nrow_links_table(graf)+1-bottom):nrow_links_table(graf))} %>%
  #     select(from,to,frequency,everything())

  graf <- pipe_update_mapfile(graf,links=links) %>%
    pipe_remove_isolated

  pipe_recalculate_all(graf)

}




nrow_factors_table <- function(graf)
  graf %>% factors_table %>% nrow
nrow_links_table <- function(graf)
  graf %>% links_table %>% nrow

#' Select factors
#'
#' @inheritParams parse_commands
#' @param top Select only the `top` factors in terms of their frequency
#' @param all
#' @param is_proportion
#'
#' @return
#' @export
#'
#' @examples
pipe_select_factors <- function(graf,top=10,bottom=NULL,all=F,field="frequency"){


  graf$factors <-
    factors_table(graf) %>%
    arrange(desc(UQ(sym(field)))) %>%
    {if(!is.null(top))slice(.,1:top) else slice(.,(nrow_factors_table(graf)+1-bottom):nrow_factors_table(graf))} %>%
    arrange(factor_id)

  # browser()

  graf <- graf %>% pipe_remove_isolated_links() %>% pipe_remove_orphaned_links()
  pipe_recalculate_all(graf)




}

#' Remove isolated factors
#'
#' @param graf
#'
#' @return
#' @export
#' @description Removes any factors which have no links.
#' This can be useful after any function like pipe_select_links() which remove links.
#' @examples
pipe_remove_isolated <- function(graf){
  #


  # browser()
  factors <- graf$factors %>%
    filter(factor_id %in% get_all_link_ids(graf$links))

  # tmp <- normalise_id(factors,graf$links,"factor_id","from","to")

  graf <- graf %>%
    pipe_update_mapfile(factors=factors,links=graf$links) %>%
    pipe_remove_isolated_links()

  finalise(graf)#,info)


}

pipe_remove_isolated_links <- function(graf,labels=F){

  # browser()

  if(labels){
    graf <- graf %>%
      pipe_update_mapfile(
        factors=graf$factors,
        links=graf$links %>% filter(from_label %in% graf$factors$label & to_label %in% graf$factors$label))
  }
    else {
    graf <-
      graf %>% pipe_update_mapfile(
        factors=graf$factors,
        links=graf$links %>% filter(from %in% graf$factors$factor_id & to %in% graf$factors$factor_id))
    }
    graf

}

#' @inheritParams parse_commands
#' @return
#' @export
#'
#' @examples
pipe_remove_selfloops <- function(graf){

  # browser()

graf <- graf %>% pipe_update_mapfile(factors=graf$factors,
links=graf$links %>% filter(from!=to))

finalise(graf,info)

}

#' Zoom factors
#'
#' Zoom out from a map, merging factors within the saem hierarchy
#'
#' @inheritParams parse_commands
#' @param level
#' @param separator
#' @param hide
#' @description Another important ability of the Causal Map Viewer is to manipulate maps which use
#' hierarchical coding, which is a very powerful way to code causal information.
#' It is possible to globally by roll up all factors into up to a certain level, for example,
#' to roll them all up to their top level.
#' But it is also possible to selectively zoom individual hierarchies while leaving others intact to any given level.


#' @return
#' @export
#'
#' @examples
pipe_zoom_factors <- function(graf,level=1,separator=";",preserve_frequency=+Inf,frequency_field="frequency",frequency_other="(other)",hide=T){

  level=as.numeric(level)
  hide=as.logical(hide)

# browser()
  preserve_frequency=as.numeric(preserve_frequency)
  if(level>0) graf <-
    graf %>%
    pipe_update_mapfile(
      factors = graf$factors %>%
        mutate(frequency_preserved=UQ(sym(frequency_field))>preserve_frequency) %>%
        mutate(old_label=label,label=if_else(!frequency_preserved & str_detect(old_label,separator),
                                             zoom_inner(old_label,{{level}},separator),old_label),
               frequency=sum(frequency)) %>%
        select(-old_label) %>%
        mutate(frequency_preserved_label=if_else(frequency_preserved,label,"")) %>%

        mutate(label=ifelse(frequency_other!="" & (str_detect(escapeRegex(label %>% list),paste0(escapeRegex(label),";"))),
                            paste0(label," ",frequency_other),
                            label
                            ))
    ) %>%
    pipe_compact_mapfile %>%
    pipe_coerce_mapfile()

  pipe_recalculate_all(graf)


}

#' Title
#'
#' @param graf
#' @param n
#'
#' @return
#' @export
#'
#' @examples
pipe_cluster_sources <- function(graf,n_clusters=3,title="#cluster_set_"){
  if(nrow(graf$links)<10){
    message("Not enough links to cluster")
    return(graf)
  }
  if(nrow(graf$sources)<10){
    message("Not enough sources to cluster")
    return(graf)
  }
  if("all"==(n_clusters)){
    return(
      graf %>%
      pipe_cluster_sources(n_clusters=2,title=title) %>%
      pipe_cluster_sources(n_clusters=3,title=title) %>%
      pipe_cluster_sources(n_clusters=4,title=title)
      )
  }
  # browser()
  if(nrow(graf$sources)<4*n_clusters){
    message("Not enough sources to cluster",3)
    return(graf)
  }
  all_links <-
    graf$links %>%
    dplyr::select(from,to,source_id) %>%
    distinct() %>%
    unite("couple",from,to) %>%
    mutate(dummy=1) %>%
    pivot_wider(id_cols = source_id,names_from = couple,values_from=dummy,values_fill = 0)

  if(nrow(all_links)<4*n_clusters){
    message("Not enough links and sources to cluster",3)
    return(graf)
  }
  sources <- all_links$source_id

  # browser()
  res <- stats::kmeans(all_links %>% dplyr::select(-source_id),centers=n_clusters)
  df <-
    tibble(sources,clus_=res$cluster) %>%
    mutate(letters=letters[clus_])
  colnames(df)[1] <- "source_id"
  colnames(df)[3] <- paste0(title,n_clusters)
  sources <-
    graf$sources %>% left_join(df %>% dplyr::select(-clus_))

  pipe_update_mapfile(graf,sources=sources) %>%
    pipe_coerce_mapfile()

}


#' Bundle factors
#'
#' @inheritParams parse_commands
#' @param value A search string.
#'
#' @return A mapfile in which factors matching the search string are merged into one, with rerouted links.
#' If the search string is empty, factors with the same first word are grouped.
#' @export
#'
#' @examples
pipe_bundle_factors <- function(graf,value=""){

  value <- value %>% make_search %>% paste0(collapse="|")
  graf <- pipe_update_mapfile(graf,factors = graf$factors %>%
                                mutate(
                                  label=if(value[1]=="")
                                    str_match(label,"^[^ ]*") %>% `[`(,1)
                                  else if_else(str_detect(label,value),str_match(label,paste0(value)),label)
                                )
  ) %>%
    pipe_compact_mapfile() %>%
    pipe_coerce_mapfile()

  pipe_recalculate_all(graf)
}



#' Trace robustness
#'
#' @inheritParams parse_commands
#' @param from
#' @param to
#' @param length
#' @param field An optional field by which to split the calculate_robustness calculation and output.
#' @description This is a powerful command which allows the user to trace paths from one or more upstream factors to one or more downstream factors.
#' Only links which are part of such paths are displayed.
#' This function wraps trace_paths and calculate_robustness


#' @return
#' @export
#'
#' @examples
pipe_trace_robustness <- function(graf,from,to,length=4,field=NULL){
  # browser()

  if(field=="")field <- NULL

  if(from=="" | to==""){message("blank from or to factor; robustness calculation may not be correct")}# this should be possible but atm results are horrible
  if(is.null(field)){

    return(graf %>%
             pipe_trace_paths(from=from,to=to,length=length) %>%
             pipe_calculate_robustness()

           # %>%
           #   finalise(info)
    )
  }

  if(field %notin% link_colnames(graf)) {
    warning("Field not found");return(graf %>% make_empty_graf%>%
                                        finalise(info))}
  # actually we only want to calculate the robustness, the map will be the same as if we were not using a field



  # get a vector of all the values of the field
  vec <- graf$links %>%
    pull(UQ(sym(field))) %>%
    unique

  if("" %in% vec){warning("Vector contains an empty string");vec <- vec %>% keep(.!="")}

  res <-
    vec %>%
    set_names %>%
    map(
      function(x) {
        message(x)
        graf %>%
        pipe_find_links(field=field,value=x,operator="equals") %>%
        pipe_trace_paths(from=from,to=to,length=length) %>%
        pipe_calculate_robustness() %>%
        get_robustness
      }
    )

  # browser()
  if(length(res[[1]])==0) {graf <- (graf %>% make_empty_graf);res <- NULL} else{

    res <-
      res %>%
      keep(~!is.na(.[[1]][[1]]))
  }
  if(length(res)==0) {graf <- (graf %>% make_empty_graf);res <- NULL} else{

    field_survivors <- names(res) %>% unique

    res <- res %>% map(~column_to_rownames(.,var="row_names"))

    allcols <-
      map(res,~colnames(.)) %>% unlist %>% unique
    allrows <-
      map(res,~rownames(.)) %>% unlist %>% unique

    res <-
      res %>%
      map(~fill_cols(.,allcols)) %>%
      map(~fill_rows(.,allrows)) %>%
      map(~select(.,any_of("All origins"),everything()))


    res$summary <-
      res %>% map(function(x){x[x>0] <- 1;x} ) %>%
      reduce(`+`) %>%
      mutate_all(~replace_na(.,0)) %>%
      rownames_to_column()
    # browser()
    attr(res$summary,which = "caption") <- field_survivors %>% paste0(collapse=";") %>% paste0("Field: ",field," = ",.)



  }
  # browser()
  info <- c(info,list(flow=res))
  graf %>%
    pipe_trace_paths(from=from,to=to,length=length) %>%   # to get the ordinary map
    pipe_update_mapfile(.,links=add_attribute(.$links,res,"flow"))%>%
    finalise(info)


}

#' Trace paths
#'
#' @inheritParams parse_commands
#' @param from
#' @param to
#' @param length
#' @description This is a powerful command which allows the user to trace paths from one or more upstream factors to one or more downstream factors.
#' Only links which are part of such paths are displayed.


#' @return
#' @export
#'
#' @examples
pipe_trace_paths <- function(graf,from="",to="",length=4,remove_links=F,threads_direction="none",field="source_id",calculate_robustness=F){

  calculate_robustness <- as.logical(calculate_robustness)
# browser()
  tmp <- graf$factors %>%
    select(label,driver_score,outcome_score)


  ######### only working out auto recognition of main_drivers and main_outcomes

  driver_max <- tmp$driver_score %>% max(na.rm=T)
  outcome_max <- tmp$outcome_score %>% max(na.rm=T)

    # browser()
  if(from=="main_drivers") {
    from <- tmp %>% arrange(desc(driver_score)) %>% filter(driver_score>(driver_max/4)) %>% slice(1:3) %>% pull(label) %>% unlist}

  if(to=="main_outcomes") to  <- tmp %>% arrange(desc(outcome_score)) %>% filter(outcome_score>(outcome_max/4)) %>% slice(1:3) %>% pull(label) %>% unlist


  ######### tidy and prepare
  graf$factors <-
    graf$factors %>%
    filter(factor_id %in% get_all_link_ids(graf$links))

  if(is.na(length)) {message("You have to specify length");return(graf)}
  if(0==(length)) {message("You have to specify length greater than 0");return(graf)}

  from <- from %>%  str_replace_all(" OR ","|") %>% str_split(.,"\\|") %>% `[[`(1) %>% map(make_search ) %>% tolower
  to <- to %>%  str_replace_all(" OR ","|") %>% str_split(.,"\\|") %>% `[[`(1) %>% map(make_search ) %>% tolower
  if(from[[1]]=="" & to[[1]]=="") return(graf%>%
                                           pipe_recalculate_all())

  links <- graf$links %>%
    select(from,to,everything())

  ######### which factors match the searches
  graf$factors$found_from <- map(from,~str_detect(tolower(graf$factors$label),.)) %>% as.data.frame %>% rowSums %>% as.logical
  graf$factors$found_to <- map(to,~str_detect(tolower(graf$factors$label),.)) %>% as.data.frame %>% rowSums %>% as.logical

  ######### found type
  factors <- graf$factors %>%
    mutate(found_type=paste0(if_else(found_from,"source","-"),if_else(found_to,"target","-"))) %>%
    mutate(found_any=found_from|found_to)


  if(!any(factors$found_from) | !any(factors$found_to)) return(graf %>% pipe_update_mapfile(factors=graf$factors %>% filter(F))%>%
                                                                 pipe_recalculate_all())

  tmp <- pipe_normalise_factors_links(pipe_update_mapfile(factors=factors,links=links))
  factors <- tmp$factors
  links <- tmp$links


  trace_after_vec <- make_igraph_from_links(links) %>% distances(to=factors %>% pull(found_from),mode="in") %>% apply(1,min,na.rm=T)
  trace_before_vec <- make_igraph_from_links(links) %>% distances(to=factors %>% pull(found_to),mode="out") %>% apply(1,min,na.rm=T)

  # here we need to intervene to make sure that influences don't move closer to the source, as this is a kind of loop

  bothvecsum <- `+`(trace_after_vec,trace_before_vec)
  bothvec <- bothvecsum<=length
  if(min(bothvecsum)<Inf) factors <- factors %>% mutate(bothvecsum=bothvecsum,trace_before_vec=trace_before_vec,
                                                        trace_after_vec=trace_after_vec,
                                                        bothvec,
                                                        found=found_from|found_to
  ) %>% filter(bothvec) else factors <- factors %>% filter(F)


  sums <- factors %>% select(found_from,found_to) %>% colSums(na.rm=T)
  if((sums[1]*sums[2])>10000){
    # if(sum(found_from,na.rm=T)*sum(found_to,na.rm=T)>10){
    message("too much to trace")
    return(graf%>%
             pipe_recalculate_all())
  }



  ##### legacy??
  factors <- factors %>%
    filter(label!="_super_sink_" & label!="_super_origin_")

  ## now we have to make sure we don't also have links between factors where the links are not part of the actual path tracing
  if(remove_links){
    links <-
      links %>% filter(from %in% factors$factor_id)  %>% filter(to %in% factors$factor_id)
  links <-
    links %>%
    left_join(factors %>% select(from=factor_id,frombeforedist=trace_before_vec,fromafterdist=trace_after_vec),by="from") %>%
    left_join(factors %>% select(to=factor_id,tobeforedist=trace_before_vec,toafterdist=trace_after_vec),by="to")

# browser()
  links <-
    links %>%
    filter(
      frombeforedist==tobeforedist+1
      |
      fromafterdist==toafterdist-1
           )
  # links <-
  #   links %>%
  #   left_join(factors %>% select(from=factor_id,fromdist=bothvecsum),by="from") %>%
  #   left_join(factors %>% select(to=factor_id,todist=bothvecsum),by="to") %>%
  #   filter(fromdist>todist)

  factors <-
    factors %>%
    filter(factor_id %in% get_all_link_ids(links))

}
  graf <- pipe_update_mapfile(graf,factors=factors,links=links)
  # browser()
  graf %>%
    pipe_remove_orphaned_links %>%
    pipe_remove_isolated_links %>%
    {if(threads_direction=="up") pipe_trace_threads(.,field = field,direction="up") else .} %>%
    {if(threads_direction=="down") pipe_trace_threads(.,field = field,direction="down") else .} %>%
    {if(calculate_robustness) pipe_trace_robustness(.,from=from,to=to,length=length,field=field) else .} %>%
    finalise(info)



}



#' Calculate robustness
#'
#' @param graf A mapfile. To use this function, the factors table of this map must include
#' two logical variables called `found_from` and `found_to`.
#' If the links table contains an integer column `n`,
#' these values are treated as the capacity of the links, otherwise the capacity for each link is taken as 1.
#' If there are multiple links between any ordered pair of nodes, the links are combined using
#' bundle_links()
#' @param field An optional field by which to split the robustness calculation.
#' @description Uses a maximum flow / minimum cut algorithm (imported from igraph)
#' to calculate the maximum flow / minimum cut from each of the factors for which `found_from` is true
#' to each of the factors for which `found_to` is true.
#'
#' calculate_robustness() is used by pipe_trace_paths().
#'
#' @return A mapfile with an additional attribute `flow`, a tibble (dataframe) in which
#' the columns are each of the factors for which `found_from` is true (if there is
#' more than one such column, and additional "All sources" column is prepended);
#' and in which the rows are each of the factors for which `found_to` is true (if there is
#' more than one such row, and additional "All targets" column is prepended.
#' The (integer) values in the table represent the maximum flow / minimum cut aka Robustness
#' score from the corresponding source factor (column) to the corresponding target factor (row).
#' The scores for the "All sources" column are calculated by constructing an additional factor
#' as an ultimate source which is connected to the other sources by links of infinite capacity,
#' and likewise for the "All targets" row.
#' If `field` is not NULL (or not the empty string), robustness is calculated several times, once for each value of that field.
#' In this case, what is returned is a dataframe which summarises the set of dataframes so that
#' the value of each cell in the returned dataframe is the number of these values for which
#' the robustness value in the corresponden is not zero.
#'
#' The table is sorted in descending order of the first column.
#' @export
#'
#' @examples
#' if(F)cashTransferMap %>% pipe_trace_paths(from="Cash",to="Increa",length=4) %>% pipe_merge_statements %>% pipe_calculate_robustness(field="#SourceID") %>% attr("flow")
pipe_calculate_robustness <- function(graf){


  res <- list()
  if("found_from" %notin% factor_colnames(graf)) {warning("No found_from column");return(graf)}  # if("found_from" %notin% factor_colnames(graf)) {warning("No found_from column");return(graf)}
  if("found_to" %notin% factor_colnames(graf)) {warning("No found_to column");return(graf)}
  # if(field %>% replace_null("")=="")field <- NULL

  # browser()
  res$summary <- calculate_robustness_inner(graf)

  graf %>%
    finalise(.,(info %>% c(list(flow=res)))) #%>% pipe_update_mapfile(.,links=add_attribute(graf$links,res,"flow"))
  # finalise(info) %>% pipe_update_mapfile(.,links=add_attribute(graf$links,res,"flow"))

}


#' pipe_combine_opposites
#'
#' @param graf
#' @param flipchar
#' @param add_colors
#'
#' @return
#' @export
#'
#' @examples
pipe_combine_opposites <- function(graf,flipchar="~",add_colors=T){

  if(add_colors)message("Also adding colours; you can turn this off with 'combine opposites add_colors=FALSE'")
  # browser()
  # old version - bathday
  if(F){factors <-
    graf$factors %>%
    mutate(
      try_flipped=str_detect(label,paste0("^ *",flipchar)),
      try_label=if_else(try_flipped,flip_vector(label,flipchar = flipchar) %>% replace_null(""),label),
      is_top=(!str_detect(label,";")), # becky wanted it flipped anyway if top level
      is_flipped=(is_top | (try_label %in% graf$factors$label)) & try_flipped,
      label=if_else(is_flipped | is_top,flip_fix_vector(try_label) %>% replace_null(""),label)# only flip if to do so would result in a label which already exists
    )
  }
  factors <-
    graf$factors %>%
    mutate(
      is_flipped=str_detect(label,paste0("^ *",flipchar)),
      try_label=if_else(is_flipped,flip_vector(label,flipchar = flipchar) %>% replace_null(""),label),
      label=flip_fix_vector(try_label)
    )



  # %>%
  #   {if(add_colors)color_combined_factors(.) else .}

  # browser()
  links <-
    graf$links

  if(nrow(factors)>0) links <-
    links %>%
    mutate(from_flipped=(recode(from,!!!(factors$is_flipped %>% set_names(factors$factor_id)))) %>% as.logical) %>%
    mutate(to_flipped=(recode(to,!!!(factors$is_flipped %>% set_names(factors$factor_id)))) %>% as.logical) %>%
    unite("flipped_bundle",from_flipped,to_flipped,sep = "|",remove=F) %>%
    {if(add_colors)color_combined_links(.) else .}
  # browser()
  graf %>%
    pipe_update_mapfile(factors=factors,links=links) %>%
    pipe_compact_mapfile()%>%
    pipe_recalculate_all()


}



#' Pipe trace threads
#' @inheritParams parse_commands
#' @param graf A mapfile resulting from pipe_trace_paths or pipe_trace_robustness
#' @param field
#' @description A different approach from robustness: gets added on after a trace paths filter.
#' @return A mapfile with additional links fields contined_down_sid, n_unique_from_before_continued, n_unique_from_before
#' @export
#'
#' @examples
pipe_trace_threads <- function(graf,field="source_id",direction="down"){
  # browser()


  #get the thread ids and put them in the links table for every link
  graf$links$these_ids <- map(graf$links$link_id,~{get_field(graf$links,field,.)}) %>% unlist


  if(direction=="down")trace_threads_down(graf,field="source_id") %>%
  pipe_recalculate_all() else {
    trace_threads_up(graf,field="source_id") %>%
    # browser()
    pipe_recalculate_all()
}

}
trace_threads_down <- function(graf,field="source_id"){

  #get the thread ids and put them in the links table for every link
  graf$links$downstream_threads <- map(graf$links$link_id,~{get_field(graf$links,field,.)}) %>% unlist

  factors <- graf$factors
  links <- graf$links


  # how many steps away is each factor
  if("trace_after_vec" %notin% colnames(factors)) {message("You need to trace paths before tracing continuity",3);return(graf)}

  origins <-                # dont think we need this
    factors %>%
    select(factor_id,trace_after_vec) %>%
    filter(trace_after_vec==0) %>%
    pull(factor_id)

  # the list of each factor id in the map which we will process in turn
  pointers <-
    factors %>%
    select(factor_id,trace_after_vec) %>%
    arrange(trace_after_vec) %>%
    pull(factor_id)


  # starting with the origins, go through each successively further away node and add the downstream threads info to factors and links
  for(node in pointers){
    graf <- graf %>%
      pipe_continue_after(node)

  }

  for_join <-
    graf$links %>% select(factor_id=to,downstream_threads,these_ids) %>%
    group_by(factor_id) %>%
    summarise_all(list) %>%
    mutate(downstream_threads= map(downstream_threads,remove_empty_string)) %>%
    mutate(n_downstream_threads_surviving=map(downstream_threads,~length(unique(.)))%>% unlist) %>%
    mutate(n_downstream_threads=map(these_ids,~length(unique(.)) )%>% unlist)

  ## the only way to get a full colour for factors i.e. max n_downstream_threads_surviving is to create a similar for_join based on from not to.

  graf$factors <-
    graf$factors %>%
    select(-any_of(c("downstream_threads","these_ids","n_downstream_threads_surviving","n_downstream_threads"))) %>%
    left_join(for_join,by="factor_id")%>%
    mutate(n_downstream_threads_surviving=replace_na(n_downstream_threads_surviving,0)) %>%
    mutate(n_downstream_threads=replace_na(n_downstream_threads,0)) %>%
    mutate(
      threads=downstream_threads,
      thread_count=n_downstream_threads_surviving
    )

  # browser()

  graf %>%
    pipe_update_mapfile(.,links=graf$links %>%
                          mutate(has_downstream_threads=downstream_threads!="") %>%
                          mutate(has_threads=has_downstream_threads,
                                 threads=downstream_threads)
                        )
    # pipe_update_mapfile(.,links=graf$links %>% mutate(has_downstream_threads=downstream_threads!="") %>% filter(has_downstream_threads))

}

# takes a graf and a single factor id and calculates the after factor continuity
pipe_continue_after <- function(graf,node){
  factors <- graf$factors
  links <- graf$links

  if(factors$trace_after_vec[factors$factor_id==node]==0) {
    # it is the origin
    graf

  }
  else {

    # this is so expensive because it goes through the whole links table
    pipe_update_mapfile(graf,links=links %>%
                          mutate(downstream_threads=map(link_id,~continue_after(links,.,node)) %>% unlist)   # only calculate for the current node
    )
  }

}
continue_after <- function(links,link_id,node){

  # two tests: 1) does this source appear in the predecessors? if so, is it a live continuation?
  this <- links$link_id==link_id
  if(!any(links$from[this] %in% node)) links$downstream_threads[this] else {
    current_id <- links$these_ids[this]
    previous_link_ids <- unlist(links$before_id[this])

    predecessors <- links$downstream_threads[links$link_id %in% previous_link_ids]
    if(any(current_id %in% predecessors)) {
      current_id
    } else ""
  }
}


## same but you need to replace up/down to/from as well as before/after
pipe_reverse_map <- function(graf){
  factors <- graf$factors %>% rename(trace_after_vec=trace_before_vec,trace_before_vec=trace_after_vec)
  links <- graf$links %>% rename(from=to,to=from,from_label=to_label,to_label=from_label,from_flipped=to_flipped,to_flipped=from_flipped,
                                 before_id=after_id,after_id=before_id)
  pipe_update_mapfile(graf,factors=factors,links=links)
}
trace_threads_up <- function(graf,field="source_id"){

  # browser()
  graf %>% pipe_reverse_map %>% trace_threads_down(field=field) %>% pipe_reverse_map()

}





OLDtrace_threads_up <- function(graf,field="source_id"){

  #get the thread ids and put them in the links table for every link
  graf$links$upstream_threads <- map(graf$links$link_id,~{get_field(graf$links,field,.)}) %>% unlist

  factors <- graf$factors
  links <- graf$links


  # how many steps away is each factor
  if("trace_before_vec" %notin% colnames(factors)) {message("You need to trace paths before tracing continuity",3);return(graf)}

  origins <-
    factors %>%
    select(factor_id,trace_before_vec) %>%
    filter(trace_before_vec==0) %>%
    pull(factor_id)

  # the list of each factor id which we will process in turn
  pointers <-
    factors %>%
    select(factor_id,trace_before_vec) %>%
    arrange(trace_before_vec) %>%
    pull(factor_id)


  for(node in pointers){
    graf <- graf %>%
      pipe_continue_before(node)

  }

  for_join <-
    graf$links %>% select(factor_id=from,upstream_threads,these_ids) %>%
    group_by(factor_id) %>%
    summarise_all(list) %>%
    mutate(upstream_threads= map(upstream_threads,remove_empty_string)) %>%
    mutate(n_upstream_threads_surviving=map(upstream_threads,~length(unique(.)))%>% unlist) %>%
    mutate(n_upstream_threads=map(these_ids,~length(unique(.)) )%>% unlist)

  ## the only way to get a full colour for factors i.e. max n_upstream_threads_surviving is to create a similar for_join based on from not to.

  graf$factors <-
    graf$factors %>%
    select(-any_of(c("upstream_threads","these_ids","n_upstream_threads_surviving","n_upstream_threads"))) %>%
    left_join(for_join,by="factor_id")%>%
    mutate(n_upstream_threads_surviving=replace_na(n_upstream_threads_surviving,0)) %>%
    mutate(n_upstream_threads=replace_na(n_upstream_threads,0))

  graf %>%
    pipe_update_mapfile(.,links=graf$links %>% mutate(has_upstream_threads=upstream_threads!=""))

}

# takes a graf and a single factor id and calculates the before factor continuity
pipe_continue_before <- function(graf,node){
  factors <- graf$factors
  links <- graf$links

  if(factors$trace_before_vec[factors$factor_id==node]==0) {
    # it is the origin
    graf

  }
  else {

    # this is so expensive because it goes through the whole links table
    pipe_update_mapfile(graf,links=links %>%
     mutate(upstream_threads=map(link_id,~continue_before(links,.,node)) %>% unlist)   # only calculate for the current node
    )
  }

}
continue_before <- function(links,link_id,node){

  # two tests: 1) does this source appear in the predecessors? if so, is it a live continuation?
  this <- links$link_id==link_id
  if(!any(links$to[this] %in% node)) links$upstream_threads[this] else {
    current_id <- links$these_ids[this]
    previous_link_ids <- unlist(links$before_id[this])

    predecessors <- links$upstream_threads[links$link_id %in% previous_link_ids]
    if(any(current_id %in% predecessors)) {
      current_id
    } else ""
  }
}


remove_empty_string <- function(vec){
  vec %>% keep(.!="")
}

#' Pipe merge map
#' @inheritParams parse_commands
#' @param graf
#' @param path
#' @description A wrapper around merge_mapfile to make it work in the app.
#' @return A tidy map. The column *_map_id is set to reflect the id of the map.
#' @export
#'
#' @examples
pipe_merge_mapfile <- function(graf,path){
  # browser()


  map2 <- load_mapfile(path=path)
  graf <- graf
  merge_mapfile(graf,map2)%>%
    pipe_recalculate_all()


}

#' Remove bracketed expressions
#'
#' @inheritParams parse_commands
#' @param value c("[","(")
#'
#' @return A mapfile in which the factor labels have had any text enclosed with square brackets or round brackets removed, along with the brackets.
#'
#' @export
#'
#'
#' @examples
pipe_remove_brackets <- function(graf,value="["){


  if(value=="[")graf <- graf %>% pipe_update_mapfile(factors=graf$factors %>%
                                                       mutate(label=str_remove_all(label,"\\[.*?\\]")))


  else if(value=="(")graf <- graf %>% pipe_update_mapfile(factors=graf$factors %>%
                                                            mutate(label=str_remove_all(label,"\\(.*?\\)")))

  pipe_recalculate_all(graf)

}
# zero_to_one <- function(vec)(vec-min(vec,na.rm=T))/(max(vec,na.rm=T)-min(vec,na.rm=T))


#' Title
#'
#' @param graf
#' @param value
#'
#' @return
#' @export
#'
#' @examples
pipe_hide_quickfields <- function(graf,value="["){

  graf <-
    graf %>%
    pipe_update_mapfile(factors=graf$factors %>%
                          mutate(label=str_remove_all(label,";* */* *[:alnum:]*\\:[:alnum:]*"))
                        # %>%
                        # mutate(label=str_remove_all(label," *; *;")) %>%
                        # mutate(label=str_remove_all(label," *; *$"))
    ) %>%
    pipe_compact_mapfile()

  pipe_recalculate_all(graf)
}

## add conditional formats -------------------------------------------------------------

#' Bundle links
#'
#' @inheritParams parse_commands
#' @param field
#'
#' @return A mapfile in which sets of coterminal, same-direction links are replaced with
#' one link (when `field` = 'n') or more than one link for each of the values of `field`
#' present in the data. In each case, each new link has a field n representing the number
#' of links it is replacing, unless the links it is replacing already had values n in which
#' case the new value of `n` is the sum of the `n` values of the constituent links.
#' @export
#'
#' @examples
#' # Showing separate (bundled) links for women and men:
#' if(F)cashTransferMap %>% pipe_merge_statements() %>%  pipe_select_factors(10) %>% pipe_bundle_links(counter="frequency",group="1. Sex")%>% pipe_label_links(field = "frequency") %>% pipe_color_links(field="1. Sex") %>% pipe_scale_links() %>%  make_print_map()
#' # or, counting sources rather than statements:
#' if(F)cashTransferMap %>% pipe_merge_statements() %>%  pipe_select_factors(10) %>% pipe_bundle_links(group="1. Sex",counter="#SourceID")%>% pipe_label_links(field = "frequency") %>% pipe_color_links(field="1. Sex") %>% pipe_scale_links() %>%  make_print_map()
pipe_bundle_links <- function(graf,field=NULL,group=field){

    # browser()
  if(is.null(group))group <- "simple_bundle"

  links <- graf$links %>% ungroup
  coln <- colnames(links)
  group <- coln[str_detect(coln,paste0(group))][1]
  if(is.na(group)) {message("no such counter");return(graf)}else
  # if(group %>% replace_null("from") %notin% coln) {message("no such counter");return(graf)}else

  {
# browser()
    return(pipe_update_mapfile(graf,
                               links=links %>%
                                 {if(is.null(group))group_by(.,from,to) else group_by(.,from,to,!!(sym(group)))} %>%
                                 add_attribute(group,attr = "group"))%>%
             finalise(info))


  }
}


#' Scale factors
#'
#' @inheritParams parse_commands
#' @param field A numerical field in the factor table which will control the scale.
#'
#' @return A mapfile with a new or overwritten column `size`in the factor table varying between .2 and 1.
#' @export
#'
#'
#' @examples
pipe_scale_factors <- function(graf,field="frequency"){


  # browser()
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  class <- graf %>% factors_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  graf %>%
    pipe_update_mapfile(factors=graf$factors %>% mutate(size=create_sizes(UQ(sym(field)),field=field,type="scale_factors")))%>%
    finalise(info)

}
#' Scale factors
#'
#' @inheritParams parse_commands
#' @param field A numerical field in the link table which will control the scale (the width of the links).
#'
#' @return A mapfile with a new or overwritten column `width`in the link table varying between .2 and 1.
#' @export
#'
#' @examples
pipe_scale_links <- function(graf,field="link_id",fixed=NULL,fun="count",value=NULL){



  if(!is.null(value)){
    tmp <- str_match(value,"(^.*?):(.*)")
    fun <- tmp[,2] %>% str_trim
    field <- tmp[,3] %>% str_trim
  }

  links <- graf$links
  oldfun <- fun
  fun <- full_function_name(links,fun)


  if(!is.null(fixed))return(graf  %>% pipe_update_mapfile(links=links %>%  mutate(width=fixed)))
  if(!is_grouped_df(links)) {
    did_group <- T
    links <- links %>% group_by(from,to)
  }else did_group <- F

  if(field %notin% colnames(links)){warning("No such column");return(graf)}

  # browser()


  links <- links %>% mutate(width=exec(fun,!!sym(field)))
  if(oldfun=="percent"){
# browser()
    links <- get_percentages(links,field)
    links$width=100*links$width/links$group_baseline

  }
  if(oldfun=="surprise"){

    links <- get_surprises(links,field)
    links$width=100*links$width/links$group_baseline

  }
  links$width=create_sizes(links$width,type="scale_links",field=field,fun=oldfun)


  if(did_group) {
    links <- links %>% ungroup
  }

  graf  %>% pipe_update_mapfile(links=links)%>%
    finalise(info)

}


#' Label factors
#'
#' @inheritParams parse_commands
#' @param field A numerical or character field in the factor table.
#' @param clear Logical. Whether to clear any existing labels or to concatenate the new result after
#' any existing labels. Default is `FALSE`.
#'
#' @return A mapfile with a column `label`. If `clear` is FALSE, the new label is concatenated
#' after any existing label. The new label is of the form `field: value`.
#' @description For example it is possible to add the factor frequency to the factor labels.
#'  More than one label can be added: Labels are additive and are applied one after the other.
#'  Other formatting commands such as factor and link colour can either specify a fixed colour (`links colour blue`)
#'  or can be used conditionally, so that a the colour or transparency of links can depend on any custom field existing in the original data
#'  such as, say, `gender` but also on fields created by the app e.g. frequency.

#' @export
#'
#'
#' @examples
pipe_label_factors <- function(graf,field="frequency",clear_previous=F,add_field_name=T,clear=clear_previous){


  # browser()
  clear_previous=as.logical(clear_previous)
  # graf <- pipe_metrics(graf)
  if(field %notin% colnames(graf$factors)){warning("No such column");return(graf)}

  graf %>%
    pipe_update_mapfile(
      factors=graf$factors %>%
        mutate(label=paste0(
          (if(clear_previous)NULL else paste0(label,". ")) %>% keep(.!=""),if(add_field_name)paste0(field,": "),UQ(sym(field)),if(add_field_name)". ")
          )
      )
}


pipe_dash_links <- function(graf,field="hashtags",operator="notequals",value="",type="dashed"){
  if(is.null(value))return(graf)
  if(is.null(field))return(graf)
  if(is.null(operator))return(graf)
  if(is.na(field)){warning("No such column");return(graf)}
  if(type %notin% xc("dotted dashed")){warning("No such style");return(graf)}
  if(field %notin% colnames(links)){warning("No such column");return(graf)}

  # browser()
  links <-
    graf$links %>%
    find_fun(field=field,value=value,operator=operator) %>%
    mutate(style=ifelse(found,type,"solid"))



  # browser()
  graf %>% pipe_update_mapfile(links=links)

}

#' Label links
#'
#' @inheritParams parse_commands
#' @param field A numerical or character field in the link table.
#' @param clear Logical. Whether to clear any existing labels or to concatenate the new result after
#' any existing labels.
#'
#' @return A mapfile with a column `label`. If `clear` is FALSE (the default), the new label is concatenated
#' after any existing label. The new label is of the form `field: value`.
#' @export
#'
#'
#' @examples
pipe_label_links <- function(graf,field="link_id",fun="count",value=NULL,add_field_name=F,clear_previous=T){

  # browser()
# browser()
  if(!is.null(value)){
    tmp <- str_match(value,"(^.*?):(.*)")
    fun <- tmp[,2] %>% str_trim
    field <- tmp[,3] %>% str_trim
    add_field_name <- as.logical(add_field_name)
    clear_previous <- as.logical(clear_previous)
  }
  # clear=as.logical(clear)
  clear_previous=as.logical(clear_previous)
  links <- graf$links

  coln <- colnames(links)
  field <- coln[str_detect(coln,paste0(field))][1]


  if(is.na(field)){warning("No such column");return(graf)}
  if(field %notin% colnames(links)){warning("No such column");return(graf)}
  # if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
  oldfun <- fun
  fun <- full_function_name(graf,fun)

  #if(!is_grouped_df(links)) links <- links %>% group_by(from,to)
  if(!is_grouped_df(links)) {
    did_group <- T
    links <- links %>% group_by(from,to)
  } else did_group <- F

  # browser()
  links <- links %>%
    mutate(new_link_label=exec(fun,!!sym(field)))

  if(oldfun=="initials"){
    # browser()
    links$new_link_label <- initials(links[,field] %>% unlist %>% unname)

    # links$new_link_label=format(100*as.numeric(links$link_label)/links$group_baseline,digits=0) %>% paste0(links$link_label," (",.,"%)")
  }
  if(oldfun=="percent"){

    # browser()
    links <- get_percentages(links,field)

    links$new_link_label=format(100*as.numeric(links$new_link_label)/links$group_baseline,digits=1) %>% paste0("%")
  }
  if(oldfun=="proportion"){

    # browser()
    links <- get_percentages(links,field)

    links$new_link_label=paste0(links$new_link_label,"/",links$group_baseline)
  }
  if(oldfun=="surprise"){
    links <- get_surprises(links,field)

    links$new_link_label=format(as.numeric(links$stdres),digits=2) %>% paste0(links$link_label," (",.,")")
  }

  # links$link_label <- exec(fun,links[[field]])

  if(did_group) {
    links <- links %>% ungroup
  }
  # browser()
  if(add_field_name){
    links <-
      links %>%
      mutate(new_link_label=paste0(field,": ",new_link_label))
  }

  if(!clear_previous) {
    links <-
      links %>%
      unite(link_label,new_link_label,link_label,sep=", ")
  } else {
    links <-
      links %>%
      mutate(link_label=new_link_label) %>%
      select(-new_link_label)

  }

  graf %>% pipe_update_mapfile(links=links)%>%
    finalise(info)
}



#' Title
#'
#' @param graf
#' @param field
#' @param add_field_name
#' @param show_number
#'
#' @return
#' @export
#'
#' @examples
pipe_mark_links <- function(graf,field="source_id",add_field_name=F,show_number=F){

  links <- graf$links
  ogroups <- groups(links)
  if(length(ogroups)==0){
    ogroups <- "simple_bundle"
    groups <-"simple_bundle"
  } else {
    groups <- ogroups %>% setdiff(c("from","to")) %>% unlist %>% pluck(1) %>% replace_null("simple_bundle")
    ogroups <- as.character(as.vector(ogroups))

  }

  # add head labels
  # browser()
  links <-
    links %>% group_by(to) %>%
    arrange(to_label,from,to) %>%
    mutate(head3=UQ(sym(groups))!=lag(UQ(sym(groups)))) %>%
    mutate(head3=replace_na(F)) %>%
    mutate(headlabel=cumsum(head3)) %>%
    # mutate(headlabel=letters[head3]) %>%
    group_by(from,to,UQ(sym(groups))) %>%
    # add tail labels
    mutate(allsources=list(UQ(sym(field)) %>% unique))

  links$headlabel <- letters[links$headlabel+1]
  inlinks <-
    links %>% summarise(headlabelin=first(headlabel),allsourcesin=unique(allsources)) %>%
    mutate(factor=to) %>%
    ungroup %>%
    select(-ogroups)

  outlinks <-
    links %>% summarise(headlabelout=first(headlabel),allsourcesout=unique(allsources)) %>%
    mutate(factor=from) %>%
    ungroup %>%
    select(-ogroups)
  #select(-from,-to,-flipped_bundle)

  tmp <-
    outlinks %>%
    left_join(inlinks,by="factor") %>%
    group_by(factor,headlabelout,headlabelin) %>%
    select(factor,headlabelout,everything()) %>%
    arrange(factor,headlabelout) %>%
    mutate(continues=length(intersect(unlist(allsourcesin),unlist(allsourcesout)))>0) %>%
    group_by(factor,headlabelout) %>%
    filter(!is.na(headlabelin)) %>%
    filter(continues) %>%
    mutate(taillabel=paste0(headlabelin %>% unique,collapse=",")) %>%
    ungroup %>%
    select("from"=factor,"headlabel"=headlabelout,taillabel)
  # this nearly works
  # mutate(x=length(intersect(unlist(allsourcesin),unlist((allsourcesout))))) %>%
  #   mutate(y=length(unlist(unique(allsourcesout)))) %>%
  #   mutate(continuity=(x/y) %>% round(2)) %>%
  #

  links <-
    links %>%
    select(-allsources,-head3) %>%
    left_join(tmp,by=c("from","headlabel")) %>%
    mutate(taillabel=replace_na(taillabel,"")) %>%
    mutate(headlabel=if_else(to %in% links$from,headlabel,""))

  graf %>% pipe_update_mapfile(links=links)%>%
    finalise(info)
}


#' Title
#'
#' @param graf
#' @param field
#'
#' @return
#' @export
#'
#' @examples
pipe_show_continuity <- function(graf,field="source_id",type="arrowtype"){


  links <- graf$links


  links$before_sources <- map(links$before_id,~{get_field(links,field,.)})
  links$after_sources <- map(links$after_id,~{get_field(links,field,.)})

  links <-
    links %>%
    mutate(these_sources = (UQ(sym(field))))

  if(!is_grouped_df(links)){
    tmp <-
      links %>%
      group_by(link_id)
  } else tmp <- links


  tmp <-
    tmp  %>%
    mutate(x=length(intersect(unlist(before_sources),unlist((these_sources))))) %>%
    mutate(y=length(unlist(unique(these_sources)))) %>%
    mutate(downstream_continuity=(x/y) %>% round(1) %>% as.character) %>%
    mutate(x=length(intersect(unlist(after_sources),unlist((these_sources))))) %>%
    mutate(upstream_continuity=(x/y) %>% round(1) %>% as.character) %>%
    mutate(nullbc=length(unlist(before_sources))==0) %>%
    mutate(downstream_continuity=if_else(nullbc,"",downstream_continuity)) %>%
    mutate(nullac=length(unlist(after_sources))==0) %>%
    mutate(upstream_continuity=if_else(nullac,"",upstream_continuity))%>%
    ungroup %>%
    select(link_id,downstream_continuity,upstream_continuity)
  # select(-these_sources,-before_sources,-after_sources,-x,-y,nullac,-nullbc)

  # browser()
  links <-
    links %>%
    left_join(tmp,by="link_id")

  if(type=="label"){
    links <-
      links %>%
      mutate(taillabel=downstream_continuity) %>%
      mutate(headlabel=upstream_continuity)
    # mutate(taillabel=if_else(downstream_continuity=="0","0",downstream_continuity)) %>%
    # mutate(headlabel=if_else(upstream_continuity=="0","0",upstream_continuity))
  } else
  {
    links <-
      links %>%
      mutate(arrowtail=make_arrowhead(downstream_continuity,dir="backwards")) %>%
      mutate(arrowhead=make_arrowhead(upstream_continuity))
  }



  # with arrowheads, we don't want to display them on links with no predecssors or successors.
  # but with colours, it is better if the values are 1 for NA
  links <-
    links %>%
    mutate(downstream_continuity=as.numeric(downstream_continuity),downstream_continuity=replace_na(downstream_continuity,1)) %>%
    mutate(upstream_continuity=as.numeric(upstream_continuity),upstream_continuity=replace_na(upstream_continuity,1))


  graf %>% pipe_update_mapfile(links=links)%>%
    finalise(info)
}


#' Color factors (background color)
#'
#' @inheritParams parse_commands
#' @param field A numerical or character field in the factor table.
#' If it is character, the other parameters are ignored and a color-blind friendly qualitative palette is applied.
#' @param fixed  Optionally, a color specification which will be applied everywhere and overrides `field`.
#' @param lo Optionally, a color specification for the low end of the color range. Default is `green`.
#' @param hi Optionally, a color specification for the high end of the color range. Default is `blue`.
#' @param mid  Optionally, a color specification for the middle of the color range. Default is `gray`.
#'
#' @return A mapfile with a new or overwritten column `color.background`in the factor table.
#' @export
#'
#'
#' @examples
pipe_color_factors <- function(graf,field="frequency",lo="#FCFDBF",hi="#9F189F",mid="#D5438E",fixed=NULL,pal=1){


  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf%>%
                                                                             finalise(info))}
  # browser()
  if(!is.null(fixed)) factors <- graf$factors %>%
      mutate(color.background=fixed) else  factors <- graf$factors %>%
          mutate(color.background=create_colors(UQ(sym(field)),lo=lo,hi=hi,mid=mid,type="color_factors",field=field,pal=pal))
      graf %>% pipe_update_mapfile(factors=factors)%>%
        finalise(info)
}
#' Color factors (border color)
#'
#' @inheritParams parse_commands
#' @param field A numerical or character field in the factor table.
#' #' If it is character, the other parameters are ignored and a color-blind friendly qualitative palette is applied.
#' @param fixed  Optionally, a color specification which will be applied everywhere and overrides `field`.
#' @param lo Optionally, a color specification for the low end of the color range. Default is `green`.
#' @param hi Optionally, a color specification for the high end of the color range. Default is `blue`.
#' @param mid  Optionally, a color specification for the middle of the color range. Default is `gray`.
#'
#' @return A mapfile with a new or overwritten column `color.border`in the factor table.
#' @export
#'
#'
#' @examples
pipe_color_borders <- function(graf,field="frequency",lo="#FCFDBF",hi="#5F187F",mid="#D3436E",fixed=NULL,pal=1){


  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf%>%
                                                                             finalise(info))}

  # browser()
  if(!is.null(fixed)) factors <- graf$factors %>% mutate(color.border=fixed) else
    factors <- graf$factors %>%
      mutate(color.border=create_colors(UQ(sym(field)),lo=lo,hi=hi,mid=mid,type="color_borders",field=field,pal=pal))
  graf %>% pipe_update_mapfile(factors=factors)%>%
    finalise(info)
}
#' Font Color factors
#'
#' @inheritParams parse_commands
#' @param field A numerical or character field in the factor table.
#' #' If it is character, the other parameters are ignored and a color-blind friendly qualitative palette is applied.
#' @param fixed  Optionally, a color specification which will be applied everywhere and overrides `field`.
#' @param lo Optionally, a color specification for the low end of the color range. Default is `green`.
#' @param hi Optionally, a color specification for the high end of the color range. Default is `blue`.
#' @param mid  Optionally, a color specification for the middle of the color range. Default is `gray`.
#'
#' @return A mapfile with a new or overwritten column `color.border`in the factor table.
#' @export
#'
#'
#' @examples
pipe_color_text <- function(graf,field="frequency",lo="#FCFDBF",hi="#5F187F",mid="#D3436E",fixed=NULL,pal=1){


  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf%>%
                                                                             finalise(info))}

  # browser()
  if(!is.null(fixed)) factors <- graf$factors %>% mutate(font.color=fixed) else
    factors <- graf$factors %>%
      mutate(font.color=create_colors(UQ(sym(field)),lo=lo,hi=hi,mid=mid,type="color_text",field=field,pal=pal))

  # fix for when all are red
  if(field=="is_opposable" &(!any(str_detect(factors$label,"~")))){
    if(!any(factors$is_opposable))
      factors$font.color="black"
  }
  graf %>% pipe_update_mapfile(factors=factors)%>%
    finalise(info)
}

#' Color links
#'
#' @inheritParams parse_commands
#' @param field A numerical or character field in the link table.
#' If it is character, the other parameters are ignored and a color-blind friendly qualitative palette is applied.
#' @param fixed  Optionally, a color specification which will be applied everywhere and overrides `field`.
#' @param lo Optionally, a color specification for the low end of the color range. Default is `green`.
#' @param hi Optionally, a color specification for the high end of the color range. Default is `blue`.
#' @param mid  Optionally, a color specification for the middle of the color range. Default is `gray`.
#'
#' @return A mapfile with a new or overwritten column `color`in the link table.
#' @export
#'
#'
#' @examples
pipe_color_links <- function(graf,field="link_id",lo="#FCFDBF",hi="#5F187F",mid="#D3436E",fixed=NULL,fun="count",value=NULL,pal=1){


  # browser()
  if(!is.null(value)){
    tmp <- str_match(value,"(^.*?):(.*)")
    fun <- tmp[,2] %>% str_trim
    field <- tmp[,3] %>% str_trim
  }
  if(!is.null(fixed)){
    links <- graf$links %>%
      mutate(color=fixed)

  } else{

    if(field %notin% link_colnames(graf)){warning("No such column");return(graf%>%
                                                                             finalise(info))}
    links <- graf$links
    oldfun <- fun
    fun <- full_function_name(links,fun)


    if(field %notin% colnames(links)){warning("No such column");return(graf%>%
                                                                         finalise(info))}

    if(!is_grouped_df(links)) {
      did_group <- T
      links <- links %>% group_by(from,to)
    }else did_group <- F

    links <- links %>% mutate(color=exec(fun,!!sym(field)))
    if(oldfun=="percent" | oldfun=="proportion"){
      # browser()
      links <- get_percentages(links,field)
      links$color=100*links$color/links$group_baseline
    }
    if(oldfun=="surprise"){
      links <- get_surprises(links,field)
      links$color=links$stdres
    }

    links$color=create_colors(links$color,type="color_links",lo=lo,mid=mid,hi=hi,field=field,fun=oldfun,pal=pal)

    if(did_group) {
      links <- links %>% ungroup
    }

  }

  graf  %>% pipe_update_mapfile(links=links)%>%
    finalise(info)

}




# add simple formats ------------------------------------------------------

#' Cluster factors
#'
#' @inheritParams parse_commands


#' @return
#' @export
#'
#' @examples
pipe_cluster_factors <- function(graf,clusters=NULL){




  if(!is.null(clusters)) {

    choices <- clusters %>% escapeRegex %>% str_split(" OR ") %>% `[[`(1) %>% str_trim

    if(length(choices)>1){
      for(c in choices){
        graf <- pipe_cluster_factors(graf,c)
      }
      return(graf)
    } else {


      #ifelse(str_detect(labs,tex),tex,"")

      nodes <- factors_table(graf)
      # browser()
      # old <- nodes$cluster

      if("cluster" %notin% colnames(nodes)) nodes$cluster <- ""

      nodes <-
        nodes %>%
        mutate(cluster=if_else(str_detect(label,choices),choices,cluster))


      # nodes$cluster <- choices %>%
      #   map(~cluster_fun(nodes$label,.)) %>%
      #   as.data.frame() %>%
      #   apply(1,function(x)paste0(x,collapse=""))
      #
      # if(!is.null(old))nodes$cluster[nodes$cluster==""] <- old    # if the current loop couldn't find anything, replace with maybe an existing cluster from a previous pass
      nodes <- nodes %>%
        mutate(cluster=ifelse(cluster=="",NA,cluster))    %>%
        mutate(cluster=str_remove_all(cluster,"\\\\"))
      graf %>% pipe_update_mapfile(factors=graf$factors %>% mutate(cluster = nodes$cluster))%>%
        finalise(info)
    }
  } else graf%>%
    finalise(info)
}


#' Wrap factor labels
#'
#' @inheritParams parse_commands
#' @param length line length
#'
#' @return A mapfile with factor labels wrapped to `length`
#' @export
#'
#'
#' @examples
pipe_wrap_factors <- function(graf,length=20){


  graf %>%
    pipe_update_mapfile(
      factors=graf$factors %>%
        mutate(factor_wrap=length)
    )%>%
    finalise(info)
}
#' Wrap link labels
#'
#' @inheritParams parse_commands
#' @param length line length
#'
#' @return A mapfile with link labels wrapped to `length`
#' @export
#'
#'
#' @examples
pipe_wrap_links <- function(graf,length=20){


  # browser()
  graf %>%
    pipe_update_mapfile(links=
                          graf$links %>%
                          mutate(link_wrap=length)
    )%>%
    finalise(info)
}

# outputs -----------------------------------------------------------------



factor_click_origin <- function(id){
  js_button(paste0('factor_click_origin_',id), label = "Trace from here (origin)")
}
factor_click_target <- function(id){
  js_button(paste0('factor_click_target_',id), label = "Trace to here (target)")
}
factor_click_focus <- function(id){
  js_button(paste0('factor_click_focus_',id), label = "Focus")
}

factor_click_edit <- function(id){
  js_button(paste0('factor_click_edit_',id), label = "Rename")
}
factor_click_delete <- function(id){
  js_button(paste0('factor_click_delete_',id), label = "Delete factor across the whole map")
}
factor_click_edit <- function(id){
  js_button(paste0('factor_click_edit_',id), label = "Edit factor")
}
factor_click_name <- function(val){
  as.character(shiny::textInput(inputId = paste0('factor_click_name'),
                                label = NULL,value=val))
}
# factor_click_name <- function(val){
#   as.character(shiny::textInput(inputId = paste0('factor_click_name'),
#                                 label = NULL,value=val,onclick= 'Shiny.onInputChange("factor_click_name", Math.random()'))
# }
link_click_delete <- function(id){
  if(str_detect(id,";"))"" else as.character(shiny::actionLink(inputId = paste0('link_click_delete_',id), label = 'Delete link',class='linky'))
}
link_click_edit <- function(id){
  if(str_detect(id,";"))as.character(div('This bundle consists of multiple original links'))else
    as.character(shiny::actionLink(inputId = paste0(
      'link_click_edit_',id), label = 'Edit link',class='linky'))
}
link_click_statement_go <- function(id){
  if(str_detect(id,";")) "" else as.character(shiny::actionLink(inputId = paste0(
      'statement_go_',id), label = 'Go to statement',class='linky'))
}
link_click_source_go <- function(id){
  if(is.na(id))return(div())
  if(str_detect(id,";")) "" else as.character(shiny::actionLink(inputId = paste0(
      'source_go_',id), label = 'Go to source',class='linky'))
}



fixed <- function(...) 2L
fixed_string <- function(...)""

#' NOT USED Prepare visual bundles
#'
#' @param graf
#' @param group
#' @param color_field
#' @param color_fun
#' @param size_field
#' @param size_fun
#' @param label_field
#' @param label_fun
#' @param lo
#' @param mid
#' @param hi
#'
#' @return
#' @export
#'
#' @examples
prepare_visual_bundles <- function(graf,
                                   group=NULL,
                                   color_field=NULL,
                                   color_fun=NULL,
                                   size_field=NULL,
                                   size_fun=NULL,
                                   label_field=NULL,
                                   label_fun=NULL,
                                   lo="",
                                   mid="",
                                   hi=""
){
  if(is.null(graf))return(graf)
  if(group %>% replace_null("")=="")group <- "link_id"
  # browser()
  if((group) %notin% colnames(graf$links))return(graf)

  if(lo%>% replace_null("")=="" )lo <- "white"
  if(mid%>% replace_null("")=="" )mid <- "white"
  if(hi%>% replace_null("")=="" )hi <- "green"
  if(color_field%>% replace_null("")=="" )color_field <- "link_id"
  if(size_field%>% replace_null("")=="" )size_field <- "link_id"
  if(label_field%>% replace_null("")=="" )label_field <- "link_id"
  if(color_fun %>% replace_null("")=="")color_fun <- "fixed"
  if(color_fun=="mean")color_fun <- "getmean"
  if(color_fun=="sum")color_fun <- "getsum"
  if(color_fun=="median")color_fun <- "getmedian"
  if(color_fun=="mode")color_fun <- "getmode"
  if(color_fun=="count")color_fun <- "count_unique"
  if(size_fun %>% replace_null("")=="")size_fun <- "fixed"
  if(size_fun=="mean")size_fun <- "getmean"
  if(size_fun=="sum")size_fun <- "getsum"
  if(size_fun=="median")size_fun <- "getmedian"
  if(size_fun=="mode")size_fun <- "getmode"
  if(size_fun=="count")size_fun <- "count_unique"
  if(label_fun %>% replace_null("")=="")label_fun <- "fixed_string"
  if(label_fun=="mean")label_fun <- "getmean"
  if(label_fun=="sum")label_fun <- "getsum"
  if(label_fun=="median")label_fun <- "getmedian"
  if(label_fun=="mode")label_fun <- "getmode"
  if(label_fun=="count")label_fun <- "count_unique"

  # browser()
  links <-
    graf$links %>% {if(is.null(group))group_by(.,from,to) else group_by(.,from,to,!!(sym(group)))} %>%
    mutate(
      frequency=length(link_id),
      color=exec(color_fun,!!sym(color_field)),
      width=exec(size_fun,!!sym(size_field)),
      color=create_colors(color,type="color_links",lo=lo,mid=mid,hi=hi,field=color_field),
      width=create_sizes(as.numeric(width),type="size_links",field=size_field) %>% as.numeric,
      link_label=exec(label_fun,!!sym(label_field))
    ) %>%
    summarise_all(collapse_unique) %>%
    ungroup
  # %>%
  #   mutate(
  #   )
  # if("size_fun"!="fixed"){

  # }
  pipe_update_mapfile(graf,links=links)

}

## visNetwork --------------------------------------------------------------

#' Make a visNetwork
#' @description Make a visNetwork (https://datastorm-open.github.io/visNetwork/) from a mapfile.
#'
#' @param graf A mapfile. The factors table and links table may contain additional formatting information like color.background.
#' @param scale Increase from the default 1 to make the map taller.
#'
#' @return A visnetwork
#' @export
#'
#'
#' @examples
make_interactive_map <- function(graf,scale=1,safe_limit=200,rainbow=F){
# browser()
  if(nrow(graf$factors %>% replace_null(tibble()))==0) return(empty_visnetwork)

  message("making interactive map")
  graf <- prepare_final(graf)

    if(nrow(graf$factors)>0){  if(max(table(graf$factors$size),na.rm=T)>1){

    graf$factors <- graf$factors %>% arrange((size))
  }
  }
  nodes <- graf$factors %>% mutate(value=size*10) %>%
    select(any_of(xc("factor_id factor_memo factor_wrap label font.color color.background color.border title group value hidden size"))) %>% ### restrictive in attempt to reduce random freezes
    fix_columns_factors()

  if("factor_wrap" %in% colnames(nodes))nodes <- nodes %>%
    mutate(label=str_wrap(label,max(factor_wrap))) else nodes <-
    nodes %>%     mutate(label=add_default_wrap(label) )

  edges <- graf$links %>% select(-any_of("label")) %>% rename(label=link_label) %>%
    fix_columns_links()

  if("link_wrap" %in% colnames(edges))edges <- edges %>%
    mutate(label=str_wrap(label,max(link_wrap))) else edges <-
    edges %>%     mutate(label=add_default_wrap(label) )

  #message("3vn")

    # browser()
  if(is_grouped_df(edges)){
    # NOTE IF YOU THOUGHT YOU HAD WRAPPED LINKS, IT WILL FAIL HERE #TODO
    edges <- edges %>% summarise(across(color,~average_color(.,combine_doubles = T)),across(quote,collapse_unique_5),across(everything(),collapse_unique)) %>%
      ungroup
  }
  # browser()

  #message("4vn")


  if(rainbow){
    edges$color <- rainbow(nrow(edges),s=.33,v=1)[1:nrow(edges)]

  } else {


    edges$color <-
      case_when(
        edges$color=="#058488;0.5:#058488" ~ ordinary_color,
        edges$color=="#f26d04;0.5:#f26d04" ~ ordinary_color,
        edges$color=="#058488;0.5:#f26d04" ~ contrary_color,
        edges$color=="#f26d04;0.5:#058488" ~ contrary_color,
        T ~ edges$color
      )
  }
  #message("5vn")

  edges$width <- as.numeric(edges$width)
  edges$label[is.na(edges$label )] <- ""
  edges$label["NA"==(edges$label )] <- ""


  # browser()
  islist <- lapply(edges,is.list) %>% unlist
  if(nrow(edges)>0) edges[,islist] <- edges[,islist] %>% mutate_all(first_map)## in case there are any list columns left*****************
  edges <-  edges %>% vn_fan_edges()
  if(is.list(edges$width))edges$width=2 else edges$width=edges$width*10
  edges <-  edges  %>%
    select(any_of(xc("from to style id source_id statement_id question_id quote color hashtags width label link_memo smooth.roundness smooth.enabled smooth.type link_id")))
  if(nrow(nodes)>1){
    layout <- layout_with_sugiyama(make_igraph(nodes,edges))$layout*-scale
    colnames(layout) <- c("y", "x")
    nodes <- data.frame(nodes, layout)
    ############## don't get tempted to use the internal visnetwork layout functions - problems with fitting to screen, and they are slower ....
  }
  edges$dashes <- edges$style!="solid"
  #message("6vn")
  nodes <- nodes %>%   mutate(id=factor_id)
  edges <- edges %>%   mutate(id=NULL) # id would be necessary for getting ids from clicks etc, but seems to stop tooltip from working
  # browser()
  if(nrow(nodes)<100)nodes <-
    nodes %>% mutate(title=paste0(
      map(label,identity),
      # map(label,factor_click_name),
      # " ",
      # map(factor_id0,factor_click_edit),
      # # factor_click_name(),
      # "</br>",
      "</br>",
      map(factor_id,factor_click_focus),
      "</br>",
      map(factor_id,factor_click_origin),
      "</br>",
      map(factor_id,factor_click_target),
      "</br>",
      map(factor_id,factor_click_edit),
      "</br>",
      map(factor_id,factor_click_delete),
      "</br>",
      paste0("Memo:", factor_memo),
      "</br>",
      paste0("ID:", factor_id)
    ))
  # browser()
  #message("7vn")
  if(nrow(nodes)<100)edges <-
    edges %>% mutate(title=paste0(
      map(statement_id,link_click_statement_go),
      "</br>",
      map(source_id,link_click_source_go),
      "</br>",
      map(link_id,link_click_edit),
      "</br>",
      map(link_id,link_click_delete)
      ,
      "<p class='link_tooltip'>Memo:", link_memo  %>% str_wrap,"</p>",
      "<p class='link_tooltip'>Source ID:", source_id  %>% str_wrap,"</p>",
      "<p class='link_tooltip'>Statement ID:", statement_id  %>% str_wrap,"</p>",
      "<p class='link_tooltip'>Hashtags:", hashtags   %>% str_wrap,"</p>",
      "<p class='link_tooltip'>Question ID:", question_id  %>% str_wrap,"</p>",
      "</br><p class='link_tooltip'>",quote %>% str_replace_all(";","</br>") %>% str_wrap,"</p>"
    ))
  #message("8vn")
  res <- visNetwork(nodes,edges,background="white")   %>%
    visNodes(
      shadow = list(enabled = T, size = 10),
      shape = "box"
      ,
      font=list(color="black")
      ,
      borderWidth=2
      ,
      scaling = list(label = list(enabled = T,min=14,max=30))# shouldn't need min and max but font not visible otherwise
      ,
      physics = T
    ) %>%
    visEdges(
      smooth = F,
      arrowStrikethrough = T,
      physics = F,
      shadow=T
      ,
      arrows =
        list(to = T)
    ) %>%
    # visIgraphLayout("layout_nicely",type="full",physics=T,randomSeed = 123) %>%  #remember that the native igraph sugiyama layout can cause hard freezes
    # visIgraphLayout("layout_with_sugiyama",type="full",physics=T,randomSeed = 123) %>%  #remember that the native igraph sugiyama layout can cause hard freezes
    # you have to have physics=T or it won't fit on first drawing
    # visEvents(type = "once", startStabilizing = "function() { this.fit({nodes:1})}") %>%
    visExport(type = "png", name = "export-network",
              float = "right", label = "Save image", background = "whitesmoke", style= "") %>%

    visInteraction(
      dragNodes = T,
      # hover =T,
      dragView = T,
      zoomView = T,
      navigationButtons = T,
      selectable = T,
      multiselect = T,
      keyboard = F, # would be nice for navigation but interferes with text editing
      selectConnectedEdges = F,
      tooltipStay=0,
      tooltipDelay=100,
      tooltipStyle='color:red;position: fixed;visibility:hidden;width:100px;background-color:aliceblue'
    ) %>%

    visEvents(click ="function(data) {
                Shiny.onInputChange('net_factor_click', data.nodes);
                Shiny.onInputChange('net_link_click', data.edges);
                ;}",
              select = "function(data) {
                Shiny.onInputChange('net_factor_selected', data.nodes);
                Shiny.onInputChange('net_link_selected', data.edges);
                ;}",
              hoverEdge = "function(edges) {
                Shiny.onInputChange('net_link_hover', edges);
                ;}"
              ,hoverNode = "function(nodes) {
                Shiny.onInputChange('net_factor_hover', nodes);
                ;}",
              blurNode = "function(nodes) {
                Shiny.onInputChange('net_factor_click', null);
                ;}"
              # ,
              # blurEdge = "function(edges) {
              #   Shiny.onInputChange('net_link_click', null);
              #   ;}"
    )  %>%
    visPhysics(stabilization = T,
               solver = "barnesHut", barnesHut = list(avoidOverlap = 0.9,centralGravity=0.4,damping=0.9 ),
               maxVelocity=25
    ) %>% # ,solver="hierarchicalRepulsion") %>% #,solver="hierarchicalRepulsion") %>%
    visOptions(
      collapse = F,
      manipulation=F
      ,
      highlightNearest = list(
        enabled = F,
        degree = list(from = 5, to = 5), # if (find_setting("diagramdownarrows",vals) %>% as.logical()) list(from = 0, to = 19) else list(from = 19, to = 0),
        hover = T,
        labelOnly = T,
        # hideColor = "green",
        algorithm = "hierarchical"
      ),
      nodesIdSelection = list(enabled=T,values=nodes %>% arrange(label) %>% pull(id))
    )
  # # %>%
  #     visIgraphLayout(layout = "layout_with_sugiyama", randomSeed = 123, type = "full")

  message("made interactive map")

  res
}


vn_fan_edges <- function(edges){
  # browser()
  if(nrow(edges)==0)return(edges)
  edges %>%
    mutate(a=ifelse(from>to,from,to),b=ifelse(from>to,to,from)) %>%
    unite(smooth_index,a,b,remove=F) %>%  # need to cope with arrows coming the other way as well
    group_by(smooth_index) %>%
    mutate(smooth.type = "continuous") %>%   #'straightCross  dynamic', 'continuous', 'discrete', 'diagonalCross', 'straightCross', 'horizontal', 'vertical', 'curlinksCW', 'curlinksCCW', 'cubicBezier'.
    mutate(n=n(),rounded=n>1 ,smooth.roundness = ifelse(rounded,rnorm(n,0,.2),0)) %>% #%>% pmin(ifelse(input$bundle_links,0,.9)))  %>%
    mutate(smooth.enabled = rounded) %>%
    mutate(smooth.type = "diagonalCross") %>%
    ungroup()

}





## grviz -------------------------------------------------------------------


#' Settings for a Graphviz map
#' @description Graphviz map: https://graphviz.org/documentation/
#' @param graf A mapfile. Link and factor tables may contain columns to control formatting
#' such as `color.border`.
#' @param maxwidth
#' @param grv_layout What layout to use. Default is `dot`.
#' @param grv_splines How to create splines. See Graphviz documentation.
#' @param grv_overlap See Graphviz documentation.
#' @param color Default font color
#' @param ranksep_slider
#' @param nodesep_slider
#' @param safe_limit Integer. Large maps with many edges can take a long time to layout.
#' If !is.null(safe_limit), the resulting map is simplified by bundling edges and selecting
#' most frequent factors.
#'
#' @return
#' @export
#'
#' @examples
pipe_set_print <- function(
  graf=NULL,
  maxwidth=NULL,
  grv_layout=NULL,
  grv_splines=NULL,
  grv_overlap=NULL,
  color=NULL,
  ranksep_slider=NULL,
  nodesep_slider=NULL,
  safe_limit=NULL
  # grv_layout="dot",
  # grv_splines ="splines",
  # grv_overlap=F,
  # color="grey",
  # ranksep_slider=3,
  # nodesep_slider=20,
  # safe_limit=200

){
  graf %>%
    add_attribute(
      list(
        maxwidth=maxwidth,
        grv_layout=grv_layout,
        grv_splines=grv_splines,
        grv_overlap=grv_overlap,
        color=color,
        ranksep_slider=ranksep_slider,
        nodesep_slider=nodesep_slider,
        safe_limit=safe_limit
      ),
      attr="set_print"
    )
}

  add_default_wrap <- function(labelvec){
  # browser()
  if(!any(str_detect(labelvec %>% na.omit,"\n"))) str_wrap(labelvec,22) else labelvec
}

# combine doubles deals with double colours of the form red:blue, then recursively works out the average
average_color <- function(colvec,combine_doubles=F){
  if(any(str_detect(colvec,":"))){

    if(combine_doubles){
      # browser()
      # SOMETHING NOT RIGHT TODO
      res <- map(colvec,~str_split(.,":") %>% unlist %>% average_color) %>% average_color
      return(res)
    }

    res <- str_split(colvec,":") %>% as.data.frame %>% t %>% as.data.frame %>% lapply(.,function(x)average_color(x)) %>% unlist
    return(paste0(res,collapse=";0.5:"))
  }
  colvec <- str_sub(colvec,1,7)  # in case there is any alpha stuff at the end
  av <- col2rgb(colvec) %>% rowMeans() %>% `/`(255)
  rgb(av[1],av[2],av[3])

}

prepare_final <- function(graf){

  if(nrow(graf$factors)>replace_null(safe_limit,200)){
    message("Map larger than 'safe limit'; showing only most frequent factors",3)
    graf <- graf %>%
      pipe_select_factors(200)

  }


  if(!is_grouped_df(graf$links) & nrow(links_table(graf))>replace_null(safe_limit,200)){
    message("Map larger than 'safe limit'; bundling and labelling links",3)
    graf <- graf %>%
      pipe_bundle_links() %>%
      pipe_label_links(field = "link_id",fun="count") %>%
      pipe_scale_links(field = "link_id",fun="count")

  }
  if("is_flipped" %in% colnames(graf$factors)){
  if(any(as.numeric(graf$factors$is_flipped)>0,na.rm=T) %>% replace_na(F))graf$factors$`color.border`= div_gradient_pal(ordinary_color,"white",contrary_color)(graf$factors$is_flipped)

  }



  graf


}

#' Make a Graphviz map
#' @description Make a Graphviz map: https://graphviz.org/documentation/
#' @param graf A mapfile. Link and factor tables may contain columns to control formatting
#' such as `color.border`.
#' @param maxwidth
#' @param grv_layout What layout to use. Default is `dot`.
#' @param grv_splines How to create splines. See Graphviz documentation.
#' @param grv_overlap See Graphviz documentation.
#' @param color Default font color
#' @param ranksep_slider
#' @param nodesep_slider
#' @param safe_limit Integer. Large maps with many edges can take a long time to layout.
#' If !is.null(safe_limit), the resulting map is simplified by bundling edges and selecting
#' most frequent factors.
#'
#' @return
#' @export
#'
#' @examples
make_print_map <- function(
  graf=NULL,
  maxwidth=NULL,
  grv_layout=NULL,
  grv_splines=NULL,
  grv_overlap=NULL,
  color=NULL,
  rainbow=F,
  graph_title="",
  ranksep_slider=NULL,
  nodesep_slider=NULL,
  safe_limit=NULL

){


  if(nrow(graf$factors %>% replace_null(tibble()))==0) return(DiagrammeR::create_graph() %>%  DiagrammeR::render_graph())

  graf <- prepare_final(graf)

  # browser()



  safe_limit <- replace_null(safe_limit,graf %>% attr("set_print") %>% .$safe_limit %>% replace_null(200))
  maxwidth <- replace_null(maxwidth,graf %>% attr("set_print") %>% .$maxwidth %>% replace_null("dot"))
  grv_layout <- replace_null(grv_layout,
                             graf %>% attr("set_print") %>% .$grv_layout %>% replace_null(
                               if_else(nrow(graf %>% factors_table)>safe_limit/3,"twopi","dot")))
  grv_splines <- replace_null(grv_splines,graf %>% attr("set_print") %>% .$grv_splines %>% replace_null(if_else(nrow(graf %>% factors_table)>safe_limit/3,"lines","splines")))
  grv_overlap <- replace_null(grv_overlap,graf %>% attr("set_print") %>% .$grv_overlap %>% replace_null(F))
  color <- replace_null(color,graf %>% attr("set_print") %>% .$color %>% replace_null("grey"))
  ranksep_slider <- replace_null(ranksep_slider,graf %>% attr("set_print") %>% .$ranksep_slider %>% replace_null(3))
  nodesep <- replace_null(nodesep_slider,graf %>% attr("set_print") %>% .$nodesep_slider %>% replace_null(2))
  ranksep <- ranksep_slider %>% replace_null(2*log(nrow(factors_table(graf))))
  graf <- pipe_normalise_factors_links(graf)


  if(is.null(graf))return()
  if(nrow(graf$factors)==0)return()
  # graf <- graf %>% pipe_fix_columns()

  if(!is_grouped_df(graf$links) & !is.null(safe_limit) & nrow(links_table(graf))>replace_null(safe_limit,200)){
    message("Map larger than 'safe limit'; bundling and labelling links")
    graf <- graf %>%
      pipe_update_mapfile(factors=fix_columns_factors(graf$factors),links=fix_columns_links(graf$links)) %>%
      pipe_bundle_links() %>%
      pipe_label_links(field = "link_id",fun="count") %>%
      pipe_scale_links(field = "link_id",fun="count")

    # browser()

    # if(nrow(factors_table(graf))>safe_limit) graf <- graf %>% pipe_select_factors(safe_limit/10)
  }


  # if("frequency" %in% colnames(links_table(graf)))graf <-  graf %>% mutate(tooltip=as.character(n))
  # browser()
  factors <-
    graf$factors %>%
    fix_columns_factors() %>%
    mutate(label=clean_grv(label) )%>%
    mutate(label=add_default_wrap(label) )%>%
    # mutate(cluster=if_else(is.na(cluster),"",cluster) )%>%
    # tooltip causes error with panzoom    mutate(tooltip= clean_grv(label)) %>%   # seemed to cause intermittent error!!!
    mutate(fillcolor=color.background) %>%
    mutate(color=color.border) %>%
    mutate(penwidth=3) %>% #if_else(color.border %>% unique %>% length %>% `==`(1),0,14)) %>% # if borders are all same colour, don't print border
    mutate(fontsize=(size+3)*5) %>%
    mutate(fontcolor=font.color) %>%
    # mutate(xlabel="blue") %>%
    select(any_of(xc("label size tooltip factor_wrap fillcolor color fontsize fontcolor cluster penwidth")))

  # %>%
  #   mutate(factor_id=factor_id)

  if(is.null(factors$cluster))factors$cluster <- ""

  links <- graf$links %>%
    fix_columns_links()

  links$link_label[is.na(links$link_label)] <- ""


  if(is_grouped_df(links)){
    links <-
      links %>% summarise(across(color,average_color),across(everything(),collapse_unique)) %>%
      ungroup
  } else
    if(rainbow){
      links$color <- rainbow(nrow(links),s=.33,v=1)[1:nrow(links)]

    }

    # browser()
  # mutate_all(first_map)%>%
  links <- links %>%
    select(any_of(xc("from to style color link_wrap simple_bundle width link_label width from_label headlabel taillabel arrowhead arrowtail to_label"))) %>%
    rename(label=link_label) %>%
    mutate(from=as.numeric(from))%>%
    mutate(to=as.numeric(to))%>%
    mutate(label=if_else(label=="","     .     ",as.character(label)))%>%
    mutate(label=clean_grv(label) )%>%
    mutate(label=replace_na(label,"     .     "))%>% # obscure! if all are =="", error
    mutate(width=as.numeric(width))%>%
    mutate(penwidth=(width+.01)*5)%>%
    # mutate(fontcolor=color)%>%
    # mutate(arrowsize=width) %>% #(3+(width*9))) %>%
    mutate(tooltip=clean_grv(simple_bundle))
  # browser()

  if(all(is.na(factors$cluster)))factors$cluster=NULL
  if(all(factors$cluster==""))factors$cluster=NULL
  # browser()
  # factors$cluster <- factors$cluster %>% replace_na("")

  if("factor_wrap" %in% colnames(factors))factors <- factors %>%
    mutate(label=str_wrap(label,max(factor_wrap))) else factors <-
    factors %>%     mutate(label=add_default_wrap(label) )

  edges <- graf$links %>% select(-any_of("label")) %>% rename(label=link_label) %>%
    fix_columns_links()

  if("link_wrap" %in% colnames(edges))edges <- edges %>%
    mutate(label=str_wrap(label,max(link_wrap))) else edges <-
    edges %>%     mutate(label=add_default_wrap(label) )

  # browser()

  grv <-
    DiagrammeR::create_graph() %>%
    add_nodes_from_table(factors  %>% mutate(id=row_number()),label_col="label") %>%
    {if(nrow(links)>0) add_edges_from_table(.,links,from_col="from",to_col="to",from_to_map = id_external) else .}%>%

    add_global_graph_attrs("label", graph_title, "graph") %>%
    add_global_graph_attrs("layout", grv_layout, "graph") %>%
    add_global_graph_attrs("splines", grv_splines, "graph") %>%
    add_global_graph_attrs("overlap", grv_overlap, "graph") %>%
    add_global_graph_attrs("labelloc", "bottom","graph") %>%
    add_global_graph_attrs("labeljust", "right","graph") %>%
    add_global_graph_attrs("outputorder", "nodesfirst","graph") %>%
    add_global_graph_attrs("tooltip", " ", "graph") %>%
    add_global_graph_attrs("rankdir", "LR", "graph") %>%
    # add_global_graph_attrs("fontsize", "24", "graph") %>%
    add_global_graph_attrs("fontname", "Arial", "graph") %>%
    add_global_graph_attrs("nodesep", as.numeric(nodesep)/8, "graph") %>%
    add_global_graph_attrs("ranksep", as.numeric(ranksep)/8, "graph") %>%
    add_global_graph_attrs("style", "filled,dashed", "graph") %>%
    add_global_graph_attrs("color", color, "graph") %>%
    add_global_graph_attrs("fillcolor", color, "graph") %>%

    add_global_graph_attrs("shape", "box", "node") %>%
    add_global_graph_attrs("style", "rounded, filled", "node") %>%
    add_global_graph_attrs("fixedsize", "false", "node") %>%
    add_global_graph_attrs("margin", "0.19", "node") %>%
    add_global_graph_attrs("width", "0", "node") %>%
    add_global_graph_attrs("height", "0", "node")  %>%


    # add_global_graph_attrs("arrowhead", "vee", "edge")    %>%
    add_global_graph_attrs("arrowtail", "none", "edge")    %>%
    add_global_graph_attrs("dir", "both", "edge")    %>%
    add_global_graph_attrs("style", "solid", "edge")    %>%
    add_global_graph_attrs("fontsize", 14, "edge")    %>%
    add_global_graph_attrs("forcelabels", T, "graph")
  # browser()
  return(
    grv %>% DiagrammeR::render_graph() %>% add_attribute(as.character(nrow(factors)),"n_nodes")
  )

}

clean_grv <- function(tx){
  tx %>% str_replace_all("'","&rsquo;") %>%
    str_replace_all("\"","&rsquo;") %>%
    str_replace_all("‘","&rsquo;") %>%
    str_replace_all("’","&rsquo;") %>%
    # strip_symbols() %>%
    str_replace_all("\"","'") %>%
    simplify_unicode
}


simplify_unicode <- function(texvec){
  texvec %>%
    str_replace_all("\u008d","'") %>%
    str_replace_all("\U008d","'") %>%
    str_replace_all("\u0085","-") %>%
    str_replace_all("\u0085","-") %>%
    str_replace_all("\u008e","'") %>%
    str_replace_all("\U008e","'") %>%
    str_replace_all("\u0092","`") %>%
    str_replace_all("\u008f","'") %>%
    str_replace_all("\u008g","'") %>%
    str_replace_all("\u2019","'") %>%
    str_replace_all("\u0090","'") %>%
    str_replace_all("\U0090","'") %>%
    str_replace_all("\UFFFD","") %>%    #that is the weird character
    str_replace_all("\xc9v","")
}

strip_symbols <- function(vec) vec %>%
  str_remove_all("\\n|\\r") %>%
  str_replace_all("\\s+", " ") %>%
  str_replace_all("\\'", "")


#' Title
#'
#' @param graf
#'
#' @return
#' @export
#'
#' @examples
get_robustness <- function(graf){
  # browser()
  # attr(graf$links,"flow")$summary
  attr(graf,"info")$flow$summary
}




# deprecated --------------------------------------------------------------


#' Merge statements into links
#'
#' @inheritParams parse_commands
#'
#' @return A mapfile in which columns from the statements table are merged into the links table.
#' @export
#'
#' @examples
#'cashTransferMap %>% pipe_merge_statements() %>% pipe_find_links(field="text",value="women",operator="contains")
#'cashTransferMap %>% pipe_find_statements(field="text",value="women",operator="contains")
pipe_merge_statements <- function(graf){
  warning("deprecated!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  statements <- statements_table(graf) %>%
    left_join_safe(sources_table(graf)) %>% suppressMessages %>%
    left_join_safe(questions_table(graf)) %>% suppressMessages

  links <- links_table(graf) %>%
    left_join_safe(statements) %>% suppressMessages # ,by="statement_id") %>% otherwise when this is repeated, you get loads of cols

  # browser()
  pipe_update_mapfile(graf,links=links,statements=statements )

}


