notify <- function(text){
  # browser()
  safely(~showNotification(ui=text))()$result

}

# constants ---------------------------------------------------------

operator_list=c("=", "less", "greater", "notcontains", "notequals", "notequal", "equals", "equal", "contains", "starts", "ends", "start", "end")
buck <- "causalmap"
table_list <- c("factors","links","statements","sources","questions","settings") #has to be in Viewer as well

library(igraph)
library(configr)
library(DiagrammeR)
library(visNetwork)
library(tidyverse)
#library(tidygraph)
library(scales)
library(paws)
# library(DT)

s3 <- paws::s3()





# Loading maps --------------------------------------------------------------

s3file_exists <- function(object,buck){
  !is.null(safely(~s3$head_object(Key=object,Bucket=buck))()$result)
}

# note this is our own function , not from aws.s3 package
s3readRDS <- function(object,bucket,version=NULL,s3confun=s3){
  # s3confun$get_object(bucket,Key=object, VersionId = version)$Body %>% rawConnection() %>% gzcon %>% readRDS
  s3confun$get_object(bucket,Key=object, VersionId = version)$Body %>% rawConnection() %>%
    gzcon %>%
    (function(con) {on.exit(close(con)); readRDS(con)})
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
get_map_from_excel <- function(path){
  readxl::excel_sheets(path) %>%
    keep(. %in% table_list) %>%
    set_names %>%
    map(~readxl::read_excel(path,sheet = .)) %>%
    add_class
}
get_map_from_s3 <- function(path){
  # browser()
  notify("Trying cm2 file")
  if(!s3file_exists(object=basename(path),buck=dirname(path))) return()
  notify("Loaded cm2 file")
  s3readRDS(object=basename(path),bucket=dirname(path))
}

get_project_table <- function(tab="data",proj=sess$project,connection=conn){
  tbl(conn,local(tab)) %>%
    filter(project==local(proj)) %>%
    collect %>%
    mutate_all(to_logical)
}
get_whole_table <- function(tab,connection=conn){
  tbl(connection,local(tab)) %>% collect %>% mutate_all(to_logical)
}

get_map_tables_from_sql <- function(path,connection=conn){
  # browser()
  vsettings <- get_whole_table("settings",connection) %>% filter(project==path) # we need this anyway
  vdata <- get_project_table("data",path,connection)
  if(nrow(vdata)==0) return()
  vmeta <- get_project_table("meta",path,connection)
  vsentiment <- get_project_table("sentiment",path,connection)


  r <- vsettings$boxes %>%
    as.character()
  if(r=="") recodes <- NULL else recodes <- fromJSON(r)   #TODO actual recodes

  # browser()
  links <- req(vdata) %>%
    select(from,to,everything(),-project) %>%
    left_join_safe(vmeta %>% group_by(session_token) %>% summarise_all(last),by="session_token") %>%
    suppressMessages

  if(nrow(vsentiment)>0)
    links <- links %>%   left_join_safe(vsentiment %>% group_by(session_token) %>% select(-project) %>% summarise_all(last),by="session_token") %>%
    suppressMessages
  factors=tibble(label=c(links$from,links$to) %>% unique)


  graf <- tbl_graph(factors,links) %>%
    mutate(label=if_else(label=="zero",vsettings$base_q,label)) %>%
    filter(label!="")

  notify(paste0("Loading sql file: ",path))
  return(list(
    factors = graf %>% factors_table,
    links = graf %>% links_table,
    statements = NULL,               ########## STILL NEED TO GET SOURCES ETC
    sources = NULL,
    questions = NULL,
    settings = NULL
  ))

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
  notify("Trying cm1 file")
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
  # attr(graf,"statements") <- statements_with_meta
  notify("Loaded cm1 file")
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



# internal utilities-----------------------------------------------------------------------------

notify <- notify # alias
# return_notify <- function(tex){
#   notify(tex,3)
#   return()
# }

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

bind_rows_safe <- function(x,y,...){

  by=intersect(colnames(x),colnames(y))
  if(is.null(by))return()
  for(i in seq_along(by)){
    y[,by[i]] <- coerceValue(unlist(y[,by[i]]),unlist(x[,by[i]]))
  }
  bind_rows(x,y,...)

}


left_join_safe <- function(x,y,by=NULL,...){
  # browser()
  if(is.null(by))by=intersect(colnames(x),colnames(y))
  # x <- as_tibble(x)
  # y <- as_tibble(y)
  for(i in seq_along(by)){
    y[,by[i]] <- coerceValue(unlist(y[,by[i]]),unlist(x[,by[i]]))
  }
  left_join(x,y,by,...)
}
# left_join_safe <- left_join


replace_null <- function(x,replacement=0){
  if(is.null(x)) replacement else x
}
replace_infOLD <- function(x,replacement=0){
  if(is.infinite(x)) replacement else x
}
replace_Inf <- function(x,replacement=0){
  # browser()
  ifelse(is.infinite(x),replacement , x)
}
replace_inf <- replace_Inf #alias
replace_zero <- function(x,replacement=0){
  if(length(x)==0) replacement else x
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

escapeRegex <- function(string){ #from hmisc
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",
       string)
}

div_pal_n <- function(vec,lo,hi,mid){
  div_gradient_pal(low=lo,high=hi,mid=mid)(rescale(vec)) %>% alpha(.95)
}
viridis_pal_n <- function(vec){
  vec <- vec %>% as.factor %>% as.numeric
  viridis_pal()(length(unique(vec)))[vec] %>% alpha(.95)
}
brewer_pal_n <- function(vec){
  vec <- vec %>% as.factor %>% as.numeric
  scales::brewer_pal("qual")(length(unique(vec)))[vec] %>% alpha(.95)
}
create_colors <- function(vec,lo,hi,mid,type,field=""){
  # browser()
  if(class(vec)=="character") res <- brewer_pal_n(vec) else res <- div_pal_n(vec,lo=lo,hi=hi,mid=mid)
  attr(res,type) <-   list(table=tibble(vec,res) %>% unique,field=field)
  res
}

# tidymap major functions and pipes but not for use with parser -------------------------------------------------------------


#' Fix columns
#'
#' @inheritParams parse_commands
#'
#' @return A tidymap with a additional columns.
#' @export
#'
#'
#' @examples
pipe_fix_columns <- function(graf){
  if(!("color.background" %in% factor_colnames(graf))) graf <- graf %>% update_map(factors=graf$factors %>% mutate(color.background="#aaaaee77"))
  if(!("color.border" %in% factor_colnames(graf))) graf <- graf %>% update_map(factors=graf$factors %>% mutate(color.border="#222222"))
  if(!("frequency" %in% factor_colnames(graf))) graf <- graf %>% update_map(factors=graf$factors %>% mutate(frequency=1L))
  if(!("size" %in% factor_colnames(graf))) graf <- graf %>% update_map(factors=graf$factors %>% mutate(size=1L))
  if(!("found" %in% factor_colnames(graf))) graf <- graf %>% update_map(factors=graf$factors %>% mutate(found=1L))


  if(!("color" %in% link_colnames(graf))) graf <- graf %>% update_map(links=graf$links %>% mutate(color="#22446688"))
  if(!("frequency" %in% link_colnames(graf))) graf <- graf %>% update_map(links=graf$links %>% mutate(frequency=1L))
  if(!("capacity" %in% link_colnames(graf))) graf <- graf %>% update_map(links=graf$links %>% mutate(capacity=1L))
  if(!("label" %in% link_colnames(graf))) graf <- graf %>% update_map(links=graf$links %>% mutate(label=""))
  if(!("width" %in% link_colnames(graf))) graf <- graf %>% update_map(links=graf$links %>% mutate(width=.2))
  graf
}

#' Normalise factors and links
#'
#' @inheritParams parse_commands
#'
#' @return A tidymap with a additional columns.
#' @export
#'
#'
#' @examples
pipe_normalise_factors_links <- function(graf){
  tmp <- normalise_id(graf$factors,graf$links,"factor_id","from","to")
  factors=tmp$main;
  links=tmp$referring

  graf %>%
    update_map(factors=tmp$main,links=tmp$referring)
}

proportion_false <- function(lis) lis %>% map(~sum(unlist(.))/length(.)) %>% unlist


color_flipped_factors <- function(factors){
  factors %>%
    mutate(is_flipped=proportion_false(is_flipped)) %>%
    mutate(color.border= div_gradient_pal("#00ffaf","white","#ff8fb8")(is_flipped))

}
color_flipped_links <- function(links){
  links %>% mutate(
    from_color = case_when(
      from_flipped  ~  "#ff8fb8",
      T ~  "#00ffaf"
    )) %>%
      mutate(
        to_color = case_when(
          to_flipped  ~  "#ff8fb8",
          T ~  "#00ffaf"
        )) %>%
      mutate(
        color=paste0(from_color,";0.5:",to_color)
      )


}



update_join <- function(old,new){
  old %>%
    mutate(.rn=row_number()) %>%
    anti_join(new) %>%
    bind_rows_safe(new) %>%
    arrange(.rn) %>%
    select(-.rn)
}


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
update_map <- function(map,
                       factors=NULL,
                       links=NULL,
                       statements=NULL,
                       sources=NULL,
                       questions=NULL,
                       settings=NULL,
                       tables=list(
                         factors=NULL,
                         links=NULL,
                         statements=NULL,
                         sources=NULL,
                         questions=NULL,
                         settings=NULL
                       ),
                       clean=T,
                       all=T){
  # browser()
  if(!is.null(tables)){
    if(is.null(factors))       factors <- tables$factors
    if(is.null(links))       links <- tables$links
    if(is.null(statements))  statements <- tables$statements
    if(is.null(sources))     sources <- tables$sources
    if(is.null(questions))   questions <- tables$questions
    if(is.null(settings))    settings <- tables$settings
  }
  if(is.null(factors) & !is.null(map))factors <- factors_table(map)
  if(!all) {
    if(".rn" %notin% colnames(factors)) return(map)

    # factors <-
    #   factors_table(map) %>%
    #   update_join(factors)
  }
  if(is.null(factors))     factors <- map$factors
  if(is.null(links))       links <- map$links
  if(is.null(statements))  statements <- map$statements
  if(is.null(sources))     sources <- map$sources
  if(is.null(questions))   questions <- map$questions
  if(is.null(settings))    settings <- map$settings

  # browser()
  assemble_map(
    factors=factors,
    links=links,
    # links=links %>%  filter(from %in% (factors %>% row_index)) %>% filter(to %in% (factors %>% row_index)),
    statements=statements,
    sources=sources,
    questions=questions,
    settings=settings

  )

}


#' Merge map
#' @inheritParams parse_commands
#' @param graf
#' @param path
#' @description This also has a wrapper, pipe_merge_map
#' @return A tidy map. The column *_map_id is set to reflect the id of the map.
#' @export
#'
#' @examples
merge_map <- function(graf,graf2){
  # browser()
  map2 <- graf2 %>% pipe_clean_map() #  clean map will put the important vars to integer.
  graf <- graf %>% pipe_clean_map() #

  maxid <- max(as.numeric(graf$factors$factor_id))
  maxmapid <- max(as.numeric(graf$factors$factor_map_id))

  assemble_map(
    factors=graf$factors %>% bind_rows_safe(map2$factors %>% mutate(factor_map_id=maxmapid+1) %>% mutate(factor_id=factor_id+maxid)),
    links=graf$links  %>% bind_rows_safe(map2$links %>% mutate(link_map_id=maxmapid+1) %>% mutate(from=from+maxid,to=to+maxid)),
    statements=graf$statements  %>% bind_rows_safe(map2$statements %>% mutate(statement_map_id=maxmapid+1)),
    sources=graf$sources %>% bind_rows_safe(map2$sources %>% mutate(source_map_id=maxmapid+1)),
    questions=graf$questions %>% bind_rows_safe(map2$questions %>% mutate(question_map_id=maxmapid+1)),
    settings=graf$settings %>% bind_rows_safe(map2$settings %>% mutate(setting_map_id=maxmapid+1))
  ) %>%
    pipe_clean_map(tables=.)

}

#' Load map
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
load_map <- function(path=NULL,connection=conn){
  graf <- NULL
  factors <- NULL
  links <- NULL
  newtables <- NULL

  if(!is.null(path)){
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
      notify("Loaded standard file")

    } else if(type=="excel"){
      graf <- get_map_from_excel(path = path)
      if(is.null(graf)) return(NULL)
      notify("Loaded sql file")
    } else if(type=="sql"){
      newtables <- get_map_tables_from_sql(path,connection=connection)
      if(is.null(newtables)) return(NULL)
      notify("Loaded sql file")
    } else  if(type=="cm2"){
      graf <- get_map_from_s3(path %>% paste0("cm2data/",.)) %>% as.list  #as list because of tidygraph format
      if(is.null(graf)) return(NULL)
      if(is.null(graf$factors) & !is.null(graf$nodes)) graf$factors <- graf$nodes
      if(is.null(graf$links) & !is.null(graf$edges)) graf$links <- graf$edges
    } else if(type=="cm1"){
      graf <- get_map_tables_from_s3_pieces(path %>% paste0("causalmap/app-sync/",.))
      if(is.null(graf))return(NULL)
    } else if(type=="unknown"){
      notify("Trying to load file, guessing origin")
      # browser()

        graf <- get_map_from_s3(path %>% paste0("cm2data/",.))
      if(is.null(graf)) {
      graf <- get_map_tables_from_s3_pieces(path %>% paste0("causalmap/app-sync/",.))

      }

    }

  if(is.null(graf) & is.null(factors) & is.null(links)) {

    graf <- assemble_map()
      notify("creating blank map");

  }
  # browser()
  notify("Loading map")
  return(graf %>%
           add_original_ids %>%
           pipe_clean_map
         )





}

add_original_ids <- function(graf){

  # browser()
  graf %>%
    update_map(
      .,
      if(nrow(.$factors)>0)factors <- .$factors %>% mutate(factor_id0=row_number()),
      if(nrow(.$links)>0)links <- .$links %>% mutate(link_id0=row_number())
    )
}


#' Assemble map
#'
#' @param factors
#' @param links
#' @param statements
#' @param sources
#' @param questions
#' @param settings
#'
#' @return
#' @export
#'
#' @examples
assemble_map <- function(factors=NULL,links=NULL,statements=NULL,sources=NULL,questions=NULL,settings=NULL){

  list(factors %>% replace_null(standard_factors()),
       links %>% replace_null(standard_links()),
       statements %>% replace_null(standard_statements()),
       sources %>% replace_null(standard_sources()),
       questions %>% replace_null(standard_questions()),
       settings %>% replace_null(standard_settings())
       ) %>%
    set_names(xc("factors links statements sources questions settings")) %>%
    add_class
}

## this used to be assemble_map!!
#' Clean map
#'
#' @param factors
#' @param links
#' @param statements
#' @param sources
#' @param questions
#' @param settings
#'
#' @return A clean map in which all issues have been resolved.
#' @export
#'
#' @examples
pipe_clean_map <- function(tables=NULL){
  # browser()
  if(!is.null(tables)){
    factors <- tables$factors
    links <- tables$links
    statements <- tables$statements
    sources <- tables$sources
    questions <- tables$questions
    settings <- tables$settings
  }
flow <- attr(links,"flow")
  if(is.null(factors) & is.null(links)){
    factors=standard_factors()
    links=standard_links()
  }
  if(!is.null(links)){
    links <-  links %>%
      add_column(.name_repair="minimal",!!!standard_links())   %>%
      select(which(!duplicated(colnames(.)))) %>%
      # select(any_of(colnames(standard_links()))) %>%
      mutate(link_id=row_number())%>%
      filter(!is.na(from) & !is.na(to)) %>%
      mutate(strength=as.numeric(strength)) %>%  #TODO warning
      mutate(weight=as.numeric(weight)) %>%  #TODO warning
      mutate(certainty=as.numeric(certainty))  #TODO warning

  }
  if(!is.null(factors)){
    factors <-  factors %>%
      {if("factor_id" %notin% colnames(.)) mutate(.,factor_id=row_number()) else .} %>%
      add_column(.name_repair="minimal",!!!standard_factors())  %>%
      select(which(!duplicated(colnames(.)))) %>%
      # select(any_of(colnames(standard_factors()))) %>%
      mutate(label=as.character(label)) %>%
      filter(!is.na(factor_id)) #TODO warning


  }
  if(!is.null(factors) & is.null(links)){
    links <- standard_links()
  }


  missing_links <-
    c(links$from,links$to) %>%
    unique %>%
    keep(. %notin% factors$factor_id)

  if(length(missing_links)>0){
    notify("missing factor ids")
    warning("missing factor ids")
    # browser()
    links <- links %>%
      filter(from %notin% missing_links & to %notin% missing_links)
    # factors <- factors %>%
    #   bind_rows(tibble(factor_id=missing_links,label=as.character(missing_links)))
  }
  if(!identical(factors$factor_id,row_index(factors))){
    res <- normalise_id(factors,links,"factor_id","from","to")
    factors <- res$main
    links <- res$referring
    notify("Normalising factor ids")
  }

  factors <- factors %>% mutate(factor_id=as.integer(factor_id))
  links <- links %>% mutate(from=as.integer(from),to=as.integer(to))


  statements <- statements %>%
    replace_null(empty_tibble) %>%
    add_column(.name_repair="minimal",!!!standard_statements()) %>%
    select(which(!duplicated(colnames(.)))) # %>% select(any_of(colnames(standard_statements())))


  tmp <- compact_factors_links(factors,links)
  factors <- tmp$factors
  links <- tmp$links

  sources <- sources %>% replace_null(empty_tibble) %>% add_column(.name_repair="minimal",!!!standard_sources()) %>% select(which(!duplicated(colnames(.)))) #%>% select(any_of(colnames(standard_sources())))
  questions <- questions %>% replace_null(empty_tibble) %>% add_column(.name_repair="minimal",!!!standard_questions()) %>% select(which(!duplicated(colnames(.))))#%>% select(any_of(colnames(standard_questions())))
  settings <- settings %>% replace_null(empty_tibble) %>% add_column(.name_repair="minimal",!!!standard_settings()) %>% select(any_of(colnames(standard_settings())))
  settings <- settings %>% mutate_all(as.character)

  if(sources$source_id  %>% replace_zero(F) %>% duplicated() %>% any){
    warning("multiple IDs")
    notify("multiple IDs")
  }


  if(questions$question_id   %>% replace_zero(F) %>% duplicated() %>% any){

    warning("multiple IDs")
    notify("multiple IDs")
  }
  # browser()

  if(!is.null(statements)){if(!identical(statements$statement_id,row_index(statements))){
    statements <- filter(statements,!is.na(statement_id))
    res <- normalise_id(statements,links,"statement_id")
    statements <- res$main
    links <- res$referring
    notify("Normalising statement ids")
  }}

  links <- links %>% mutate(statement_id=as.integer(statement_id))
  statements <- statements %>% mutate(statement_id=as.integer(statement_id))

  sources$source_id <- coerceValue(sources$source_id,statements$source_id)
  questions$question_id <- coerceValue(questions$question_id,statements$question_id)
  links$statement_id <- coerceValue(links$statement_id,statements$statement_id)

  statements <- statements %>%
    left_join_safe(sources %>% rename_with(~paste0("r.",.),!matches("source_id"))) %>% suppressMessages %>%
    left_join_safe(questions %>% rename_with(~paste0("q.",.),!matches("question_id"))) %>% suppressMessages
  # statements <- statements %>%
  #     safely(~left_join_safe(sources %>% rename_with(~paste0("r.",.),!matches("source_id"))),otherwise=.)() %>% pluck("result") %>%
  #     safely(~left_join_safe(questions %>% rename_with(~paste0("q.",.),!matches("question_id"))),otherwise=.)() %>% pluck("result")

  # browser()
  links <- links %>%
    left_join_safe(statements %>% rename_with(~paste0("s.",.),!matches("statement_id"))) %>%
    suppressMessages
  # links <- links %>%
  #   safely(~left_join_safe(statements %>% rename_with(~paste0("s.",.),!matches("statement_id"))),otherwise=.)() %>% pluck("result")  # ,by="statement_id") %>% otherwise when this is repeated, you get loads of cols

  if(nrow(links)==0 | nrow(factors)==0){
    links <-
      links %>% mutate(from_label= "",to_label="",bundle="")

  } else
  links <-
    links %>% mutate(from_label= recode(from,!!!factors$label %>% set_names(factors$factor_id))) %>%
    mutate(to_label= recode(to,!!!factors$label %>% set_names(factors$factor_id))) %>%
    unite(bundle,from_label,to_label,remove = F,sep = " / ") %>%
    select(from_label,to_label,statement_id,quote,everything())

  ig <- make_igraph(factors,links)
  # ig <- make_igraph_from_links(links)

  factors$betweenness <- igraph::centr_betw(ig)$res %>% round(2)
  factors$betweenness_rank <- factors$betweenness %>% rank
  factors$in_degree=ig %>% igraph::degree(mode = "in")
  factors$out_degree=ig %>% igraph::degree(mode = "out")
  factors$frequency <- factors$in_degree+factors$out_degree

  attr(links,"flow") <- flow


  assemble_map(factors,links,statements,sources,questions,settings) %>%
    pipe_fix_columns()

}

make_igraph_from_links <- function(links){
  links %>% select(from,to) %>% as.matrix()%>% graph_from_edgelist()
}
make_igraph_from_map <- function(graf){
  make_igraph(graf$factors,graf$links)

}
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

normalise_id <- function(main,referring,keyname,referring_keyname1=keyname,referring_keyname2=NULL){
  if(nrow(main)==0)return(list(main=main,referring=referring))
  if(is.null(main[,keyname])){notify("keyname not in main table")}
  if(is.null(referring[,referring_keyname1])){notify("keyname not in referring table")}
  # browser()
  if(is.null(main$label))main$label <- main$factor_id
  # if(length(unique(main[,keyname]))!=nrow(main))
  main$.old_key <- main[,keyname] %>% unlist
  main[,keyname] <- 1:nrow(main)
  recodes=main[,keyname] %>% unlist %>% as.list
  names(recodes)=main$.old_key %>% unlist

  referring[,keyname] <- referring[,referring_keyname1]
  referring[,keyname] <- dplyr::recode(referring[,keyname] %>% unlist,!!!recodes)
  referring[,referring_keyname1] <- referring[,keyname]
  if(referring_keyname1!=keyname) referring[,keyname] <- NULL

  if(!is.null(referring_keyname2)){
    referring[,keyname] <- referring[,referring_keyname2]
    referring[,keyname] <- dplyr::recode(referring[,keyname] %>% unlist,!!!recodes)
    referring[,referring_keyname2] <- referring[,keyname]
    if(referring_keyname2!=keyname) referring[,keyname] <- NULL
  }


  return(list(main=main %>% select(-.old_key) %>% mutate(factor_id=row_number()),referring=referring))
}


compact_map <- function(graf){
  factors <- graf$factors
  links <- graf$links
  tmp <- compact_factors_links(factors,links)
  update_map(graf,factors=tmp$factors,links=tmp$links)

}
collapse_unique <- function(vec){
  paste0(unique(vec),collapse="; ")
}
compact_factors_links <- function(factors,links){
  if(factors$label %>% table %>% max %>% `>`(1)){
    notify("Some factor labels are duplicates; compacting")
    # browser()
    factors <-
      factors %>%
      group_by(label) %>%
      mutate(new_id=cur_group_id(),
             frequency=sum(frequency),
             in_degree=sum(in_degree),
             out_degree=sum(out_degree)
             )


    # browser()
    new_id <- factors$new_id

    links$from <-
      links$from %>% recode(!!!new_id %>% set_names(factors$factor_id))
    links$to <-
      links$to %>% recode(!!!new_id %>% set_names(factors$factor_id))

    factors <-
      factors %>%
      summarise_all(first) %>%  # should it be collapse_unique???
      mutate(factor_id=new_id) %>%
      select(-new_id)

  }
  return(list(factors=factors,links=links))
}


# tidymap minor functions ----------------------------------------------------

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

#' Add attribute
#'
#' @param graf
#' @param value
#' @param attr
#'
#' @return
#' @export
#'
#' @examples
add_attribute <- function(graf,value,attr="flow"){
  attr(graf,attr) <- value
  graf
}



standard_factors <- function(links=standard_links()){if(is.null(links$from) | is.null(links$to))stop("Wrong links")
  tibble(label=c(links$from,links$to) %>% unique %>% as.character,factor_memo="",factor_map_id=1,factor_id=as.numeric(label))
  }
standard_links <- function(){tibble(
  link_id=1,
  statement_id=1,
  from=1,
  to=1,
  quote="",
  weight=1,
  strength=1,
  certainty=1,
  from_flipped=F,
  to_flipped=F,
  link_label="",
  hashtag="",
  link_memo="",
  link_map_id=1
  )}
standard_statements <- function()tibble(statement_id=1,text="blank statement",statement_memo="",source_id="1",question_id="1",statement_map_id=1)
standard_sources <- function()tibble(source_id="1",source_memo="global source",source_map_id=1)
standard_questions <- function()tibble(question_id="1",question_text="global question",question_memo="",question_map_id=1)
standard_settings <- function()tibble(setting_id="background_colour",value="",setting_map_id=1)

#' Add class
#'
#' @export
#'
#' @examples
add_class <- function(x,cls="tidymap"){
  class(x) <- c(cls,class(x)) %>% unique
  x
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



print.tidymap <- function (graf, n=2,...)
{
  # browser()
  cat("Factors: ");graf$factors %>% as_tibble  %>% print(n=n)
  cat("Links: ");graf$links %>% as_tibble  %>% print(n=n)
  cat("Statements: ");graf$statements %>% as_tibble  %>% print(n=n)
  cat("Sources: ");graf$sources %>% as_tibble  %>% print(n=n)
  cat("Questions: ");graf$questions %>% as_tibble  %>% print(n=n)
  cat("Settings: ");graf$settings %>% as_tibble  %>% print(n=n)

}


factor_colnames <- function(graf)graf %>% factors_table %>% colnames
link_colnames <- function(graf)graf %>% links_table %>%  colnames

print_more <- function(graf,n=99){
  graf$factors %>% as_tibble %>% print(n=n)
  graf$links %>% as_tibble %>% print(n=n)
}




zoom_inner <- function(string,n,char){
  string %>% map(~str_split(.,char) %>% `[[`(1) %>% `[`(1:n) %>% keep(!is.na(.)) %>% paste0(collapse=char)) %>% unlist
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
    flip_inner_component(flipchar=flipchar) %>%
    paste0(collapse=sepchar)
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
  graf <- graf %>% pipe_bundle_links() %>% pipe_clean_map()


  links <- graf$links %>%
    mutate(frequency=if_else(is.na(frequency),1L,as.integer(frequency)))


  from_vec <- factors_table(graf) %>% filter(found_from) %>% pull(label)
  to_vec <- factors_table(graf) %>% filter(found_to) %>% pull(label)
  newnodes <- tibble(
    factor_id=c(from_vec,"_super_source_"))

  newedges <- tibble(
    from="_super_source_",
    to=from_vec,
    capacity=Inf
  )

  newgraf <- assemble_map(newnodes,newedges) %>%
    pipe_normalise_factors_links()


  graf <- graf %>% merge_map(newgraf) %>% pipe_clean_map

  newnodes <- tibble(
    factor_id=c(to_vec,"_super_sink_"))

  newedges <- tibble(
    to="_super_sink_",
    from=to_vec,
    capacity=Inf
  )
  newgraf <- assemble_map(newnodes,newedges) %>%
    pipe_normalise_factors_links() %>%
    pipe_clean_map

  graf <-
    graf %>% merge_map(newgraf)

  graf$factors$capacity  <- graf$factors$capacity %>% replace_null(1)
  graf$factors  <- graf$factors %>%
    mutate(capacity=if_else(is.na(capacity),1,capacity)) %>%
    mutate(capacity=pmax(frequency,capacity,na.rm=T))

  graf$links  <- graf$links %>%
    filter(from!=to)

  ig <- make_igraph(graf$factors,graf$links,use_labels = T)

  source <- V(ig)[graf$factors$label=="_super_source_"]
  sink <- V(ig)[graf$factors$label=="_super_sink_"]
  res <- ig %>%
    max_flow(source=source, target=sink)
  sources <- V(ig)[graf$factors$found_from %>% replace_na(F)]
  sinks <- V(ig)[graf$factors$found_to %>% replace_na(F)]

  if(length(sinks)>1){
    sinkvec <- c(sink,sinks)

    rn <- (graf %>% factors_table %>% filter(found_to) %>% pull(label)) %>% c("All targets",.)
  }else {
    sinkvec <- sinks
    rn <- graf %>% factors_table %>% filter(found_to) %>% pull(label)
  }
  if(length(sources)>1){
    sourcevec <- c(source,sources)
    cn <- (graf %>% factors_table %>% filter(found_from) %>% pull(label)) %>% c("All sources",.)

  }
  else {
    cn <- (graf %>% factors_table %>% filter(found_from) %>% pull(label))
    sourcevec <- sources
  }
# graf %>% distance_table()
## actually this is stupid because you don't need to calculate flow, you can just
## look at the distances. However flow is hardly any slower, so why not.
## Here is the same thing with distances - it isn't any faster.
#   if(quick){
# # browser()
#   all_flows <-
#     sinkvec %>% map(function(y)(sourcevec %>% map(function(x) if(x %in% sinks) Inf else shortest_paths(graf,x,y,mode="out")$vpath %>% pluck(1) %>% length %>% `>`(1))) %>% unlist) %>%
#     do.call("rbind",.) %>%
#     as_tibble %>%
#     mutate_all(as.numeric)
# }
#     else
  all_flows <-

    sinkvec %>% map(function(y)(
      sourcevec %>% map(function(x) if(x %in% sinks) Inf else max_flow(ig,x,y)$value)) %>% unlist) %>%
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
find_fun <- function(df,field=NULL,value,operator=NULL,what,pager=F){
  # browser()
  if(is.null(field) & is.null(operator)){
    field="label"
    operator="contains"
  }
    value_original <- value

  if(is.character(df[[field]])){
  # if(field %in% xc("label text from_label to_label")){

    value <- value %>% make_search %>% tolower()
  }

  if(field %notin% colnames(df)) {notify("No such field");return(df)}


  if(operator=="contains"){df <- df %>%  mutate(found=str_detect(tolower(unwrap(UQ(sym(field)))),value %>% paste0(collapse="|")))} else
    if(operator=="notcontains"){df <- df %>%  mutate(found=!str_detect(tolower(unwrap(UQ(sym(field)))),value %>% paste0(collapse="|")))} else
      if(operator %in% xc("= equals equal")){df <- df %>%  mutate(found=(make_search(tolower(unwrap(UQ(sym(field))))) %in% value))} else
        if(operator %in% xc("notequals notequal")){df <- df %>%  mutate(found=(tolower(unwrap(UQ(sym(field)))) %notin% value))} else
          if(operator %in% xc("greater")){df <- df %>%  mutate(found=(as.numeric(UQ(sym(field)))>max(as.numeric(value),na.rm=T)))} else
            if(operator %in% xc("less")){df <- df %>%  mutate(found=(as.numeric(UQ(sym(field)))<min(as.numeric(value),na.rm=T)))} else
              if(operator %in% xc("starts start")){df <- df %>%  mutate(found=str_detect(tolower(unwrap(UQ(sym(field)))),paste0("^",value %>% paste0(collapse="|"))))} else
                if(operator %in% xc("ends end")){df <- df %>%  mutate(found=str_detect(tolower(unwrap(UQ(sym(field)))),paste0(value %>% paste0(collapse="|"),"$")))}


  if(pager & operator %in% xc("= equals equal")){
    vec <- df[,field] %>% unique
    pager_current <- which(value_original==vec) %>% min
    attr(df,"pager") <- list(pager=vec,pager_current=pager_current)
  }
  df

}

# exported tidymap utilities ---------------------------------------------------------



#' Extracting tibbles from A tidymap
#'
#' @inheritParams parse_commands
#' @description These three functions extract tables of factors, links or statements from a tidymap.
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

#' Parse line
#'
#' The engine for parse_commands
#' @param graf A tidymap representing a causal map.
#' @param line A line of text to be parsed
#' @return A list containing the function name and a list of parameters.
#' @export
parse_line <- function(line,graf){
  # browser()
  notify(line)
  if(str_trim(line)=="")return()
  fun <- word(line, 1,2, sep=" ")
  if(is.na(fun)){notify("No such function");return()}
  if(!exists(str_replace(fun," ","_") %>% paste0("pipe_",.))){notify("No such function");return(NULL)}

  body <-
    str_remove(line,fun) %>%
    str_trim


  # case: just text nothing else
  if(fun %in% c("find factors") & !str_detect(body,operator_list %>% keep(.!="=") %>% paste0(collapse="|"))){

    # browser()
    updown <- body %>% str_match("(up *([0-9]+) *)*( down *([0-9]+))* *$")
    up <- updown[,3] %>% replace_na(0)
    down <- updown[,5] %>% replace_na(0)
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
        up <- updown[,3] %>% replace_na(0)
        down <- updown[,5] %>% replace_na(0)
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

    if(length(fields)!=length(vals)){notify("Wrong number of values");return(graf %>% filter(F))}

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
#' @param graf A tidymap representing a causal map.
#' A tidymap is a tidygraph, which consists of a table of edges linked to a table of nodes,
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
#' @return A tidymap, the result of successively applying the commands to the input graph.
#' @export
#' @examples
#'cashTransferMap %>% parse_commands("select factors top=10 \n color factors field=n") %>% make_vn()
parse_commands <- function(graf=NULL,tex){
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

# browser()
      graf <- possibly(~do.call(tmp$fun,tmp$vals),otherwise=graf)()
}
    }
  }
  graf
}


# main graph functions ----------------------------------------------------

pipe_page_factors <- function(...)pipe_find_factors(pager=T,...)
pipe_page_statements <- function(...)pipe_find_statements(pager=T,...)
pipe_page_links <- function(...)pipe_find_links(pager=T,...)


#' Find factors
#'
#' @inheritParams pipe_find_factors
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
#' A tidymap containing only matching factors; if `up`!=0 then also factors this number of steps
#' upstream of the matching factors are also included and likewise for `down`!=0.
#' The links are filtered correspondingly to return only the "ego network" i.e. links between the returned factors.

#'
#' If operator and field are both NULL, the value is treated as a simple search string for the field `label`.
#' @export
#'
#' @examples
#' pipe_find_factors(cashTransferMap,NULL,"Cash")
#' pipe_find_factors(cashTransferMap,field="label",value="Cash",operator="contains")
#' pipe_find_factors(cashTransferMap,field="id",value=10,operator="greater")
#' pipe_find_factors(cashTransferMap,NULL,"purchase OR buy")
pipe_find_factors <- function(graf,field="label",value,operator="contains",up=1,down=1,pager=F){
  st <- attr(graf,"statements")
  df <- graf %>% factors_table %>% find_fun(field,value,operator,pager=pager)
  pager <- df %>% attr("pager")
  graf <- update_map(graf,factors=df) %>% add_attribute(pager,"pager")

  ig <- make_igraph(graf$factors,graf$links)
  downvec <- ig %>% igraph::distances(to=graf %>% factors_table %>% pull(found),mode="in") %>% apply(1,min) %>% `<=`(down)
  upvec <- ig %>% igraph::distances(to=graf %>% factors_table %>% pull(found),mode="out") %>% apply(1,min) %>% `<=`(up)

  if(any(upvec)|any(downvec))
    graf %>% update_map(factors=factors_table(graf) %>% filter(found|upvec|downvec)) %>%
    pipe_normalise_factors_links %>%
    pipe_remove_isolated_links() else
      graf %>% filter(F)
  # if we don't clean the map here, the factor and link ids get out of sync

}

#' Find links
#'
#' @inheritParams pipe_find_factors
#' A tidymap containing only matching links. Factors are not removed, so this function may return maps with isolated factors,
#' i.e. factors with no links.
#' @export
#'
#' @examples
#' pipe_find_links(cashTransferMap,value="Cash")
#' pipe_find_links(cashTransferMap,field="label",value="Cash",operator="contains")
#' pipe_find_links(cashTransferMap,field="from",value="12",operator="greater")
pipe_find_links <- function(graf,field=NULL,value,operator=NULL,pager=F){
# browser()
  graf$links <- graf$links %>% find_fun(field,value,operator,pager=pager) %>% filter(found)

  graf

}

#' Find statements
#'
#' @inheritParams pipe_find_factors
#' @return A tidymap filtered by statements.
#' @export
#'
#' @examples
pipe_find_statements <- function(graf,field,value,operator="=",pager=F){
# browser()
  statements <- graf$statements %>% find_fun(field,value,operator,pager=pager)  %>%
    filter(found)

  links <- graf$links %>%  filter(statement_id %in% statements$statement_id)

  return(graf %>%
           update_map(
             links=links,
             statements=statements
             )
         )


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
pipe_select_links <- function(graf,top=NULL,bottom=NULL){
  graf <- graf %>%
    pipe_bundle_links()

  links <- graf$links %>%
    arrange(desc(frequency)) %>%
    {if(!is.null(top))slice(.,1:top) else slice(.,(nrow_links_table(graf)+1-bottom):nrow_links_table(graf))} %>%
    select(from,to,frequency,everything())

  update_map(graf,links=links) %>%
    pipe_remove_isolated
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
pipe_select_factors <- function(graf,top=NULL,bottom=NULL,all=F){

  graf$factors <- factors_table(graf) %>%
    arrange(desc(frequency)) %>%
    {if(!is.null(top))slice(.,1:top) else slice(.,(nrow_factors_table(graf)+1-bottom):nrow_factors_table(graf))} %>%
    arrange(factor_id)

  graf %>% pipe_remove_isolated_links() %>% pipe_normalise_factors_links()




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
  # browser()
  factors <- graf$factors %>%
    filter(factor_id %in% get_all_link_ids(graf$links))

  tmp <- normalise_id(factors,graf$links,"factor_id","from","to")

  graf %>%
    update_map(factors=tmp$main,links=tmp$referring) %>%
    pipe_remove_isolated_links()

}

pipe_remove_isolated_links <- function(graf,labels=F){
  # browser()

  if(labels)graf %>% update_map(factors=graf$factors,
                      links=graf$links %>% filter(from_label %in% graf$factors$label & to_label %in% graf$factors$label)) else
  graf %>% update_map(factors=graf$factors,
                      links=graf$links %>% filter(from %in% graf$factors$factor_id & to %in% graf$factors$factor_id))

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
pipe_zoom_factors <- function(graf,level=1,separator=";",hide=T){
  level=as.numeric(level)
  hide=as.logical(hide)
  # flow=attr(graf,"flow")
  # statements <- graf %>% statements_table()
  if(level<1) return(graf)

# browser()
  graf %>%
    update_map(
      factors <- graf$factors %>%
    mutate(old_label=label,label=if_else(str_detect(old_label,separator),
                                         zoom_inner(old_label,level,separator),old_label),
           frequency=sum(frequency)) %>%
    select(-old_label)) %>%
    compact_map

  # %>%
  #   pipe_clean_map



}

#' Bundle factors
#'
#' @inheritParams parse_commands
#' @param value A search string.
#'
#' @return A tidymap in which factors matching the search string are merged into one, with rerouted links.
#' If the search string is empty, factors with the same first word are grouped.
#' @export
#'
#' @examples
pipe_bundle_factors <- function(graf,value=""){
  value <- value %>% make_search %>% paste0(collapse="|")
  update_map(graf,factors <- graf$factors %>%
    mutate(
      label=if(value[1]=="")
        str_match(label,"^[^ ]*") %>% `[`(,1)
      else if_else(str_detect(label,value),str_match(label,paste0(value)),label)
    )
    ) %>%
    compact_map()
  }



#' Bundle links
#'
#' @inheritParams parse_commands
#' @param field
#'
#' @return A tidymap in which sets of coterminal, same-direction links are replaced with
#' one link (when `field` = 'n') or more than one link for each of the values of `field`
#' present in the data. In each case, each new link has a field n representing the number
#' of links it is replacing, unless the links it is replacing already had values n in which
#' case the new value of `n` is the sum of the `n` values of the constituent links.
#' @export
#'
#' @examples
#' # Showing separate (bundled) links for women and men:
#' if(F)cashTransferMap %>% pipe_merge_statements() %>%  pipe_select_factors(10) %>% pipe_bundle_links(counter="frequency",group="1. Sex")%>% pipe_label_links(field = "frequency") %>% pipe_color_links(field="1. Sex") %>% pipe_scale_links() %>%  make_grviz()
#' # or, counting sources rather than statements:
#' if(F)cashTransferMap %>% pipe_merge_statements() %>%  pipe_select_factors(10) %>% pipe_bundle_links(group="1. Sex",counter="#SourceID")%>% pipe_label_links(field = "frequency") %>% pipe_color_links(field="1. Sex") %>% pipe_scale_links() %>%  make_grviz()
pipe_bundle_links <- function(graf,counter="frequency",group=NULL){
  # statements <- graf %>% statements_table()
  # flow <- graf %>% attr("flow")
  # factors <- factors_table(graf)
  links <- graf$links
  # if(nrow(factors)==0) return(NULL)
  coln <- colnames(links)


  if(counter %notin% coln & counter!="frequency" ) {
    notify("counter not found, trying with s.") #legacy
    if(paste0("s.",counter) %in% coln  & counter!="frequency") counter <-  paste0("s.",counter) else
    {
      notify("counter not found")
      return(graf)

    }
  }
  if(!is.null(group)){if(group %notin% coln) {notify("no such group");return(graf)}}

  if(is.null(group)) links <- links %>% group_by(from,to) else
    links <- links %>% group_by(from,to,UQ(sym(group)))

  if (counter == "frequency") {
    if ("frequency" %in% coln){

      # browser()
      links <- links %>%
        mutate(rn_ = row_number()) %>%
        mutate(frequency = sum(frequency))
    }else
      links <- links %>%
        mutate(rn_ = row_number()) %>%
        mutate(frequency = n())
  } else
    links <- links %>%
    mutate(rn_ = row_number()) %>%
    mutate(frequency = length(unique(UQ(sym(
      counter
    )))))

  links <-
    links %>%
    filter(rn_ == 1) %>% ungroup %>% select(-rn_)

  update_map(graf,links=links)

}


#' Cluster factors
#'
#' @inheritParams parse_commands


#' @return
#' @export
#'
#' @examples
pipe_cluster_factors <- function(graf,clusters=NULL){


  if(!is.null(clusters)) {
# browser()
    choices <- clusters %>% escapeRegex %>% str_split(" OR ") %>% `[[`(1) %>% str_trim

    nodes <- factors_table(graf)
    nodes$cluster <- choices %>%
      map(~cluster_fun(nodes$label,.)) %>%
      as.data.frame() %>%
      apply(1,function(x)paste0(x,collapse=""))
    nodes <- nodes %>%
      mutate(cluster=ifelse(cluster=="",NA,cluster))    %>%
      mutate(cluster=str_remove_all(cluster,"\\\\"))
    graf %>% update_map(factors=graf$factors %>% mutate(cluster = nodes$cluster))
  }
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
  graf %>%
    pipe_trace_paths(from=from,to=to,length=length) %>%
    pipe_calculate_robustness(field=field)

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
pipe_trace_paths <- function(graf,from,to,length=4){
  if(is.na(length)) {notify("You have to specify length");return(graf)}
  if(from=="") {notify("You have to specify source factors");return(graf)}
  if(to=="") {notify("You have to specify target factors");return(graf)}
  # browser()
  from <- from %>% make_search
  to <- to %>% make_search
  if(from=="" & to =="") return(graf)
  links <- graf$links %>%
    select(from,to,everything())

  factors <- graf$factors %>%
    mutate(found_from=str_detect(tolower(label),tolower(from))) %>%
    mutate(found_to=str_detect(tolower(label),tolower(to))) %>%
    mutate(found_type=paste0(if_else(found_from,"source","-"),if_else(found_to,"target","-"))) %>%
    mutate(found_any=found_from|found_to)

  # browser()

  if(!any(factors %>% pull(found_any))) return(graf %>% filter(F))



  tracedownvec <- make_igraph_from_links(links) %>% distances(to=factors %>% pull(found_from),mode="in") %>% apply(1,min,na.rm=T)
  traceupvec <- make_igraph_from_links(links) %>% distances(to=factors %>% pull(found_to),mode="out") %>% apply(1,min,na.rm=T)

  # here we need to intervene to make sure that influences don't move closer to the source, as this is a kind of loop

  bothvecsum <- `+`(tracedownvec,traceupvec)
  bothvec <- bothvecsum<=length
  if(min(bothvecsum)<Inf) factors <- factors %>% mutate(traceupvec=traceupvec,
                                                  tracedownvec=tracedownvec,
                                                  bothvec,
                                                  found=found_from|found_to
  ) %>% filter(bothvec) else factors %>% filter(F)


  sums <- factors %>% select(found_from,found_to) %>% colSums(na.rm=T)
  if((sums[1]*sums[2])>10000){
    # if(sum(found_from,na.rm=T)*sum(found_to,na.rm=T)>10){
    notify("too much to trace")
    return(graf)
  }


# all_flows <- calculate_robustness(graf)

# notify(glue("Number of cuts is {res$cut %>% length}"))
  factors <- factors %>%
    filter(label!="_super_sink_" & label!="_super_source_")


  update_map(graf,factors,links) %>%
    pipe_normalise_factors_links %>%
    pipe_remove_isolated_links()
  # browser()
  # update_map(graf,factors=factors_table(graf %>% filter(label!="_super_sink_" & label!="_super_source_")))  %>%
  #   pipe_clean_map



}



#' Calculate robustness
#'
#' @param graf A tidymap. To use this function, the factors table of this map must include
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
#' @return A tidymap with an additional attribute `flow`, a tibble (dataframe) in which
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
pipe_calculate_robustness <- function(graf,field=NULL){
  res <- list()
  # browser()
  if("found_from" %notin% factor_colnames(graf)) {warning("No found_from column");return(graf)}  # if("found_from" %notin% factor_colnames(graf)) {warning("No found_from column");return(graf)}
  if("found_to" %notin% factor_colnames(graf)) {warning("No found_to column");return(graf)}
  if(field %>% replace_null("")=="")field <- NULL
# browser()

  if(is.null(field)) res$summary <- (calculate_robustness_inner(graf)) else

  if(field %notin% link_colnames(graf)) {warning("Field not found");res$summary <- graf}
   else {



  vec <- graf %>%
    links_table() %>%
    pull(UQ(sym(field))) %>%
    unique

  if("" %in% vec){warning("Vector contains an empty string");vec <- vec %>% keep(.!="")}

  res <- vec %>% set_names(vec) %>% map(
    function(x) graf %>%
      pipe_find_links(field=field,value=x,operator="=") %>%
      calculate_robustness_inner()
  )

  summary <- res %>% map(~(select(.,-1))%>% mutate_all(~if_else(.>0 ,1,0))) %>%
    Reduce(`+`,.)
  res$summary <- cbind(res[[1]][,1],summary) %>% as_tibble





   }
  # browser()
  graf %>% update_map(.,links=add_attribute(graf$links,res,"flow"))

}




pipe_flip_opposites <- function(graf,flipchar="~",add_colors=T){
  if(add_colors)notify("Also adding colours; you can turn this off with 'flip opposites add_colors=FALSE'")
  factors <-
    graf$factors %>%
    mutate(
      is_flipped=str_detect(label,paste0("^ *",flipchar)),
      label=if_else(is_flipped,flip_vector(label,flipchar = flipchar),label),
      label=flip_fix_vector(label)
    ) %>%
    {if(add_colors)color_flipped_factors(.) else .}

# browser()
  links <-
    graf$links %>%
    mutate(from_flipped=factors$is_flipped[from] %>% as.logical) %>%
    mutate(to_flipped=factors$is_flipped[to] %>% as.logical) %>%
    {if(add_colors)color_flipped_links(.) else .}

  graf %>%
    update_map(factors,links) %>%
    compact_map()


}



#' Merge statements into links
#'
#' @inheritParams parse_commands
#'
#' @return A tidymap in which columns from the statements table are merged into the links table.
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
  update_map(graf,links=links,statements=statements )

}

#' Pipe merge map
#' @inheritParams parse_commands
#' @param graf
#' @param path
#' @description A wrapper around merge_map to make it work in the app.
#' @return A tidy map. The column *_map_id is set to reflect the id of the map.
#' @export
#'
#' @examples
pipe_merge_map <- function(graf,path){
  # browser()
  map2 <- load_map(path=path)
  graf <- graf
  merge_map(graf,map2)


}

# zero_to_one <- function(vec)(vec-min(vec,na.rm=T))/(max(vec,na.rm=T)-min(vec,na.rm=T))



## add formats -------------------------------------------------------------


#' Scale factors
#'
#' @inheritParams parse_commands
#' @param field A numerical field in the factor table which will control the scale.
#'
#' @return A tidymap with a new or overwritten column `size`in the factor table varying between .2 and 1.
#' @export
#'
#'
#' @examples
pipe_scale_factors <- function(graf,field="frequency"){
  # graf <- pipe_metrics(graf)
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  class <- graf %>% factors_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  # browser()
  graf %>%
    update_map(factors=graf$factors %>% mutate(size=scales::rescale(UQ(sym(field)),to=c(0.2,1))))

}
#' Scale factors
#'
#' @inheritParams parse_commands
#' @param field A numerical field in the link table which will control the scale (the width of the links).
#'
#' @return A tidymap with a new or overwritten column `width`in the link table varying between .2 and 1.
#' @export
#'
#' @examples
pipe_scale_links <- function(graf,field="frequency",fixed=NULL){
  if(!is.null(fixed))return(graf  %>% update_map(links=graf$links %>%  mutate(width=fixed)))
  # browser()
  if(field %notin% colnames(links_table(graf))){warning("No such column");return(graf)}

  class <- graf$links %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  graf  %>% update_map(links=graf$links %>% mutate(width=scales::rescale(UQ(sym(field)),to=c(0.1,1))))
}

#' Label factors
#'
#' @inheritParams parse_commands
#' @param field A numerical or character field in the factor table.
#' @param clear Logical. Whether to clear any existing labels or to concatenate the new result after
#' any existing labels. Default is `FALSE`.
#'
#' @return A tidymap with a column `label`. If `clear` is FALSE, the new label is concatenated
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
pipe_label_factors <- function(graf,field="frequency",clear=F){
  # browser()
  clear=as.logical(clear)
  # graf <- pipe_metrics(graf)
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}

  graf %>%
    update_map(factors=graf$factors %>%
    mutate(label=paste0((if(clear)NULL else paste0(label,". ")) %>% keep(.!=""),field,": ",UQ(sym(field)),". ")))
}



#' Label links
#'
#' @inheritParams parse_commands
#' @param field A numerical or character field in the link table.
#' @param clear Logical. Whether to clear any existing labels or to concatenate the new result after
#' any existing labels.
#'
#' @return A tidymap with a column `label`. If `clear` is FALSE (the default), the new label is concatenated
#' after any existing label. The new label is of the form `field: value`.
#' @export
#'
#'
#' @examples
pipe_label_links <- function(graf,field="frequency",clear=T,field_label=F){
  clear=as.logical(clear)
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
  # browser()
  graf %>% update_map(links=graf$links %>%
                        mutate(link_label=paste0(
                          (if(clear)NULL else paste0(link_label,". ")) %>%
                            keep(.!="")
                          ,
                          if(field_label)paste0(field,": ")
                          ,
                          UQ(sym(field))
                          ,
                          if(field_label)". "
                        )
                        )
                      )
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
#' @return A tidymap with a new or overwritten column `color.background`in the factor table.
#' @export
#'
#'
#' @examples
pipe_color_factors <- function(graf,field="frequency",lo="green",hi="blue",mid="gray",fixed=NULL){
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  if(!is.null(fixed)) factors <- graf$factors %>%
      mutate(color.background=fixed) else  factors <- graf$factors %>%
    mutate(color.background=create_colors(UQ(sym(field)),lo=lo,hi=hi,mid=mid,type="color_factors",field=field))
  graf %>% update_map(factors=factors)
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
#' @return A tidymap with a new or overwritten column `color.border`in the factor table.
#' @export
#'
#'
#' @examples
pipe_color_borders <- function(graf,field="frequency",lo="green",hi="blue",mid="gray",fixed=NULL){
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}

# browser()
  if(!is.null(fixed)) factors <- graf$factors %>% mutate(color.border=fixed) else
    factors <- graf$factors %>%
      mutate(color.border=create_colors(UQ(sym(field)),lo=lo,hi=hi,mid=mid,type="color_borders",field=field))
  graf %>% update_map(factors=factors)
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
#' @return A tidymap with a new or overwritten column `color`in the link table.
#' @export
#'
#'
#' @examples
pipe_color_links <- function(graf,field="frequency",lo="green",hi="blue",mid="gray",fixed=NULL){
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
# browser()
  if(!is.null(fixed)) links <- graf$links %>% mutate(color=fixed) else
    links <- graf$links %>%
      mutate(color=create_colors(UQ(sym(field)),lo=lo,hi=hi,mid=mid,type="color_links",field=field))
  graf %>% update_map(links=links)

}
#' Fade factors
#'
#' @inheritParams parse_commands
#' @param field A numerical field in the link table which will control the amount of fading
#' (the alpha value of the factors).
#' @return A tidymap in which the column `color.background`in the factor table has alpha proportionate to the values in `field`.
#' @export
#'
#'
#' @examples
pipe_fade_factors <- function(graf,field="frequency"){
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  if("color.background" %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  class <- graf %>% factors_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  # browser()
  graf %>% update_map(factors=graf$factors %>%
                        mutate(color.background=alpha(color.background,scales::rescale(UQ(sym(field)),to=c(0.2,1)))))
}

#' Fade links
#'
#' @inheritParams parse_commands
#' @param field A numerical field in the link table which will control the amount of fading
#' (the alpha value of the links).
#' @return A tidymap in which the column `color`in the link table has alpha proportionate to the values in `field`.
#' @export
#'
#' @examples
pipe_fade_links <- function(graf,field="frequency"){
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
  if("color" %notin% link_colnames(graf)){warning("No such column");return(graf)}
  class <- graf %>% links_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  # browser()
  graf %>% update_map(links=graf$links %>%  mutate(color=alpha(color,scales::rescale(UQ(sym(field)),to=c(0.2,1)))))
}

#' Remove bracketed expressions
#'
#' @inheritParams parse_commands
#' @param value c("[","(")
#'
#' @return A tidymap in which the factor labels have had any text enclosed with square brackets or round brackets removed, along with the brackets.
#'
#' @export
#'
#'
#' @examples
pipe_remove_brackets <- function(graf,value="["){
  if(value=="[")graf %>% update_map(factors=graf$factors %>%
    mutate(label=str_remove_all(label,"\\[.*?\\]")))
  else if(value=="(")graf %>% update_map(factors=graf$factors %>%
    mutate(label=str_remove_all(label,"\\(.*?\\)")))
}
#' Wrap factor labels
#'
#' @inheritParams parse_commands
#' @param length line length
#'
#' @return A tidymap with factor labels wrapped to `length`
#' @export
#'
#'
#' @examples
pipe_wrap_factors <- function(graf,length=20){
  graf %>%
    update_map(
      factors=graf$factors %>%
    mutate(label=str_remove_all(label,"\n")) %>%
    mutate(label=str_wrap(label,length))
    )
}
#' Wrap link labels
#'
#' @inheritParams parse_commands
#' @param length line length
#'
#' @return A tidymap with link labels wrapped to `length`
#' @export
#'
#'
#' @examples
pipe_wrap_links <- function(graf,length=20){
  graf %>%
    update_map(links=
    graf$links %>%
      mutate(label=str_remove_all(label,"\n")) %>%
    mutate(label=str_wrap(label,length))
    )
}

# outputs -----------------------------------------------------------------


#' Make map-level metrics
#'
#' @inheritParams parse_commands
#'
#' @return A tibble with map-level metrics
#' @export
#'
#'
#' @examples
make_map_metrics <- function(graf){
  ig <- make_igraph_from_map(graf)
  metric=c(
    igraph::edge_connectivity(ig),
    # igraph::clique_num(ig),
    # igraph::count_max_cliques(ig),
    igraph::count_components(ig),
    igraph::count_motifs(ig),
    igraph::diameter(ig),
    igraph::radius(ig),
    igraph::gsize(ig),
    igraph::gorder(ig),
    igraph::reciprocity(ig) %>% round(2),
    igraph::min_cut(ig),
    igraph::mean_distance(ig) %>% round(2)
  )

  name=c(
    "adhesion",
    # "clique_num",
    # "clique_count",
    "component_count",
    "motif_count",
    "diameter",
    "radius",
    "size",
    "order",
    "reciprocity",
    "min_cut",
    "mean_dist"
  )
  description=c(
    "The minimum edge connectivity.",
    # "The size of the largest clique.",
    # "The number of maximal cliques in the graph.",
    "The number of unconnected components in the graph.",
    "The number of motifs in the graph.",
    "The length of the longest geodesic.",
    "The smallest eccentricity in the graph.",
    "The number of edges in the graph.",
    "The number of nodes in the graph.",
    "The proportion of mutual connections in the graph.",
    "The minimum number of edges to remove in order to split the graph into two clusters.",
    "The mean distance between all node pairs in the graph."
  )

  tibble(name,metric,description)
}

factor_click_edit <- function(id){
  as.character(shiny::actionButton(inputId = paste0('factor_click_edit_',id), label = "Rename",class="linky"))
}
factor_click_delete <- function(id){
  as.character(shiny::actionButton(inputId = paste0('factor_click_delete_',id), label = "delete",class="linky"))
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
  as.character(shiny::actionLink(inputId = paste0('link_click_delete_',id), label = "delete",class="linky"))
}


## visNetwork --------------------------------------------------------------

#' Make a visNetwork
#' @description Make a visNetwork (https://datastorm-open.github.io/visNetwork/) from a tidymap.
#'
#' @param graf A tidymap. The factors table and links table may contain additional formatting information like color.background.
#' @param scale Increase from the default 1 to make the map taller.
#'
#' @return A visnetwork
#' @export
#'
#'
#' @examples
make_vn <- function(graf,scale=1,safe_limit=200){

  # browser()
  if(nrow(links_table(graf))>replace_null(safe_limit,200)){
    notify("Map larger than 'safe limit'; bundling and labelling links")
    graf <- graf %>%
      pipe_bundle_links() %>%
      pipe_label_links("frequency") %>%
      pipe_scale_links("frequency")

  }

  # browser()
  if(max(table(graf$factors$size))>1)graf <- graf %>% update_map(factors=.$factors %>% arrange((size))) %>% pipe_normalise_factors_links()#because this is the way to get the most important ones in front

  nodes <- graf$factors %>% mutate(value=size*10) %>%
    select(any_of(xc("factor_id factor_id0 factor_memo  label color.background color.border title group value hidden size"))) ### restrictive in attempt to reduce random freezes
  edges <- graf$links %>%  as_tibble %>% select(-any_of("label")) %>% rename(label=link_label)
  edges <-  edges %>% vn_fan_edges() %>% mutate(width=width*10) %>%
    select(any_of(xc("from to id s.source_id statement_id s.question_id quote color width label link_memo smooth.roundness smooth.enabled smooth.type link_id0")))
  if(nrow(nodes)>1){
    layout <- layout_with_sugiyama(make_igraph(nodes,edges))$layout*-scale
    colnames(layout) <- c("y", "x")
    nodes <- data.frame(nodes, layout)
    ############## don't get tempted to use the internal visnetwork layout functions - problems with fitting to screen, and they are slower ....
  }
  nodes <- nodes %>%   mutate(id=row_number())
  edges <- edges %>%   mutate(id=NULL) # id would be necessary for getting ids from clicks etc, but seems to stop tooltip from working
# browser()
  nodes <-
    nodes %>% mutate(title=paste0(
      map(label,factor_click_name),
      " ",
      map(factor_id0,factor_click_edit),
      # factor_click_name(),
      "</br>",
      "</br>",
      map(factor_id0,factor_click_delete),
      "</br>",
      paste0("Memo:", factor_memo)
      ))
  edges <-
    edges %>% mutate(title=paste0(
      map(link_id0,link_click_delete),
      "</br>",
      quote %>% str_wrap,
      "</br>",
      "Memo:", link_memo,
      "</br>",
      "Source ID:", s.source_id,
      "</br>",
      "Statement ID:", statement_id,
      "</br>",
      "Question ID:", s.question_id
      ))

  visNetwork(nodes,edges,background="white")   %>%
    visNodes(
      shadow = list(enabled = T, size = 10),
      shape = "box",
      font=list(color="black"),
      borderWidth=2,
      scaling = list(label = list(enabled = T)),
      physics = T
    ) %>%
    visEdges(
      smooth = F,
      arrowStrikethrough = T,
      physics = F,
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
      tooltipDelay=100
      ,
      tooltipStyle='color:red;position: fixed;visibility:hidden;width:400px;background-color:aliceblue'
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
    visPhysics(stabilization = T) %>% # ,solver="hierarchicalRepulsion") %>% #,solver="hierarchicalRepulsion") %>%
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
      nodesIdSelection = T
    )
  # # %>%
  #     visIgraphLayout(layout = "layout_with_sugiyama", randomSeed = 123, type = "full")

}


vn_fan_edges <- function(edges){
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
#' @param graf A tidymap. Link and factor tables may contain columns to control formatting
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

#' Make a Graphviz map
#' @description Make a Graphviz map: https://graphviz.org/documentation/
#' @param graf A tidymap. Link and factor tables may contain columns to control formatting
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
make_grviz <- function(
  graf=NULL,
  maxwidth=NULL,
  grv_layout=NULL,
  grv_splines=NULL,
  grv_overlap=NULL,
  color=NULL,
  ranksep_slider=NULL,
  nodesep_slider=NULL,
  safe_limit=NULL

){
  # graf b
  # if(is.null(grv_layout))
  safe_limit <- replace_null(safe_limit,graf %>% attr("set_print") %>% .$safe_limit %>% replace_null(200))

  # if((nrow(graf %>% factors_table)>safe_limit/3))notify("Map larger than 'safe limit'; setting print layout to twopi")
  # if((nrow(graf %>% links_table)>safe_limit))notify("Map larger than 'safe limit'; setting print layout to use straight edges")

  maxwidth <- replace_null(maxwidth,graf %>% attr("set_print") %>% .$maxwidth %>% replace_null("dot"))

  grv_layout <- replace_null(grv_layout,
                             graf %>% attr("set_print") %>% .$grv_layout %>% replace_null(
                               if_else(nrow(graf %>% factors_table)>safe_limit/3,"twopi","dot")))



  grv_splines <- replace_null(grv_splines,graf %>% attr("set_print") %>% .$grv_splines %>% replace_null(if_else(nrow(graf %>% factors_table)>safe_limit/3,"lines","splines")))
  grv_overlap <- replace_null(grv_overlap,graf %>% attr("set_print") %>% .$grv_overlap %>% replace_null(F))
  color <- replace_null(color,graf %>% attr("set_print") %>% .$color %>% replace_null("grey"))
  ranksep_slider <- replace_null(ranksep_slider,graf %>% attr("set_print") %>% .$ranksep_slider %>% replace_null(3))
  nodesep_slider <- replace_null(nodesep_slider,graf %>% attr("set_print") %>% .$nodesep_slider %>% replace_null(20))

  if(is.null(graf))return()
  # graf <- graf %>% pipe_fix_columns()

  if(!is.null(safe_limit) & nrow(links_table(graf))>replace_null(safe_limit,200)){
    notify("Map larger than 'safe limit'; bundling and labelling links")
    graf <- graf %>%
      pipe_bundle_links() %>%
      pipe_label_links("frequency")

    # if(nrow(factors_table(graf))>safe_limit) graf <- graf %>% pipe_select_factors(safe_limit/10)
  }


  # if("frequency" %in% colnames(links_table(graf)))graf <-  graf %>% mutate(tooltip=as.character(n))
# browser()
# browser()
  factors <- graf$factors %>%
    mutate(label=clean_grv(label) )%>%
    # mutate(cluster=if_else(is.na(cluster),"",cluster) )%>%
    mutate(tooltip=label)%>%
    mutate(fillcolor=color.background) %>%
    mutate(color=color.border) %>%
    mutate(fontsize=(size+2)*20) %>%
    mutate(fontcolor="black") %>%
    select(any_of(xc("label size tooltip fillcolor color fontsize fontcolor cluster"))) %>%
    mutate(factor_id=row_number())

  links <- graf$links %>%
    select(any_of(xc("from to color width link_label width from_label to_label"))) %>%
    rename(label=link_label) %>%
    mutate(from=as.numeric(from))%>%
    mutate(to=as.numeric(to))%>%
    mutate(label=if_else(label=="",".",label))%>%
    mutate(label=clean_grv(label) )%>%
    mutate(label=replace_na(label,"."))%>% # obscure! if all are =="", error
    mutate(penwidth=width*48)%>%
    mutate(arrowsize=.5) %>%
    mutate(arrowhead="vee")

# browser()

  grv <-
    DiagrammeR::create_graph() %>%
    add_nodes_from_table(factors  %>% mutate(id=factor_id),label_col="label") %>%
    add_edges_from_table(links,from_col="from",to_col="to",from_to_map = id_external) %>%

    add_global_graph_attrs("layout", grv_layout, "graph") %>%
    add_global_graph_attrs("splines", grv_splines, "graph") %>%
    add_global_graph_attrs("overlap", grv_overlap, "graph") %>%
    add_global_graph_attrs("labelloc", "bottom","graph") %>%
    add_global_graph_attrs("tooltip", " ", "graph") %>%
    add_global_graph_attrs("rankdir", "LR", "graph") %>%
    add_global_graph_attrs("fontsize", "28", "graph") %>%
    add_global_graph_attrs("fontname", "Arial", "graph") %>%
    add_global_graph_attrs("nodesep", 1, "graph") %>%
    add_global_graph_attrs("ranksep", 1.5*log(nrow(factors_table(graf))), "graph") %>%
    add_global_graph_attrs("style", "filled,dashed", "graph") %>%
    add_global_graph_attrs("color", color, "graph") %>%
    add_global_graph_attrs("fillcolor", color, "graph") %>%

    add_global_graph_attrs("shape", "box", "node") %>%
    add_global_graph_attrs("style", "rounded, filled", "node") %>%
    add_global_graph_attrs("fixedsize", "false", "node") %>%
    add_global_graph_attrs("fontcolor", "black", "node") %>%
    # add_global_graph_attrs("fontsize", "80", "node") %>%
    add_global_graph_attrs("margin", "0.3", "node") %>%
    add_global_graph_attrs("penwidth", "14", "node") %>%
    add_global_graph_attrs("width", "0", "node") %>%
    add_global_graph_attrs("height", "0", "node")  %>%

    add_global_graph_attrs("fontsize", 63, "edge") %>%
    add_global_graph_attrs("fontcolor", "#666666", "edge") %>%
    add_global_graph_attrs("arrowsize", .5, "edge")

  return(
    grv %>% DiagrammeR::render_graph()
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



# Shiny UI functions ------------------------------------------------------


#' Robustness UI
#'
#' @param graf
#'
#' @return
#' @export
#'
#' @examples
robustUI <- function(graf){
  # browser()
  flow <- attr(graf$links,"flow")$summary
  if(is.null(flow)) {notify("No paths");return(NULL)}
  if(nrow(flow)==0) {notify("No paths");return(NULL)}



  if(!is.null(flow)){
    flow <-  flow %>% column_to_rownames(var="row_names")
    flow[is.infinite(as.matrix(flow))] <- NA # because the colorbar plugin chokes on Inf
  # browser()

    flow <- flow %>%
      arrange(UQ(sym(colnames(flow)[1])) %>% desc)

    ## because if all targets / all sources is NA, top row will not be All targets
    if("All targets" %in% rownames(flow)){
      # flow <-
      #   bind_rows(g["All targets",],flow[rownames(flow)!="All targets",])
    }

    flow %>%
      datatable(caption="Maximum flow / minimum cut",rownames = T,editable=F,extensions = 'Buttons',
                options = list(
        # columnDefs = list(list(width = paste0(100/ncol(row),"%"), targets = (0:ncol(flow)))),
        autoWidth = F,
        autoHideNavigation=T,


        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print', I('colvis'))
      )) %>% add_heat_map(flow)
  }}


#' Add heat map
#'
#' @param dt
#' @param flow
#'
#' @return
#' @export
#'
add_heat_map <- function(dt,flow){
  # browser()
  flow2 <- flow %>% ungroup%>% mutate(across(where(~!is.numeric(.)),~0))
heat_breaks <- c(quantile(flow2, probs = seq(.05, .9899, .05), na.rm = TRUE),
                         quantile(flow2, probs = seq(.99, 1, .001), na.rm = TRUE))
heat_colors <- round(seq(255, 90, length.out = length(heat_breaks) + 1), 0) %>%
  paste0("rgb(",.,",", ., ",", "255)")

formatStyle(dt,names(flow),
                                          backgroundColor = styleInterval(heat_breaks,heat_colors))
}



