library(igraph)
library(DiagrammeR)
library(visNetwork)
library(tidyverse)
library(tidygraph)
library(scales)
#library(RColorBrewer)


# internal utilities-----------------------------------------------------------------------------

notify <- message # alias
return_notify <- function(tex){
  notify(tex,3)
  return()
}

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


xc <- function(x, sep = " ") {
  str_split(x, sep)[[1]]
}

`%notin%` <- Negate(`%in%`)

escapeRegex <- function(string){ #from hmisc
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",
       string)
}

# internal tidymap functions ----------------------------------------------------


has_statements <- function(graf) !is.null(graf %>% statements_table)

add_statements <- function(graf,statements){
  attr(graf,"statements") <- statements
  graf
}
add_attribute <- function(graf,value,attr="flow"){
  attr(graf,attr) <- value
  graf
}




factor_colnames <- function(graf)graf %>% factors_table %>% colnames
link_colnames <- function(graf)graf %>% links_table %>% colnames

print_more <- function(graf,n=99){
  graf %>% activate(nodes) %>% as_tibble %>% print(n=n)
  graf %>% activate(edges) %>% as_tibble %>% print(n=n)
}

load_graf_from_rds <- function(name){
  tmp <- readRDS(name)
  tbl_graph(tmp$factors,tmp$links)
}


make_search <- function(x)x %>% escapeRegex %>% str_replace_all(" OR ","|") %>% str_trim



zoom_inner <- function(string,n,char){
  string %>% map(~str_split(.,char) %>% `[[`(1) %>% `[`(1:n) %>% keep(!is.na(.)) %>% paste0(collapse=char)) %>% unlist
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

div_pal_n <- function(vec,lo=lo,hi=hi,mid=mid){
  div_gradient_pal(low=lo,high=hi,mid=mid)(rescale(vec)) %>% alpha(.6)
}
viridis_pal_n <- function(vec){
  vec <- vec %>% as.factor %>% as.numeric
  viridis_pal()(length(unique(vec)))[vec] %>% alpha(.6)
}
brewer_pal_n <- function(vec){
  vec <- vec %>% as.factor %>% as.numeric
  scales::brewer_pal("qual")(length(unique(vec)))[vec] %>% alpha(.9)
}
create_colors <- function(vec,lo=lo,hi=hi,mid=mid){
  if(class(vec)=="character") brewer_pal_n(vec) else div_pal_n(vec,lo=lo,hi=hi,mid=mid)
}




cluster_fun <- function(labs,tex){
  ifelse(str_detect(labs,tex),tex,"")
}

calculate_robustness_inner <- function(graf){
  if("found_from" %notin% factor_colnames(graf)) {warning("No found_from column");return(NA)}
  if("found_to" %notin% factor_colnames(graf)) {warning("No found_to column");return(NA)}

  if(nrow(factors_table(graf))==0) {warning("No paths");return(NA)}
  graf <- graf %N>% pipe_bundle_links() %E>%
    mutate(n=if_else(is.na(n),1L,as.integer(n))) %>%
    activate(nodes)


  from_vec <- factors_table(graf) %>% filter(found_from) %>% pull(label)
  to_vec <- factors_table(graf) %>% filter(found_to) %>% pull(label)
  newnodes <- tibble(
    label=c(from_vec,"_super_source_"))

  newedges <- tibble(
    from="_super_source_",
    to=from_vec,
    capacity=Inf
  )
  newgraf <- tbl_graph(newnodes,newedges)


  graf <- graf %>% graph_join(newgraf)
  # sink
  # sink_vec <- c("six", "seven")
  newnodes <- tibble(
    label=c(to_vec,"_super_sink_"))

  newedges <- tibble(
    to="_super_sink_",
    from=to_vec,
    capacity=Inf
  )
  newgraf <- tbl_graph(newnodes,newedges)


  graf <-
    graf %>% graph_join(newgraf) %E>%
    mutate(capacity=if_else(is.na(capacity),1,capacity)) %>%
    mutate(capacity=pmax(n,capacity,na.rm=T)) %E>%
    filter(from!=to) %>%
    activate(nodes)
  source <- V(graf)[(graf %>% factors_table)$label=="_super_source_"]
  sink <- V(graf)[(graf %>% factors_table)$label=="_super_sink_"]
  res <- graf %N>%
    max_flow(source=source, target=sink)
  sources <- V(graf)[(graf %>% factors_table)$found_from %>% replace_na(F)]
  sinks <- V(graf)[(graf %>% factors_table)$found_to %>% replace_na(F)]

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


  all_flows <-

    sinkvec %>% map(function(y)(sourcevec %>% map(function(x) if(x %in% sinks) Inf else max_flow(graf,x,y)$value)) %>% unlist) %>%
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
find_fun <- function(graf,field=NULL,value,operator=NULL,what){
# browser()
  if(is.null(field) & is.null(operator)){
    field="label"
    operator="contains"
  }

  if(field %in% xc("label text")){

    value <- value %>% make_search
  }
  # if("what"=="factors") graf <- graf %>% activate(nodes) else
  # if("what"=="links") graf <- graf %>% activate(edges) else
  #   df <- graf %>% attr("statements")
    df <- graf

  if(field %notin% colnames(df)) {notify("No such field");return(df)}

  if(operator=="contains"){df <- df %>%  mutate(found=str_detect(tolower(unwrap(UQ(sym(field)))),tolower(value)))} else
    if(operator=="notcontains"){df <- df %>%  mutate(found=!str_detect(tolower(unwrap(UQ(sym(field)))),tolower(value)))} else
      if(operator %in% xc("= equals equal")){df <- df %>%  mutate(found=(tolower(unwrap(UQ(sym(field))))==tolower(value)))} else
        if(operator %in% xc("notequals notequal")){df <- df %>%  mutate(found=(tolower(unwrap(UQ(sym(field))))!=tolower(value)))} else
          if(operator %in% xc("greater")){df <- df %>%  mutate(found=(as.numeric(UQ(sym(field)))>as.numeric(value)))} else
            if(operator %in% xc("less")){df <- df %>%  mutate(found=(as.numeric(UQ(sym(field)))<as.numeric(value)))} else
              if(operator %in% xc("starts start")){df <- df %>%  mutate(found=str_detect(tolower(unwrap(UQ(sym(field)))),paste0("^",tolower(value))))} else
                if(operator %in% xc("ends end")){df <- df %>%  mutate(found=str_detect(tolower(unwrap(UQ(sym(field)))),paste0(tolower(value),"$")))}


  return(df)

}
# constants ---------------------------------------------------------

operator_list=xc("= less greater notcontains notequals notequal equals equal contains starts ends start end")

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
factors_table <- function(graf)graf %>%
  activate(nodes) %>% as_tibble

#' @rdname tibbles
#' @export
#'
links_table <- function(graf)graf %>%
  activate(edges) %>% as_tibble

#' @rdname tibbles
#' @export
#'
statements_table <- function(graf)graf %>%
  attr("statements")

#' @rdname tibbles
#' @export
#'
links_table_full <- function(graf){
  graf %>%
    links_table %>%
    left_join(factors_table(graf) %>% mutate(id=row_number()) %>% select(from=id,label_from=label),by="from") %>%
    left_join(factors_table(graf) %>% mutate(id=row_number()) %>% select(to=id,label_to=label),by="to") %>%
    select(label_from,label_to,everything())
}

# Parser ------------------------------------------------------------------

#' Parse line
#'
#' The engine for parse_commands
#' @param graf A tidymap representing a causal map.
#' @param line A line of text to be parsed
#' @return A list containing the function name and a list of parameters.
#' @export
parse_line <- function(line,graf){
  if(str_trim(line)=="")return()
  fun <- word(line, 1,2, sep=" ")
  if(is.na(fun)){notify("No such function");return(graf %>% filter(F))}
  if(!exists(str_replace(fun," ","_") %>% paste0("pipe_",.))){notify("No such function");return(graf %>% filter(F))}

  body <-
    str_remove(line,fun) %>%
    str_trim


  # browser()
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
        }else
          if(fun %in% c("hide factors") ){
            fun <- "find factors"

            vals=list(
              graf=graf,
              field="label",
              value=body ,

              operator="notcontains"
            )

          }
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
parse_commands <- function(graf,tex){
  tex <- tex %>% replace_null("") %>% str_split("\n") %>% `[[`(1) %>% str_trim() #%>% escapeRegex
  if(length(tex)>1)tex <- tex %>% keep(.!="")
  if(tex[[1]]=="") graf <- graf else {

    for(line in tex){
      # browser()
      tmp <- parse_line(line,graf)

      graf <- possibly(~do.call(tmp$fun,tmp$vals),otherwise=graf)()

    }
  }
  graf
}


# main graph functions ----------------------------------------------------

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

  graf %>%
    activate(edges) %>%
    left_join(attr(graf,"statements") ,by="statement_id") %>%
    activate(nodes)
}


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
pipe_find_factors <- function(graf,field=NULL,value,operator=NULL,up=0,down=0){

  st <- attr(graf,"statements")
  df <- graf %>% factors_table %>% find_fun(field,value,operator)
  graf <- tbl_graph(df,links_table(graf)) %>% add_statements(st)
  downvec <- graf %>% distances(to=graf %>% factors_table %>% pull(found),mode="in") %>% apply(1,min) %>% `<=`(down)
  upvec <- graf %>% distances(to=graf %>% factors_table %>% pull(found),mode="out") %>% apply(1,min) %>% `<=`(up)
  # browser()
  if(any(upvec)|any(downvec))graf %>% mutate(upvec=upvec,downvec=downvec) %>% filter(found|upvec|downvec) else graf %>% filter(F)
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
pipe_find_links <- function(graf,field=NULL,value,operator=NULL){
  st <- attr(graf,"statements")
  df <- graf %>% links_table %>% find_fun(field,value,operator)
  tbl_graph(factors_table(graf),df) %E>% filter(found) %>% add_statements(st) %>% activate(nodes)

}

#' Find statements
#'
#' @inheritParams pipe_find_factors
#' @return A tidymap filtered by statements.
#' @export
#'
#' @examples
pipe_find_statements <- function(graf,field,value,operator="="){
  if(!has_statements(graf)) {notify("No statements");return(graf)}


  tmp <- graf %>%
    attr("statements") %>% find_fun(field,value,operator)  %>%
    filter(found)
  return(graf %>%
           activate(edges) %>%
           filter(statement_id %in% tmp$statement_id) %>%
           add_statements(tmp) %>%
           activate(nodes)
         )


}


#' Select links
#'
#' @inheritParams parse_commands
#' @param top Bundle the links and select only the `top` links in terms of their frequency
#' @param all
#' @param is_proportion
#'
#' @return
#' @export
#'
#' @examples
pipe_select_links <- function(graf,top){
  graf %>%
    pipe_bundle_links() %E>%
    arrange(desc(n)) %>%
    slice(1:top) %>%
    select(from,to,n,everything()) %>%
    select(1:7) %>%
    activate(nodes)
}


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
pipe_select_factors <- function(graf,top=20,all=F){
  graf %>%
    activate(nodes) %>%
    mutate(n = centrality_degree()) %>%
    mutate(n=rank(n)) %>%
    arrange(desc(n)) %>%
    slice(1:top)

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
pipe_zoom_factors <- function(graf,level,separator=";",hide=T){
  # browser()
  level=as.numeric(level)
  hide=as.logical(hide)
  # flow=attr(graf,"flow")
  statements <- graf %>% statements_table()
  if(level<1) return(graf)
  gr <- graf %>%
    activate(nodes) %>%
    filter(!hide | str_detect(label,separator)) %>%
    mutate(label=if_else(str_detect(label,separator),zoom_inner(label,level,separator),label)) %>%
    convert(to_contracted,label,simplify=F)  %>%
    mutate(zoomed_=str_detect(label,separator))

  tbl_graph(gr %>% factors_table %>% as.data.frame %>% select(label=1,zoomed_),gr %>% links_table  %>% as.data.frame) %>%
    add_statements(statements)# %>%
    # add_attribute(flow,"flow")

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
  graf <- graf %>% activate(nodes)
  statements <- graf %>% statements_table()
  value <- value %>% make_search

  gr <-
    graf %>%
    pipe_fix_columns() %>%
    mutate(
      label=if(value=="")
        str_match(label,"^[^ ]*") %>% `[`(,1)
      else if_else(str_detect(label,value),str_match(label,paste0(value)),label)
    ) %>%
    group_by(label) %>%
    mutate(new_id=cur_group_id()) %>%
    ungroup

  new_id <- gr %>% pull(new_id) %>% unlist
  contract(gr,mapping=new_id,vertex.attr.comb = list(label="first",n="sum","ignore")) %>%
    as_tbl_graph() %>%
    add_statements(statements)


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
    graf %N>% mutate(cluster = nodes$cluster)
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
  graf <- graf %>% activate(nodes)
  from <- from %>% make_search
  to <- to %>% make_search
  if(from=="" & to =="") return(graf)
  graf <- graf %>%
    mutate(found_from=str_detect(tolower(label),tolower(from))) %>%
    mutate(found_to=str_detect(tolower(label),tolower(to))) %>%
    mutate(found_type=paste0(if_else(found_from,"source","-"),if_else(found_to,"target","-"))) %>%
    mutate(found_any=found_from|found_to)

  # browser()

  if(!any(graf %>% factors_table %>% pull(found_any))) return(graf %>% filter(F))

  tracedownvec <- graf %>% distances(to=graf %>% factors_table %>% pull(found_from),mode="in") %>% apply(1,min)
  traceupvec <- graf %>% distances(to=graf %>% factors_table %>% pull(found_to),mode="out") %>% apply(1,min)

  # here we need to intervene to make sure that influences don't move closer to the source, as this is a kind of loop

  bothvecsum <- `+`(tracedownvec,traceupvec)
  bothvec <- bothvecsum<=length
  if(min(bothvecsum)<Inf) graf <- graf %>% mutate(traceupvec=traceupvec,
                                                  tracedownvec=tracedownvec,
                                                  bothvec,
                                                  found=found_from|found_to
  ) %>% filter(bothvec) else graf %>% filter(F)

  sums <- graf %>% factors_table %>% select(found_from,found_to) %>% colSums(na.rm=T)
  if((sums[1]*sums[2])>10000){
    # if(sum(found_from,na.rm=T)*sum(found_to,na.rm=T)>10){
    notify("too much to trace")
    return(graf)
  }


# all_flows <- calculate_robustness(graf)

# notify(glue("Number of cuts is {res$cut %>% length}"))
  graf %>%
    activate(nodes) %>%
    filter(label!="_super_sink_" & label!="_super_source_")

  # attr(graf,"flow")=all_flows
  # graf



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
  if("found_from" %notin% factor_colnames(graf)) {warning("No found_from column");return(NA)}
  if("found_to" %notin% factor_colnames(graf)) {warning("No found_to column");return(NA)}
  if(field %>% replace_null("")=="")field <- NULL

  if(is.null(field)) res$summary <- (calculate_robustness_inner(graf)) else

  if(field %notin% link_colnames(graf)) {warning("Field not found");res$summary <- graf}
   else {



  vec <- graf %>%
    links_table() %>%
    pull(UQ(sym(field))) %>%
    unique

  if("" %in% vec){warning("Vector contains an empty string");vec <- vec %>% keep(.!="")}

  # browser()
  res <- vec %>% set_names(vec) %>% map(
    function(x) graf %>%
      pipe_find_links(field=field,value=x,operator="=") %>%
      calculate_robustness_inner()
  )


  summary <- res %>% map(~(select(.,-1))%>% mutate_all(~if_else(.>0 ,1,0))) %>%
    Reduce(`+`,.)
  res$summary <- cbind(res[[1]][,1],summary) %>% as_tibble





   }
  graf %>% add_attribute(res,"flow")

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
  graf %>%
    activate(nodes) %>%
    filter(!node_is_isolated())
}



pipe_flip_opposites <- function(graf,flipchar="~"){
  graf %N>%
    mutate(
      is_flipped=str_detect(label,paste0("^ *",flipchar)),
      label=if_else(is_flipped,flip_vector(label,flipchar = flipchar),label)
    ) %>%
    activate(edges) %>%
    mutate(from_flipped=.N()$is_flipped[from]) %>%
    mutate(to_flipped=.N()$is_flipped[to]) %>%
    activate(nodes)
}



# zero_to_one <- function(vec)(vec-min(vec,na.rm=T))/(max(vec,na.rm=T)-min(vec,na.rm=T))

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
#' if(F)cashTransferMap %>% pipe_merge_statements() %>%  pipe_select_factors(10) %>% pipe_bundle_links(counter="n",group="1. Sex")%>% pipe_label_links(field = "n") %>% pipe_color_links(field="1. Sex") %>% pipe_scale_links() %>%  make_grviz()
#' # or, counting sources rather than statements:
#' if(F)cashTransferMap %>% pipe_merge_statements() %>%  pipe_select_factors(10) %>% pipe_bundle_links(group="1. Sex",counter="#SourceID")%>% pipe_label_links(field = "n") %>% pipe_color_links(field="1. Sex") %>% pipe_scale_links() %>%  make_grviz()
pipe_bundle_links <- function(graf,counter="n",group=NULL){
  # browser()
  statements <- graf %>% statements_table()
  flow <- graf %>% attr("flow")
  nodes <- factors_table(graf)
  edges <- links_table(graf)
  if(nrow(nodes)==0) return(NULL)
  coln <- link_colnames(graf)


  if(counter %notin% link_colnames(graf) & counter!="n" ) {notify("no such counter");return(graf)}
  if(!is.null(group)){if(group %notin% coln) {notify("no such counter");return(graf)}}

  if(is.null(group)) edges <- edges %>% group_by(from,to) else edges <- edges %>% group_by(from,to,UQ(sym(group)))

  if(counter=="n"){

  if("n" %in% coln)edges <- edges %>%
    mutate(rn_=row_number()) %>%
    mutate(n=sum(n))
  else edges <- edges %>%
    mutate(rn_=row_number()) %>%
    mutate(n=n())
} else edges <- edges %>%
    mutate(rn_=row_number()) %>%
    mutate(n=length(unique(UQ(sym(counter)))))




  tbl_graph(nodes,edges %>% filter(rn_==1) %>% ungroup %>% select(-rn_)) %>%
    activate(nodes) %>%
    add_statements(statements) %>%
    add_attribute(flow)


}

## pipes but not for use with parser -------------------------------------------------------------


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

  # if(!("color" %in% factor_colnames(graf))) graf <- graf %N>% mutate(color="#222222")
  if(!("color.background" %in% factor_colnames(graf))) graf <- graf %N>% mutate(color.background="#aaaaee77")
  if(!("color.border" %in% factor_colnames(graf))) graf <- graf %N>% mutate(color.border="#222222")
  if(!("n" %in% factor_colnames(graf))) graf <- graf %N>% mutate(n=1L)
  if(!("size" %in% factor_colnames(graf))) graf <- graf %N>% mutate(size=1L)
  if(!("found" %in% factor_colnames(graf))) graf <- graf %N>% mutate(found=1L)
  if(!("color" %in% link_colnames(graf))) graf <- graf %E>% mutate(color="#22446688")
  if(!("n" %in% link_colnames(graf))) graf <- graf %E>% mutate(n=1L)
  if(!("capacity" %in% link_colnames(graf))) graf <- graf %E>% mutate(capacity=1L)
  if(!("label" %in% link_colnames(graf))) graf <- graf %E>% mutate(label="")
  if(!("width" %in% link_colnames(graf))) graf <- graf %E>% mutate(width=.2)
  if(!("flow" %in% link_colnames(graf))) graf <- graf %E>% mutate(flow=1L)
  graf %>% activate(nodes)
}

#' Create factor metrics
#'
#' @inheritParams parse_commands
#'
#' @return A tidymap with new or overwritten columns for factor metrics.
#' @export
#'
#'
#' @examples

pipe_metrics <- function(graf){
  # browser()
  if(is.null(graf)){notify("No graph for metrics");return(graf)}

  graf  %N>%
    mutate(
      group=suppressMessages(group_infomap()),
      "in_degree"=centrality_degree(mode = "in"),
      "out_degree"=centrality_degree(mode = "out"),
      n=in_degree+out_degree,
      # keyplayer=node_is_keyplayer(),
      "is_centre"=node_is_center(mode = "out"),
      "is_cut"=node_is_cut(),
      betweenness=centrality_betweenness(directed = T) %>% round(2)
    )
}

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
pipe_scale_factors <- function(graf,field="n"){
  graf <- pipe_metrics(graf)
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  class <- graf %>% factors_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  # browser()
  graf %N>% mutate(size=scales::rescale(UQ(sym(field)),to=c(0.2,1))) %>% activate(nodes)

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
pipe_label_factors <- function(graf,field="n",clear=F){
  clear=as.logical(clear)
  graf <- pipe_metrics(graf)
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  graf %N>%
    mutate(label=paste0((if(clear)NULL else paste0(label,". ")) %>% keep(.!=""),field,": ",UQ(sym(field)),". ")) %>% activate(nodes)
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
pipe_color_factors <- function(graf,field="n",lo="green",hi="blue",mid="gray",fixed=NULL){
  if(!is.null(fixed))return(graf %N>% mutate(color.background=fixed))
  graf <- pipe_metrics(graf)
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  graf %N>% mutate(color.background=create_colors(UQ(sym(field)),lo=lo,hi=hi,mid=mid))
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
pipe_color_borders <- function(graf,field="n",lo="green",hi="blue",mid="gray",fixed=NULL){
  if(!is.null(fixed))return(graf %N>% mutate(color.border=fixed))
  graf <- pipe_metrics(graf)
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  graf %N>% mutate(color.border=create_colors(UQ(sym(field)),lo=lo,hi=hi,mid=mid))
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
pipe_color_links <- function(graf,field="n",lo="green",hi="blue",mid="gray",fixed=NULL){
  if(!is.null(fixed))return(graf %E>% mutate(color=fixed) %>% activate(nodes))
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
  graf %E>% mutate(color=create_colors(UQ(sym(field)),lo=lo,hi=hi,mid=mid)) %>% activate(nodes)


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
pipe_fade_factors <- function(graf,field="n"){
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  if("color.background" %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  class <- graf %>% factors_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  # browser()
  graf %N>% mutate(color.background=alpha(color.background,scales::rescale(UQ(sym(field)),to=c(0.2,1)))) %>% activate(nodes)
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
pipe_fade_links <- function(graf,field="n"){
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
  if("color" %notin% link_colnames(graf)){warning("No such column");return(graf)}
  class <- graf %>% links_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  # browser()
  graf %E>% mutate(color=alpha(color,scales::rescale(UQ(sym(field)),to=c(0.2,1)))) %>% activate(nodes)
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
pipe_scale_links <- function(graf,field="n",fixed=NULL){
  if(!is.null(fixed))return(graf %E>% mutate(width=fixed) %>% activate(nodes))
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}

  class <- graf %>% links_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  # browser()
  graf %E>% mutate(width=scales::rescale(UQ(sym(field)),to=c(0.1,1))) %>% activate(nodes)
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
pipe_label_links <- function(graf,field="n",clear=F){
  clear=as.logical(clear)
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
  graf %E>%
    mutate(label=paste0((if(clear)NULL else paste0(label,". ")) %>% keep(.!=""),field,": ",UQ(sym(field)),". ")) %>% activate(nodes)
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
  if(value=="[")graf %N>%
    mutate(label=str_remove_all(label,"\\[.*\\]"))
  else if(value=="(")graf %N>%
    mutate(label=str_remove_all(label,"\\(.*\\)"))
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
  graf %N>%
    mutate(label=str_remove_all(label,"\n")) %>%
    mutate(label=str_wrap(label,length))
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
  graf %E>%
    mutate(label=str_remove_all(label,"\n")) %>%
    mutate(label=str_wrap(label,length)) %>%
    activate(nodes)
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
  metric=c(
    igraph::edge_connectivity(graf),
    # igraph::clique_num(graf),
    # igraph::count_max_cliques(graf),
    igraph::count_components(graf),
    igraph::count_motifs(graf),
    igraph::diameter(graf),
    igraph::radius(graf),
    igraph::gsize(graf),
    igraph::gorder(graf),
    igraph::reciprocity(graf) %>% round(2),
    igraph::min_cut(graf),
    igraph::mean_distance(graf) %>% round(2)
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
    "The number of motifs in a graph.",
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
make_vn <- function(graf,scale=1){
  graf <- graf %>% pipe_fix_columns()
  # browser()
  nodes <- graf %N>% as_tibble %>% mutate(value=size*10) %>%
    select(any_of(xc("label color.background color.border title group value hidden size"))) ### restrictive in attempt to reduce random freezes
  # browser()
  edges <- graf %E>% as_tibble
  edges <-  edges %>% vn_fan_edges() %>% mutate(width=width*10) %>%
    select(any_of(xc("from to id color width label smooth.roundness smooth.enabled smooth.type")))
  if(nrow(nodes)>1){
    layout <- layout_with_sugiyama(tbl_graph(nodes,edges))$layout*-scale
    colnames(layout) <- c("y", "x")
    nodes <- data.frame(nodes, layout)
    ############## don't get tempted to use the internal visnetwork layout functions - problems with fitting to screen, and they are slower ....
  }
  nodes <- nodes %>%   mutate(id=row_number())
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
      selectConnectedEdges = F
    ) %>%
    visEvents(click ="function(data) {
                Shiny.onInputChange('node_click', data.nodes);
                Shiny.onInputChange('edge_click', data.edges);
                ;}",
              blurNode = "function(nodes) {
                Shiny.onInputChange('node_click', null);
                ;}",
              blurEdge = "function(edges) {
                Shiny.onInputChange('edge_click', null);
                ;}")  %>%
    visPhysics(stabilization = T) %>% # ,solver="hierarchicalRepulsion") %>% #,solver="hierarchicalRepulsion") %>%
    visOptions(
      collapse = F,
      manipulation=T
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
  grv_layout="dot",
  grv_splines ="splines",
  grv_overlap=F,
  color="grey",
  ranksep_slider=3,
  nodesep_slider=20,
  safe_limit=200

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
  if((nrow(graf %>% links_table)>safe_limit))notify("Map larger than 'safe limit'; setting print layout to twopi")
  if((nrow(graf %>% links_table)>safe_limit))notify("Map larger than 'safe limit'; setting print layout to use straight edges")

  maxwidth <- replace_null(maxwidth,graf %>% attr("set_print") %>% .$maxwidth %>% replace_null("dot"))
  grv_layout <- replace_null(grv_layout,graf %>% attr("set_print") %>% .$grv_layout %>% replace_null(if_else(nrow(graf %>% factors_table)>safe_limit,"twopi","dot")))
  grv_splines <- replace_null(grv_splines,graf %>% attr("set_print") %>% .$grv_splines %>% replace_null(if_else(nrow(graf %>% factors_table)>safe_limit,"lines","splines")))
  grv_overlap <- replace_null(grv_overlap,graf %>% attr("set_print") %>% .$grv_overlap %>% replace_null(F))
  color <- replace_null(color,graf %>% attr("set_print") %>% .$color %>% replace_null("grey"))
  ranksep_slider <- replace_null(ranksep_slider,graf %>% attr("set_print") %>% .$ranksep_slider %>% replace_null(3))
  nodesep_slider <- replace_null(nodesep_slider,graf %>% attr("set_print") %>% .$nodesep_slider %>% replace_null(20))

  if(is.null(graf))return()
  graf <- graf %>% pipe_fix_columns()

  if(!is.null(safe_limit) & nrow(links_table(graf))>replace_null(safe_limit,200)){
    notify("Map larger than 'safe limit'; bundling and labelling links")
    graf <- graf %>%
      pipe_bundle_links() %>%
      pipe_label_links("n")

    if(nrow(factors_table(graf))>safe_limit) graf <- graf %>% pipe_select_factors(safe_limit/10)
  }


  if("id" %in% colnames(factors_table(graf)))graf <-  graf %>% select(-id)
  # if("n" %in% colnames(links_table(graf)))graf <-  graf %>% mutate(tooltip=as.character(n))

  grv <-
    graf %>%
    activate(nodes) %>%
    mutate(label=clean_grv(label) )%>%
    mutate(tooltip=label)%>%
    mutate(fillcolor=color.background) %>%
    mutate(color=color.border) %>%
    mutate(fontsize=(size+5)*10) %>%
    mutate(fontcolor="black") %>%
    activate(edges) %>%
    mutate(label=if_else(label=="",".",label))%>%
    mutate(label=clean_grv(label) )%>%
    mutate(label=replace_na(label,"."))%>% # obscure! if all are =="", error
    # mutate(label="")%>%
    # select(-label) %>%
    mutate(penwidth=width*28)%>%
    mutate(arrowsize=3) %>%
    mutate(arrowhead="normal") %>%
    select(-any_of("id")) %>%
    activate(nodes) %>%
    DiagrammeR::from_igraph() %>%
    DiagrammeR::add_global_graph_attrs("layout", grv_layout, "graph") %>%
    DiagrammeR::add_global_graph_attrs("splines", grv_splines, "graph") %>%
    DiagrammeR::add_global_graph_attrs("overlap", grv_overlap, "graph") %>%
    DiagrammeR::add_global_graph_attrs("labelloc", "bottom","graph") %>%
    DiagrammeR::add_global_graph_attrs("tooltip", " ", "graph") %>%
    DiagrammeR::add_global_graph_attrs("rankdir", "LR", "graph") %>%
    DiagrammeR::add_global_graph_attrs("fontsize", "28", "graph") %>%
    DiagrammeR::add_global_graph_attrs("fontname", "Arial", "graph") %>%
    DiagrammeR::add_global_graph_attrs("nodesep", 1, "graph") %>%
    DiagrammeR::add_global_graph_attrs("ranksep", 1.5*log(nrow(factors_table(graf))), "graph") %>%
    DiagrammeR::add_global_graph_attrs("style", "filled,dashed", "graph") %>%
    DiagrammeR::add_global_graph_attrs("color", color, "graph") %>%
    DiagrammeR::add_global_graph_attrs("fillcolor", color, "graph") %>%

    DiagrammeR::add_global_graph_attrs("shape", "box", "node") %>%
    DiagrammeR::add_global_graph_attrs("style", "rounded, filled", "node") %>%
    DiagrammeR::add_global_graph_attrs("fixedsize", "false", "node") %>%
    DiagrammeR::add_global_graph_attrs("fontcolor", "black", "node") %>%
    DiagrammeR::add_global_graph_attrs("fontsize", "80", "node") %>%
    DiagrammeR::add_global_graph_attrs("margin", "0.3", "node") %>%
    DiagrammeR::add_global_graph_attrs("penwidth", "14", "node") %>%
    DiagrammeR::add_global_graph_attrs("width", "0", "node") %>%
    DiagrammeR::add_global_graph_attrs("height", "0", "node")  %>%

    DiagrammeR::add_global_graph_attrs("fontsize", 63, "edge") %>%
    DiagrammeR::add_global_graph_attrs("fontcolor", "#666666", "edge")

  return(
    grv %>% DiagrammeR::render_graph()
         )

}

clean_grv <- function(tx){
  tx %>% str_replace_all("'","&rsquo;") %>%
    str_replace_all("\"","&rsquo;") %>%
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
  flow <- attr(graf,"flow")$summary
  if(is.null(flow)) {notify("No paths");return(NULL)}
  if(nrow(flow)==0) {notify("No paths");return(NULL)}



  if(!is.null(flow)){
    flow <-  flow %>% column_to_rownames(var="row_names")
    flow[is.infinite(as.matrix(flow))] <- NA # because the colorbar plugin chokes on Inf
    brks <- c(quantile(flow, probs = seq(.05, .9899, .05), na.rm = TRUE),
              quantile(flow, probs = seq(.99, 1, .001), na.rm = TRUE))
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(",.,",", ., ",", "255)")}
    flow %>%
      arrange(UQ(sym(colnames(flow)[1])) %>% desc) %>%
      datatable(caption="Maximum flow / minimum cut",rownames = T,editable=F,extensions = 'Buttons',
                options = list(
        # columnDefs = list(list(width = paste0(100/ncol(row),"%"), targets = (0:ncol(flow)))),
        autoWidth = F,
        autoHideNavigation=T,


        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print', I('colvis'))
      )) %>% formatStyle(names(flow),
                         backgroundColor = styleInterval(brks, clrs))
                         #backgroundSize = '98% 88%',
                         #background = styleColorBar(range(flow,na.rm=T), 'lightblue'),
                         #backgroundRepeat = 'no-repeat',
                         #backgroundPosition = 'center')
  }}

