library(igraph)
library(DiagrammeR)
library(visNetwork)
library(tidyverse)
library(tidygraph)
library(scales)



# utilities-----------------------------------------------------------------------------

notify <- message # alias


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


# graph utilities ---------------------------------------------------------


fix_col <- function(graf,col,tab="factors",default=NA){
  if(tab=="factors") {if (!(col %in% (graf %>% factor_table %>% colnames))) graf %>% activate(nodes) %>% mutate("{col}":=default) else graf} else
  {if (!(col %in% (graf %>% link_table %>% colnames))) graf %>% activate(edges) %>% mutate("{col}":=default) else graf}
}


factor_colnames <- function(graf)graf %>% factor_table %>% colnames
link_colnames <- function(graf)graf %>% link_table %>% colnames

print_more <- function(graf,n=99){
  graf %>% activate(nodes) %>% as_tibble %>% print(n=n)
  graf %>% activate(edges) %>% as_tibble %>% print(n=n)
}

load_graf_from_rds <- function(name){
  tmp <- readRDS(name)
  tbl_graph(tmp$factors,tmp$links)
}



#' Extracting tibbles from a tidygraph
#'
#' @param graf A tidygraph
#' @return A tibble.
#' @name tibbles
NULL
#> NULL

#' @rdname tibbles
#' @export
factor_table <- function(graf)graf %>%
  activate(nodes) %>% as_tibble

#' @rdname tibbles
#' @export
link_table <- function(graf)graf %>%
  activate(edges) %>% as_tibble



#' Parse commands
#'
#' A parser which breaks a text input into individual commands.
#' There is one command format corresponding to each of
#' the family of pipe functions.
#'
#' @param graf A tidygraph
#' @param tex A set of commands to parse, separated by linebreaks if there is more than one command.
#' Each line consists of two words corresponding to the name of the pipe function to be applied, e.g. `color links` calls the function `color_links`
#' The function name is followed by field=value pairs corresponding to the arguments of the function such as `top=10`.
#'
#' @return A tidygraph, the result of successively applying the commands to the input graph.
#' @export
#'@examples
#'\dontrun{
#'cashTransferMap %>% parse_commands("select factors top=10 \n color factors field=n") %>% make_vn()
#'}
parse_commands <- function(graf,tex){
  tex <- tex %>% replace_null("") %>% str_split("\n") %>% `[[`(1) %>% str_trim() %>% escapeRegex
  if(length(tex)>1)tex <- tex %>% keep(.!="")
  if(tex[[1]]=="") graf <- graf else {

    for(line in tex){
      if(str_trim(line)=="")return()
      fun <- word(line, 1,2, sep=" ")

      body <-
        str_remove(line,fun) %>%
        str_replace_all(" *=","=") %>%
        str_trim

      vals <-
        body %>%
        str_split("[^ ]*=") %>%
        `[[`(1) %>%
        keep(.!="") %>%
        str_trim %>%
        as.list

      fields <-
        body %>%
        str_extract_all("[^ ]*=") %>%
        `[[`(1) %>%
        str_trim %>%
        str_remove_all("=$")

      names(vals) <- fields
      vals$graf=graf

      fun <- fun %>% str_replace(" ","_") %>% paste0("pipe_",.)
# browser()
      graf <- possibly(~do.call(fun,vals),otherwise=graf)()

    }
  }
  graf
}


# helper graph functions ----------------------------------------------------
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
filter_things <- function(graf,field,value,operator="=",what){
  # browser()
  if(what=="links") graf <- graf %>% activate(edges) else graf <- graf %>% activate(nodes)

  if(operator=="=") graf %>%
      filter(UQ(sym(field)) %in% value) %>% activate(nodes)
  else if(operator=="contains") {
    value <- str_replace_all(value," OR ","|") %>% str_trim
    graf %>%
      filter(str_detect(tolower(UQ(sym(field))),tolower(escapeRegex(value)))) %>% activate(nodes)
  }

}

div_pal_n <- function(vec,lo=lo,hi=hi,mid=mid){
  div_gradient_pal(low=lo,high=hi,mid=mid)(rescale(vec)) %>% alpha(.6)
}
viridis_pal_n <- function(vec){
  vec <- vec %>% as.factor %>% as.numeric
  viridis_pal()(length(unique(vec)))[vec] %>% alpha(.6)
}
create_colors <- function(vec,lo=lo,hi=hi,mid=mid){
  if(class(vec)=="character") viridis_pal_n(vec) else div_pal_n(vec,lo=lo,hi=hi,mid=mid)
}

# main graph functions ----------------------------------------------------



pipe_filter_factors <- function(graf,field,value,operator="="){filter_things(graf=graf,field=field,value=value,operator=operator,what="factors")}
pipe_filter_links <- function(graf,field,value,operator="="){filter_things(graf=graf,field=field,value=value,operator=operator,what="links")}


pipe_select_links <- function(graf,top,all=F,is_proportion=F){
  gr <- graf %>%
    activate(edges) %>%
    group_by(from,to) %>%
    mutate(n=n()) %>%
    mutate(n=rank(n)) %>%
    # mutate(rtop=row_number()) %>%
    ungroup

  if(is_proportion) gr <- gr %>%
      filter(n/max(.E()$n,na.rm=T)>top) else gr <- gr %>%
          filter(n>top)

      gr %>%
        select(from,to,n,everything()) %>%
        activate(nodes)
}


pipe_select_factors <- function(graf,top=20,all=F,is_proportion=F){
  gr <- graf %>%
    activate(nodes) %>%
    mutate(n = centrality_degree()) %>%
    mutate(n=rank(n)) %>%
    arrange(desc(n))

  if(is_proportion) gr <- gr %>%
      filter(n/max(.N()$n,na.rm=T)>top) else gr <- gr %>%
          slice(1:top)

      gr
}


pipe_hide_factors <- function(graf,text){
  text <- str_replace_all(text," OR ","|")
  graf %N>% filter(str_detect(label,text,negate=T))
}


pipe_zoom_factors <- function(graf,level,char,hide){
  # browser()
  level=as.numeric(level)
  hide=as.logical(hide)
  if(level<1) return(graf)
  gr <- graf %>%
    activate(nodes) %>%
    filter(!hide | str_detect(label,char)) %>%
    mutate(label=if_else(str_detect(label,char),zoom_inner(label,level,char),label)) %>%
    convert(to_contracted,label,simplify=F)  %>%
    mutate(zoomed_=str_detect(label,char))

  tbl_graph(gr %>% factor_table %>% as.data.frame %>% select(label=1,zoomed_),gr %>% link_table  %>% as.data.frame)

}

pipe_bundle_factors <- function(graf,text=""){
  graf <- graf %>% activate(nodes)
  if(text=="") gr <- graf %>%
      mutate(label=str_match(label,"^[^ ]*")) %>%
      convert(to_contracted,label,simplify=F) %>%
      mutate(shrunk_=str_detect(label,"^[^ ]*"))

  else gr <- graf %>%
      mutate(label=if_else(str_detect(label,text),str_match(label,paste0(text)),label)) %>%
      convert(to_contracted,label,simplify=F)  %>%
      mutate(shrunk_=str_detect(label,text))
  # browser()
  tbl_graph(gr %>% factor_table %>% as.data.frame %>% select(label=1,shrunk_) ,gr %>% link_table  %>% as.data.frame)%>% pipe_fix_columns

  # i d on'tunderstand this convert / morph stuff and can't get a normal graph back

}

pipe_trace_paths <- function(graf,from,to,length){
  if(is.na(length)) {notify("You have to specify length");return(graf)}
  if(from=="") {notify("You have to specify source factors");return(graf)}
  if(to=="") {notify("You have to specify target factors");return(graf)}
  graf <- graf %>% activate(nodes)
  from <- str_replace_all(from," OR ","|") %>% str_trim
  to <- str_replace_all(to," OR ","|") %>% str_trim
  if(from=="" & to =="") return(graf)
  graf <- graf %>%
    mutate(found_from=str_detect(tolower(label),tolower(from))) %>%
    mutate(found_to=str_detect(tolower(label),tolower(to))) %>%
    mutate(found_any=found_from|found_to)

  # browser()

  if(!any(graf %>% factor_table %>% pull(found_any))) return(graf %>% filter(F))

  tracedownvec <- graf %>% distances(to=graf %>% factor_table %>% pull(found_from),mode="in") %>% apply(1,min)
  traceupvec <- graf %>% distances(to=graf %>% factor_table %>% pull(found_to),mode="out") %>% apply(1,min)

  # here we need to intervene to make sure that influences don't move closer to the source, as this is a kind of loop

  bothvecsum <- `+`(tracedownvec,traceupvec)
  bothvec <- bothvecsum<=length
  if(min(bothvecsum)<Inf) graf <- graf %>% mutate(traceupvec=traceupvec,
                                                  tracedownvec=tracedownvec,
                                                  bothvec,
                                                  found=found_from|found_to
  ) %>% filter(bothvec) else graf %>% filter(F)
  # browser()

  # graf <- graf %E>% mutate(backfrom=.N()$tracedownvec[from],backto=.N()$tracedownvec[to]) %>%
  #   mutate(is_backwards=backto<=backfrom)
  #
  # backvec=(graf %>% pull(is_backwards))
  # if(any(backvec))notify(glue("removing {sum(backvec)}edges which are not forwards") )
  #
  # graf <- graf %E>% filter(!is_backwards)

  sums <- graf %>% factor_table %>% select(found_from,found_to) %>% colSums(na.rm=T)
  if((sums[1]*sums[2])>10000){
    # if(sum(found_from,na.rm=T)*sum(found_to,na.rm=T)>10){
    notify("too much to trace")
    return(graf)
  }
  graf <- graf %N>% pipe_bundle_links() %E>%
    mutate(n=if_else(is.na(n),1L,as.integer(n))) %>%
    activate(nodes)
  from_vec <- factor_table(graf) %>% filter(found_from) %>% pull(label)
  to_vec <- factor_table(graf) %>% filter(found_to) %>% pull(label)
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
  source <- V(graf)[(graf %>% factor_table)$label=="_super_source_"]
  sink <- V(graf)[(graf %>% factor_table)$label=="_super_sink_"]
  # browser()
  res <- graf %N>%
    max_flow(source=source, target=sink)

  notify(glue("Flow is {res$value}"))
  notify(glue("Number of cuts is {res$cut %>% length}"))
  graf <- graf %>%
    # mutate(flow_from=node_max_flow_from(ID(.,"_super_source_")) %>% replace_na(0)) %>%
    # mutate(flow_to=node_max_flow_to(ID(.,"_super_sink_")) %>% replace_na(0)) %>%
    # activate(edges) %>% mutate(flow=res$flow %>% replace_na(0) %>% replace_inf(0)) %>%
    activate(nodes) %>%
    filter(label!="_super_sink_" & label!="_super_source_")

  attr(graf,"flow")=res$value
  graf



}






pipe_find_factors <- function(graf,text,up=0,down=0,tolower=T){
  graf <- graf %>% activate(nodes)
  text <- str_replace_all(text," OR ","|") %>% str_trim
  if(tolower)graf <- graf %>%
    mutate(found=str_detect(tolower(label),tolower(text)))
  else graf <- graf %>%
    mutate(found=str_detect((label),(text)))

  if(!any(graf %>% factor_table %>% pull(found))) return(graf %>% filter(F))

  downvec <- graf %>% distances(to=graf %>% factor_table %>% pull(found),mode="in") %>% apply(1,min) %>% `<=`(down)
  upvec <- graf %>% distances(to=graf %>% factor_table %>% pull(found),mode="out") %>% apply(1,min) %>% `<=`(up)
  if(any(upvec)|any(downvec))graf %>% mutate(upvec=upvec,downvec=downvec) %>% filter(found|upvec|downvec) else graf %>% filter(F)
}

pipe_remove_orphans <- function(graf){
  graf %>%
    activate(nodes) %>%
    filter(!node_is_isolated())
}



pipe_flip_opposites <- function(graf,flipchar="~"){
  graf %N>%
    mutate(
      is_flipped=str_detect(label,paste0("^ *",flipchar)),
      label=if_else(is_flipped,flip_vector(label),label)
    ) %>%
    activate(edges) %>%
    mutate(from_flipped=.N()$is_flipped[from]) %>%
    mutate(to_flipped=.N()$is_flipped[to]) %>%
    activate(nodes)
}



# zero_to_one <- function(vec)(vec-min(vec,na.rm=T))/(max(vec,na.rm=T)-min(vec,na.rm=T))

pipe_bundle_links <- function(graf,field=NULL){
  # browser()
  nodes <- factor_table(graf)
  if(nrow(nodes)==0) return(NULL)
  if(!is.null(field)){
    if(field %notin% link_colnames(graf)) return(graf)
  edges <- graf %>% activate(edges) %>%
    as_tibble %>%
    group_by(from,to,UQ(sym(field))) %>%
    mutate(rn=row_number())

  } else
  edges <- graf %>% activate(edges) %>%
    as_tibble %>%
    group_by(from,to) %>%
    mutate(rn=row_number())

  if("n" %in% link_colnames(graf))  {edges <- edges %>%
    mutate(n=sum(n,na.rm=T)) %>%
    filter(rn==1)
  } else {edges <- edges %>%
    mutate(n=n()) %>%
    filter(rn==1)
}

  tbl_graph(nodes,edges %>% ungroup)

}

## internal pipes -------------------------------------------------------------
pipe_fix_columns <- function(graf){

  # if(!("color" %in% factor_colnames(graf))) graf <- graf %N>% mutate(color="#222222")
  if(!("color.background" %in% factor_colnames(graf))) graf <- graf %N>% mutate(color.background="#aaaaee77")
  if(!("color.border" %in% factor_colnames(graf))) graf <- graf %N>% mutate(color.border="#222222")
  if(!("n" %in% factor_colnames(graf))) graf <- graf %N>% mutate(n=1L)
  if(!("size" %in% factor_colnames(graf))) graf <- graf %N>% mutate(size=1L)
  if(!("found" %in% factor_colnames(graf))) graf <- graf %N>% mutate(found=1L)
  if(!("color" %in% link_colnames(graf))) graf <- graf %E>% mutate(color="#222222")
  if(!("n" %in% link_colnames(graf))) graf <- graf %E>% mutate(n=1L)
  if(!("capacity" %in% link_colnames(graf))) graf <- graf %E>% mutate(capacity=1L)
  if(!("label" %in% link_colnames(graf))) graf <- graf %E>% mutate(label="")
  if(!("width" %in% link_colnames(graf))) graf <- graf %E>% mutate(width=.1)
  if(!("flow" %in% link_colnames(graf))) graf <- graf %E>% mutate(flow=1L)
  graf %>% activate(nodes)
}
pipe_metrics <- function(graf){
  # browser()
  if(is.null(graf)){notify("No graph for metrics");return(graf)}

  graf  %N>%
    mutate(
      group=group_infomap(),
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
#' @param graf a tidygraph
#' @param field A numerical field in the factor table which will control the scale.
#'
#' @return A tidygraph with a new or overwritten column `size`in the factor table varying between .2 and 1.
#' @export
#'
#' @examples
pipe_scale_factors <- function(graf,field="n"){
  graf <- pipe_metrics(graf)
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  class <- graf %>% factor_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  # browser()
  graf %N>% mutate(size=scales::rescale(UQ(sym(field)),to=c(0.2,1))) %>% activate(nodes)

}

#' Label factors
#'
#' @param graf A tidygraph
#' @param field A numerical or character field in the factor table.
#' @param clear Logical. Whether to clear any existing labels or to concatenate the new result after
#' any existing labels. Default is `FALSE`.
#'
#' @return A tidygraph with a column `label`. If `clear` is FALSE, the new label is concatenated
#' after any existing label. The new label is of the form `field: value`.
#' @export
#'
#' @examples
pipe_label_factors <- function(graf,field="n",clear=F){
  clear=as.logical(clear)
  graf <- pipe_metrics(graf)
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
  graf %N>%
    mutate(label=paste0((if(clear)NULL else paste0(label,". ")) %>% keep(.!=""),field,": ",UQ(sym(field)),". ")) %>% activate(nodes)
}


#' Color factors (background color)
#'
#' @param graf A tidygraph
#' @param field A numerical or character field in the factor table.
#' If it is character, the other parameters are ignored and a color-blind friendly qualitative palette is applied.
#' @param fixed  Optionally, a color specification which will be applied everywhere and overrides `field`.
#' @param lo Optionally, a color specification for the low end of the color range. Default is `green`.
#' @param hi Optionally, a color specification for the high end of the color range. Default is `blue`.
#' @param mid  Optionally, a color specification for the middle of the color range. Default is `gray`.
#'
#' @return A tidygraph with a new or overwritten column `color.background`in the factor table.
#' @export
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
#' @param graf A tidygraph
#' @param field A numerical or character field in the factor table.
#' #' If it is character, the other parameters are ignored and a color-blind friendly qualitative palette is applied.
#' @param fixed  Optionally, a color specification which will be applied everywhere and overrides `field`.
#' @param lo Optionally, a color specification for the low end of the color range. Default is `green`.
#' @param hi Optionally, a color specification for the high end of the color range. Default is `blue`.
#' @param mid  Optionally, a color specification for the middle of the color range. Default is `gray`.
#'
#' @return A tidygraph with a new or overwritten column `color.border`in the factor table.
#' @export
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
#' @param graf A tidygraph
#' @param field A numerical or character field in the link table.
#' If it is character, the other parameters are ignored and a color-blind friendly qualitative palette is applied.
#' @param fixed  Optionally, a color specification which will be applied everywhere and overrides `field`.
#' @param lo Optionally, a color specification for the low end of the color range. Default is `green`.
#' @param hi Optionally, a color specification for the high end of the color range. Default is `blue`.
#' @param mid  Optionally, a color specification for the middle of the color range. Default is `gray`.
#'
#' @return A tidygraph with a new or overwritten column `color`in the link table.
#' @export
#'
#' @examples
pipe_color_links <- function(graf,field="n",lo="green",hi="blue",mid="gray",fixed=NULL){
  if(!is.null(fixed))return(graf %E>% mutate(color=fixed) %>% activate(nodes))
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
  graf %E>% mutate(color=create_colors(UQ(sym(field)),lo=lo,hi=hi,mid=mid)) %>% activate(nodes)


}
#' Fade factors
#'
#' @param graf A tidygraph
#' @param field A numerical field in the link table which will control the amount of fading
#' (the alpha value of the factors).
#' @return A tidygraph in which the column `color.background`in the factor table has alpha proportionate to the values in `field`.
#' @export
#'
#' @examples
pipe_fade_factors <- function(graf,field="n"){
  if(field %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  if("color.background" %notin% factor_colnames(graf)){warning("No such column");return(graf)}
  class <- graf %>% factor_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  # browser()
  graf %N>% mutate(color.background=alpha(color.background,scales::rescale(UQ(sym(field)),to=c(0.2,1)))) %>% activate(nodes)
}

#' Fade links
#'
#' @param graf A tidygraph
#' @param field A numerical field in the link table which will control the amount of fading
#' (the alpha value of the links).
#' @return A tidygraph in which the column `color`in the link table has alpha proportionate to the values in `field`.
#' @export
#'
#' @examples
pipe_fade_links <- function(graf,field="n"){
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
  if("color" %notin% link_colnames(graf)){warning("No such column");return(graf)}
  class <- graf %>% link_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  # browser()
  graf %E>% mutate(color=alpha(color,scales::rescale(UQ(sym(field)),to=c(0.2,1)))) %>% activate(nodes)
}

#' Scale factors
#'
#' @param graf A tidygraph
#' @param field A numerical field in the link table which will control the scale (the width of the links).
#'
#' @return A tidygraph with a new or overwritten column `width`in the link table varying between .2 and 1.
#' @export
#'
#' @examples
pipe_scale_links <- function(graf,field="n"){
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
  class <- graf %>% link_table %>% pull(UQ(sym(field))) %>% class
  if(class =="character"){warning("No such column");return(graf)}
  # browser()
  graf %E>% mutate(width=scales::rescale(UQ(sym(field)),to=c(0.2,1))) %>% activate(nodes)
}

#' Label links
#'
#' @param graf A tidygraph
#' @param field A numerical or character field in the link table.
#' @param clear Logical. Whether to clear any existing labels or to concatenate the new result after
#' any existing labels.
#'
#' @return A tidygraph with a column `label`. If `clear` is FALSE (the default), the new label is concatenated
#' after any existing label. The new label is of the form `field: value`.
#' @export
#'
#' @examples
pipe_label_links <- function(graf,field="n",clear=F){
  clear=as.logical(clear)
  if(field %notin% link_colnames(graf)){warning("No such column");return(graf)}
  graf %E>%
    mutate(label=paste0((if(clear)NULL else paste0(label,". ")) %>% keep(.!=""),field,": ",UQ(sym(field)),". ")) %>% activate(nodes)
}


# outputs -----------------------------------------------------------------


make_graph_metrics <- function(graf){
  metric=c(
    igraph::edge_connectivity(graf),
    igraph::clique_num(graf),
    igraph::count_max_cliques(graf),
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
    "clique_num",
    "clique_count",
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
    "The size of the largest clique.",
    "The number of maximal cliques in the graph.",
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


make_vn <- function(graf,scale=1){
  graf <- graf %>% pipe_fix_columns()
  nodes <- graf %N>% as_tibble %>% mutate(value=size*10)
  # browser()
  edges <- graf %E>% as_tibble %>%
    vn_fan_edges() %>% mutate(width=width*10)
  if(nrow(nodes)>1){
    layout <- layout_with_sugiyama(graf)$layout*-scale
    colnames(layout) <- c("y", "x")
    nodes <- data.frame(nodes, layout)
  }
  nodes <- nodes %>%   mutate(id=row_number())
  visNetwork(nodes,edges,background="white")   %>%
    visNodes(
      shadow = list(enabled = F, size = 10),
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
  # %>%
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
    ungroup()

}





## grviz -------------------------------------------------------------------



make_grviz <- function(
  graf=NULL,
  maxwidth=NULL,
  grv_cluster=F,
  grv_cluster_select=NULL,
  grv_layout="dot",
  grv_splines="splines",
  grv_overlap=F,
  color="grey",
  ranksep_slider=3,
  nodesep_slider=20,
  wrap_slider=12,
  safe_limit=T

){
  # graf b
  # browser()
  if(is.null(graf))return()
  graf <- graf %>% pipe_fix_columns()

  if(safe_limit & nrow(link_table(graf))>200){
    notify("Map is too large for print view; consolidating.",3)
    graf <- graf %>%
      pipe_bundle_links() %>%
      pipe_label_links("n")

    if(nrow(factor_table(graf))>200) graf <- graf %>% pipe_select_factors(20)
  }
  if("id" %in% colnames(factor_table(graf)))graf <-  graf %>% select(-id)
  grv <-  graf %>%
    pipe_fix_columns %>%
    activate(nodes) %>%
    # slice(1:68) %>%
    mutate(label=clean_grv(label) %>% str_wrap(20))%>%
    mutate(fillcolor=color.background) %>%
    mutate(color=color.border) %>%
    mutate(fontsize=(size+5)*100) %>%
    # mutate(color="black") %>%
    mutate(fontcolor="black") %>%
    activate(edges) %>%
    mutate(label=if_else(label=="",".",label))%>%
    mutate(penwidth=width*2)%>%
    mutate(label=clean_grv(label) %>% str_wrap(20))%>%
    # select(from,to,label)  %>%
    # group_by(from,to) %>%
    # mutate(rn=row_number()) %>%
    # filter(rn==1) %>%
    # ungroup %>%
    mutate(arrowsize=4) %>%
    mutate(arrowhead="normal") %>%
    DiagrammeR::from_igraph() %>%
    DiagrammeR::add_global_graph_attrs("layout", grv_layout, "graph") %>%
    DiagrammeR::add_global_graph_attrs("splines", grv_splines, "graph") %>%
    DiagrammeR::add_global_graph_attrs("overlap", grv_overlap, "graph") %>%
    # add_global_graph_attrs("nslimit1", "1", "graph") %>%
    # add_global_graph_attrs("nslimit",  "1", "graph") %>%
    # add_global_graph_attrs("overlap", F, "graph") %>%
    DiagrammeR::add_global_graph_attrs("labelloc", "bottom","graph") %>%
    DiagrammeR::add_global_graph_attrs("tooltip", " ", "graph") %>%
    DiagrammeR::add_global_graph_attrs("rankdir", "LR", "graph") %>%
    DiagrammeR::add_global_graph_attrs("fontsize", "28", "graph") %>%
    DiagrammeR::add_global_graph_attrs("fontname", "Arial", "graph") %>%
    DiagrammeR::add_global_graph_attrs("nodesep", 1, "graph") %>%
    DiagrammeR::add_global_graph_attrs("ranksep", 3, "graph") %>%

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
    DiagrammeR::add_global_graph_attrs("fontsize", "63", "edge") %>%
    # add_global_graph_attrs("color", "gray", "edge") %>%
    # DiagrammeR::add_global_graph_attrs("penwidth", "10", "edge") %>%
    DiagrammeR::add_global_graph_attrs("fontcolor", "#666666", "edge")

  return(grv %>% DiagrammeR::render_graph())

}

clean_grv <- function(tx){
  tx %>% str_replace_all("'","&rsquo;") %>%
    str_replace_all("\"","&rsquo;") %>%
    strip_symbols() %>%
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
