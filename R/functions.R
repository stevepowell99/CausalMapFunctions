# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#library("tidygraph")
#library("igraph")
#library("tidyverse")
#library("glue")
#library("chroma")
#library("visNetwork")


# utilities-----------------------------------------------------------------------------

notify <- message # alias
hello <- function() {
  print("Hello, world!")
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


link_table <- function(graf)graf %>%
  activate(edges) %>% as_tibble
factor_table <- function(graf)graf %>%
  activate(nodes) %>% as_tibble


# main graph functions ----------------------------------------------------


filter_links <- function(graf,field,value,all=F){
  graf %>% activate(edges) %>% filter(UQ(sym(field)) %in% value) %>% activate(edges)
}

filter_factors <- function(graf,field,value,all=F){
  graf %>% activate(nodes) %>% filter(UQ(sym(field)) %in% value)
}


filter_n_links <- function(graf,n_,all=F,is_proportion=F){
  gr <- graf %>%
    activate(edges) %>%
    group_by(from,to) %>%
    mutate(n=n()) %>%
    mutate(n=rank(n)) %>%
    # mutate(rn_=row_number()) %>%
    ungroup

  if(is_proportion) gr <- gr %>%
      filter(n/max(.E()$n,na.rm=T)>n_) else gr <- gr %>%
          filter(n>n_)

      gr %>%
        select(from,to,n,everything()) %>%
        activate(nodes)
}
filter_n_factorsOLD <- function(graf,n_,all=F,is_proportion=F){
  gr <- graf %>%
    activate(nodes) %>%
    mutate(n = centrality_degree()) %>%
    mutate(n=rank(n))

  if(is_proportion) gr <- gr %>%
      filter(n/max(.N()$n,na.rm=T)>n_) else gr <- gr %>%
          filter(n>n_)

      gr
}
filter_n_factors <- function(graf,n_,all=F,is_proportion=F){
  gr <- graf %>%
    activate(nodes) %>%
    mutate(n = centrality_degree()) %>%
    mutate(n=rank(n)) %>%
    arrange(desc(n))

  if(is_proportion) gr <- gr %>%
      filter(n/max(.N()$n,na.rm=T)>n_) else gr <- gr %>%
          slice(1:n_)

      gr
}


hide_factors <- function(graf,source_string){
  source_string <- str_replace_all(source_string," OR ","|")
  graf %N>% filter(str_detect(label,source_string,negate=T))
}


zoom_inner <- function(string,n,char){
  string %>% map(~str_split(.,char) %>% `[[`(1) %>% `[`(1:n) %>% keep(!is.na(.)) %>% paste0(collapse=char)) %>% unlist
}
zoom <- function(graf,n_,char,hide){
  # browser()
  n_=as.numeric(n_)
  hide=as.logical(hide)
  if(n_<1) return(graf)
  gr <- graf %>%
    activate(nodes) %>%
    filter(!hide | str_detect(label,char)) %>%
    mutate(label=if_else(str_detect(label,char),zoom_inner(label,n_,char),label)) %>%
    convert(to_contracted,label,simplify=F)  %>%
    mutate(zoomed_=str_detect(label,char))

  tbl_graph(gr %>% factor_table %>% as.data.frame %>% select(label=1,zoomed_),gr %>% link_table  %>% as.data.frame)

}


trace_factors <- function(graf,from,to,length){
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
  graf <- graf %N>% add_collapse_edges() %E>%
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
    mutate(capacity=pmax(n,capacity,na.rm=T)) %>% activate(nodes)
  source <- V(graf)[(graf %>% factor_table)$label=="_super_source_"]
  sink <- V(graf)[(graf %>% factor_table)$label=="_super_sink_"]
  res <- graf %>% max_flow(source=source, target=sink)

  # browser()
  notify(glue("Flow is {res$value}"))
  graf %>%
    # mutate(flow_from=node_max_flow_from(ID(.,"_super_source_")) %>% replace_na(0)) %>%
    # mutate(flow_to=node_max_flow_to(ID(.,"_super_sink_")) %>% replace_na(0)) %>%
    # activate(edges) %>% mutate(flow=res$flow %>% replace_na(0) %>% replace_inf(0)) %>%
    activate(nodes) %>%
    filter(label!="_super_sink_" & label!="_super_source_")



}






find_string <- function(graf,source_string,up=0,down=0,tolower=T){
  graf <- graf %>% activate(nodes)
  source_string <- str_replace_all(source_string," OR ","|") %>% str_trim
  if(tolower)graf <- graf %>%
    mutate(found=str_detect(tolower(label),tolower(source_string)))
  else graf <- graf %>%
    mutate(found=str_detect((label),(source_string)))

  if(!any(graf %>% factor_table %>% pull(found))) return(graf %>% filter(F))

  downvec <- graf %>% distances(to=graf %>% factor_table %>% pull(found),mode="in") %>% apply(1,min) %>% `<=`(down)
  upvec <- graf %>% distances(to=graf %>% factor_table %>% pull(found),mode="out") %>% apply(1,min) %>% `<=`(up)
  if(any(upvec)|any(downvec))graf %>% mutate(upvec=upvec,downvec=downvec) %>% filter(found|upvec|downvec) else graf %>% filter(F)
}

remove_isolated <- function(graf){
  graf %>%
    activate(nodes) %>%
    filter(!node_is_isolated())
}

shrink <- function(graf=sess$graf,what){
  graf <- graf %>% activate(nodes)
  if(what=="") gr <- graf %>%
      mutate(label=str_match(label,"^[^ ]*")) %>%
      convert(to_contracted,label,simplify=F) %>%
      mutate(shrunk_=str_detect(label,"^[^ ]*"))

  else gr <- graf %>%
      mutate(label=if_else(str_detect(label,what),str_match(label,paste0(what)),label)) %>%
      convert(to_contracted,label,simplify=F)  %>%
      mutate(shrunk_=str_detect(label,what))
  # browser()
  tbl_graph(gr %>% factor_table %>% as.data.frame %>% select(label=1,shrunk_) ,gr %>% link_table  %>% as.data.frame)%>% add_columns

  # i d on'tunderstand this convert / morph stuff and can't get a normal graph back

}

# graf %>%
#   filter_n_links(4) %>%
#   link_table
# graf %>%
#   filter_n_links(2) %>%
#   remove_not_shown %>%
#   arrange()%>%
#   mutate(label=row_number()) %>%
#   make_vn







parse_commands <- function(graf,tex){
  tex <- tex %>% replace_null("") %>% str_split("\n") %>% `[[`(1) %>% str_trim() %>% escapeRegex
  if(length(tex)>1)tex <- tex %>% keep(.!="")
  if(tex[[1]]=="") graf <- graf else {

    for(line in tex){
      if(str_trim(line)=="")return()

      pal <- function(x)interp_map(x,colors=c("#ddddff","#0000ff"))

      if(str_detect(line,"^find")) {
        up <- str_match(line," up *([0-9]*)")[,2] %>% str_trim %>% replace_na(0) %>% as.numeric
        down <- str_match(line," down *([0-9]*)")[,2] %>% str_trim %>% replace_na(0) %>% as.numeric
        one <- str_remove(line,"up *[0-9]*.*$") %>% str_remove("down *[0-9]*.*$") %>% str_remove("^ *find *") %>% str_trim
        # len <- str_match(line," up ([0-9]*)")[,2] %>% replace_na(1) %>% as.numeric
        graf <- graf %>% find_string(one,up,down)
      } else
        if(str_detect(line,"^trace")) {
          length <- str_match(line,"trace *([0-9]*)")[,2] %>% str_trim %>% replace_na(0) %>% as.numeric
          to <- str_match(line," to (.*)")[,2] %>% str_trim %>% replace_na("")
          from <- (str_remove(line," to .*$") %>% str_match("from *(.*)"))[,2] %>% str_trim %>% replace_na("")
          graf <- graf %>% trace_factors(from,to,length)
        } else
          if(str_detect(line,"^filter links") & str_detect(line,"%")) {
            one <- str_match(line,"^filter links ([0-9]*)")[,2]%>% replace_na(0) %>% as.numeric %>% `/`(100)
            graf <- graf %>% filter_n_links(one,is_proportion=T)
          } else
            if(str_detect(line,"^filter links ")) {
              one <- str_match(line,"^filter links ([^ ]*)")[,2]%>% replace_na(0) %>% as.numeric
              graf <- graf %>% filter_n_links(one)
            } else
              if(str_detect(line,"^filter factors") & str_detect(line,"%")) {
                one <- str_match(line,"^filter factors ([0-9]*)")[,2]%>% replace_na(0) %>% as.numeric %>% `/`(100)
                graf <- graf %>% filter_n_factors(one,is_proportion=T)
              } else
                if(str_detect(line,"^filter factors ")) {
                  one <- str_match(line,"^filter factors ([^ ]*)")[,2]%>% replace_na(0) %>% as.numeric
                  graf <- graf %>% filter_n_factors(one)
                } else
                  if(str_detect(line,"^remove")) graf <- graf %>% remove_isolated else
                    if(str_detect(line,"^shrink")) {
                      what <- str_match(line,"^shrink (.*)")[,2]%>% replace_na("")
                      graf <- graf %>% shrink(what)
                    } else
                      if(str_detect(line,"^hide")) {
                        one <- str_match(line,"^hide (.*)")[,2]%>% replace_na("")
                        graf <- graf %>% hide_factors(one)
                      } else
                        if(str_detect(line,"^zoom")) {
                          one <- str_match(line,"^zoom ([0-9]*)")[,2]%>% replace_na(0)
                          char <- str_match(line,"^zoom [0-9]* (.*)")[,2] %>% replace_na(";") %>% str_remove(" *hide$")
                          hide <- str_detect(line,"hide *$") %>% replace_na(F)
                          graf <- graf %>% zoom(one,char,hide)
                      } else if(str_detect(line,"^bundle")) {
                          one <- str_match(line,"^bundle (.*)")[,2]%>% replace_na(NULL)
                          graf <- graf %>% add_collapse_edges(one)
                        } else

      # browser()
      if(str_detect(line,"^ *factors")) {
          graf <- graf %>% add_metrics()### IS THIS THE BEST WAY??



        hit <- str_match(line,"^ *factors *(.*)")[,2] %>% str_trim %>% replace_na("")

        if(str_detect(hit,"^ *colou?rborder")) {
          val <- str_match(hit," *colou?rborder *(.*)")[,2] %>% str_trim %>% replace_na("")
          # if(val %in% factor_colnames(graf)){
          if(val %in% factor_colnames(graf))graf <-
              graf %N>% mutate(color.border=pal(UQ(sym(val))) %>% str_sub(1,7) %>% paste0("88"))

          # }
        } else
          if(str_detect(hit,"^ *label")) {
            val <- str_match(hit," *label *(.*)")[,2] %>% str_trim %>% replace_na("")
            if(val %in% factor_colnames(graf))graf <-
                graf %N>% mutate(label=paste0(label %>% keep(.!=""),". ",val,": ",UQ(sym(val))))

            # }
          } else
            if(str_detect(hit,"^ *colou?r")) {
              val <- str_match(hit," *colou?r *(.*)")[,2] %>% str_trim %>% replace_na("")
              # if(val %in% factor_colnames(graf)){
              if(val %in% factor_colnames(graf))graf <-
                  graf %N>% mutate(color.background=pal(UQ(sym(val))) %>% str_sub(1,7) %>% paste0("88"))

              # }
            }
      }
      else
        if(str_detect(line,"^ *links")) {


          # graf <- graf %>% add_collapse_edges()### IS THIS THE BEST WAY??


          hit <- str_match(line,"^ *links *(.*)")[,2] %>% str_trim %>% replace_na("")

          if(str_detect(hit,"^ *label")) {
            val <- str_match(hit," *label *(.*)")[,2] %>% str_trim %>% replace_na("")
            # if(val %in% link_colnames(graf)){
            # browser()
            if(val %in% link_colnames(graf)) graf <-
                graf %E>% mutate(label=paste0(label %>% keep(.!=""),". ",val,": ",UQ(sym(val)))) %>% activate(nodes)

            # }
          } else
            if(str_detect(hit,"^ *colou?r")) {
              val <- str_match(hit," *colou?r *(.*)")[,2] %>% str_trim %>% replace_na("")
              # if(val %in% link_colnames(graf)){
              # browser()
              if(val %in% link_colnames(graf)) graf <-
                  graf %E>% mutate(color=pal(UQ(sym(val)) ) %>% str_sub(1,7) %>% paste0("88")) %>% activate(nodes)

              # }
            } else
              if(str_detect(hit,"^ *alpha")) {
                val <- str_match(hit," *alpha *(.*)")[,2] %>% str_trim %>% replace_na("")
                # if(val %in% link_colnames(graf)){
                if("color" %notin% link_colnames(graf)) graf <- graf %E>% mutate(color="blue") %>% activate(nodes)
                # browser()
                if(val %in% link_colnames(graf)) graf <-
                    graf %E>% mutate(color=alpha(color,(zero_to_one(UQ(sym(val))))*.8+.2)) %>% activate(nodes)

                # }
              }
        }
    }
  }
  graf
}
add_filters <- parse_commands #alias

add_flip_inner_component <- function(tex,flipchar="~"){
  if_else(str_detect(tex,paste0("^ *",flipchar)),str_remove(tex,paste0("^ *",flipchar)),paste0("~",tex))
}
add_flip_inner <- function(tex,flipchar="~",sepchar=";"){
  tex %>%
    str_split(sepchar) %>%
    `[[`(1) %>%
    add_flip_inner_component(flipchar=flipchar) %>%
    paste0(collapse=sepchar)
}
add_flip_vector <- function(tex,flipchar="~",sepchar=";"){
  lapply(tex,function(x)add_flip_inner(x,flipchar=flipchar,sepchar=sepchar)) %>%
    unlist(recursive=F)
}
add_flip <- function(graf,flipchar="~"){
  graf %N>%
    mutate(
      is_flipped=str_detect(label,paste0("^ *",flipchar)),
      label=if_else(is_flipped,add_flip_vector(label),label)
    ) %>%
    activate(edges) %>%
    mutate(from_flipped=.N()$is_flipped[from]) %>%
    mutate(to_flipped=.N()$is_flipped[to]) %>%
    activate(nodes)
}
add_columns <- function(graf){

  # if(!("color" %in% factor_colnames(graf))) graf <- graf %N>% mutate(color="#222222")
  if(!("color.background" %in% factor_colnames(graf))) graf <- graf %N>% mutate(color.background="#aaaaee77")
  if(!("color.border" %in% factor_colnames(graf))) graf <- graf %N>% mutate(color.border="#222222")
  if(!("n" %in% factor_colnames(graf))) graf <- graf %N>% mutate(n=1L)
  if(!("found" %in% factor_colnames(graf))) graf <- graf %N>% mutate(n=1L)
  if(!("color" %in% link_colnames(graf))) graf <- graf %E>% mutate(color="#222222")
  if(!("n" %in% link_colnames(graf))) graf <- graf %E>% mutate(n=1L)
  if(!("capacity" %in% link_colnames(graf))) graf <- graf %E>% mutate(capacity=1L)
  if(!("label" %in% link_colnames(graf))) graf <- graf %E>% mutate(label=NULL)
  if(!("flow" %in% link_colnames(graf))) graf <- graf %E>% mutate(flow=1L)
  graf
}
add_metrics <- function(graf){
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



zero_to_one <- function(vec)(vec-min(vec,na.rm=T))/(max(vec,na.rm=T)-min(vec,na.rm=T))

add_collapse_edges <- function(graf,col=NULL){
  nodes <- factor_table(graf)
  if(nrow(nodes)==0) return(NULL)
  if(!is.null(col)){
    if(col %notin% link_colnames(graf)) return(graf)
  edges <- graf %>% activate(edges) %>%
    as_tibble %>%
    group_by(from,to,UQ(sym(col))) %>%
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
make_vn <- function(graf,scale=1){
  nodes <- graf %>% as_tibble
  edges <- graf %E>% as_tibble
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
      # borderWidth=0,
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




