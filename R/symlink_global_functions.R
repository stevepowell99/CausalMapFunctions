## remember this is really in the shared functions folder

# ### NOOO this needs to be removed again
# config = configr::read.config("config.yml")$default
#
#
# conn <- DBI::dbConnect(
#   drv = RMySQL::MySQL(max.con=1000, fetch.default.rec=1000),
#   idleTimeout=900000,#15 minutes
#   dbname = config$sql_cm$dbname,
#   host = config$sql_cm$host,
#   port = config$sql_cm$port,
#   username = config$sql_cm$username,
#   password = config$sql_cm$password
# )
#


only_first <- function(vec){
  seq_along(vec) %>%
    map(function(x){
      if(x==1) vec[x] else if(vec[x]==vec[x-1]) "" else vec[x]
    }) %>%
    unlist
}

nwords <- function(vec){
  map(vec,~length(strsplit(., "\\w+")[[1]])) %>% unlist
}
## overwriting the tidyr one because it is so picky
replace_na <- function(vec,rep){
  # map(vec,~{if(is.na(.)) rep else .}) %>% unlist()
  vec[is.na(vec)] <- rep
  vec
}

replace_empty <- function(x,replacement=0){
  if(x=="") replacement else x
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
chain_to_links <- function(vec,statement_id){
  vec %>%
    seq_along %>%
    head(-1) %>%
    map(function(i){
      tibble(from=vec[i],to=vec[i+1],statement_id=statement_id)
    }) %>%
    bind_rows
}
chains_to_links <- function(lis,source){
  lis %>%
    map(chain_to_links,source) %>%
    bind_rows
}

keep_top_level <- function(vec)  vec %>% str_remove_all(";.*")
drop_top_level <- function(vec)  vec %>% map(~str_match(., ";.*") %>% replace_na(";") %>% str_remove("^;")) %>%
  unlist

collap <- function(vec,sep="\n"){
  vec %>% paste0(collapse=sep)
}
collapc <- function(vec){
  vec %>% collap(",")
}
uncollapc <- function(vec){
  vec %>% map(function(x){
    str_split(x,pattern=",") %>% pluck(1)

  })
}
remove_lines_starting_with <- function(text,char="#"){
  text %>%
    str_split("\n") %>%
    pluck(1) %>%
    keep(~!str_detect(.,paste0("^",char))) %>%
    collap
}


xc <- function(x, sep = " ") {
  str_split(x, sep)[[1]]
}

`%notin%` <- Negate(`%in%`)
pp <- function(obj,nn=99){
  print(obj,n=nn)
}
time_stamp <- function(){
  Sys.time() %>% str_replace_all(":","-")
}


notify <- function(tx,level=2,id=NULL,shiny=T){
  # if(level>2)showNotification(glue(tx),duration=0,closeButton = F,type=switch(level,"default","default","message","error"),id=id)
  if(shiny & level>2)showNotification(tx,duration=switch(level,4,4,3,5,5),closeButton = F,type=switch(level,"default","default","message","error"),id=id)
  message(tx)
}
collap <- function(vec,sep="\n\n"){
  vec %>% paste0(collapse=sep)
}

maxclean2 <- function(x){
  x %>%
    str_replace_all("[^[/][:alnum:]-]", "") %>%
    str_replace_all(" ", "") %>%
    stri_trans_general("latin-ascii") %>%
    tolower

}

maxclean3 <- function(x){
  x %>%
    str_replace_all("[^[/][:alnum:]- \n]", "") %>%
    tolower

}
maxclean_allow_slashes <- maxclean2


# general -----------------------------------------------------------------
row_index <- function(df)1:nrow(df)


to_logical <- function(vec){
  as.logical(vec)
}
is_edit <- function(){
  sess$is_owner | sess$is_admin
}
chunk2 <- function(x,n) {
  if(n==1) return(x)
  split(x, cut(seq_along(x), n, labels = FALSE))
  }

sum_safe <- function(vec,replacement=0){
  if(all(is.na(vec))) return(replacement)
  vec <- vec[!is.na(vec)]
  if(length(vec)==0)return(replacement)
  sum(vec)

}

max_safe <- function(vec,replacement=0){
  if(all(is.na(vec))) return(replacement)
  vec <- vec[!is.na(vec)]
  if(length(vec)==0)return(replacement)
  max(vec)

}


# text --------------------------------------------------------------------

replace_blank <- function(tex,replacement){
  if(is.null(tex))(replacement)else
    if(is.na(tex))(replacement)else
      if(""==(tex))(replacement)else
        tex
}
str_split1 <- function(vec){
  str_split(vec,"\\|") %>% unlist
}
strip_badwords <- function(vec){
  str_replace_all(vec,badwords %>% paste0(collapse="\\b|\\b") %>% paste0("\\b","\\b"),"****")
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


maxclean <- function(x){
  x %>%
    str_replace_all("[^[/][:alnum:]-]", "") %>%
    str_replace_all(" ", "") %>%
    stri_trans_general("latin-ascii")

}

# returns the string in a vector of strings which is most similar




str_replace_multiple <- function(str,links_search,links_replace){
  for(s in links_search){
    str <- str_replace_all(str,coll(s),links_replace)

  }
  str
}


# causal mapping basics ----------------------------------------------------------------------
len_un <-function(vec)length(unique(vec))
maxrn <-function(vec)max(vec,na.rm=T)
minrn <-function(vec)min(vec,na.rm=T)

make_factor_list <- function(links){

  main <-
    links %>%
    get_both_labels()

  tops <-
    main %>% keep_top_level() %>% unique

  c(tops,main) %>% unique
}
get_hashtags <- function(links){
  links$hashtags %>% replace_na("[]") %>% map(fromJSON) %>% unlist %>% unique %>% c("plain_coding")
}

get_both_labels <- function(link){c(link$from_label,link$to_label) %>% unique}

make_factors_from_links <- function(links){
  from_links <-
    links %>%
    select(from_label,source_id,statement_id) %>%
    group_by(from_label) %>%
    summarise(from_source_count=len_un(source_id),
              from_sources=list(source_id %>% unique),
              from_frequency=n()) %>%
    ungroup

  to_links <-
    links %>%
    select(to_label,source_id,statement_id) %>%
    group_by(to_label) %>%
    summarise(to_source_count=len_un(source_id),
              to_sources=list(source_id %>% unique),
              to_frequency=n()) %>%
    ungroup


  bind_rows(from_links %>% rename(label=from_label),to_links %>% rename(label=to_label)) %>%
    group_by(label) %>%
    summarise(source_count=c(unlist(from_sources),unlist(to_sources)) %>% len_un ,
              link_count=sum(from_frequency,to_frequency,na.rm=T),
              in_degree=sum(from_frequency,na.rm=T),
              out_degree=sum(to_frequency,na.rm=T)
    )
}

make_factors_from_transformed_links <- function(links){  # this is the transformed version - Note we filter for retained
  # we need just source and link counts, source list, and the originals
  #
  # browser()
  from_links <-
    links %>%
    # filter(retained) %>%
    select(retained,
           label=from_label,
           from_link_id=link_id,
           from_link_count=link_count,
           from_source_count=source_count,
           from_sources=sources
           # ,
           # original_from_link_count=original_link_count,
           # original_from_source_count=original_source_count,
           # original_from_sources=original_sources
           )

  to_links <-
    links %>%
    # filter(retained) %>%
    select(retained,
           label=to_label,
           to_link_id=link_id,
           to_link_count=link_count,
           to_source_count=source_count,
           to_sources=sources
           # ,
           # original_to_link_count=original_link_count,
           # original_to_source_count=original_source_count,
           # original_to_sources=original_sources
           )


bind_rows(from_links,to_links) %>%
  group_by(label,retained) %>%
  mutate(
            out_degree=sum(from_link_count,na.rm=T),
            in_degree=sum(to_link_count,na.rm=T),
            link_count=sum(from_link_count,to_link_count,na.rm=T),
            source_count=sum(from_source_count,to_source_count,na.rm=T),
            outcomeness=signif(100*from_link_count/link_count) %>% replace_na(0),
            original_out_degree=sum(from_link_count*retained,na.rm=T) %>% replace_na(0),
            original_in_degree=sum(to_link_count*retained,na.rm=T) %>% replace_na(0),
            original_link_count=sum(from_link_count*retained,to_link_count*retained,na.rm=T) %>% replace_na(0),
            original_source_count=sum(from_source_count*retained,to_source_count*retained,na.rm=T) %>% replace_na(0),
            original_outcomeness=signif(100*from_link_count*retained/link_count*retained) %>% replace_na(0)
            )%>%
  ungroup %>%
  filter(retained)
}

make_igraph_from_edgelist <- function(links){
  links %>% select(from_label,to_label) %>%
    filter(!is.na(from_label) & !is.na(to_label)) %>%
    as.matrix %>%
    igraph::graph_from_edgelist(directed = TRUE)
}



# db basics ----------------------------------------------------------------------

confirmButton <- function(id,label=NULL,inner_button_text="Confirm",icon="trash",alert_header="Please confirm",alert_text=""){
  dropdown(
    h1(alert_header),
    p(paste0("Are you sure you want to ",alert_text,"?")),
    actionButton(id,inner_button_text,class="pink"),
    p("This action may not be reversible. If you make a mistake, you will have to restore an earlier version from a backup.",class="confirmButton_warning"),
    tooltip=alert_text,
    label=label,
    status="drop_delete",
    icon=icon(icon),
    width = "300px"

  ) %>% div(class="myelement delete_holder",id=paste0(id,"_holder"))
}

clean_receive <- function(vec){
  if(vec %>% unique %>% `%in%`(0:1) %>% all) vec <- as.logical(vec)
  # if(is.character(vec)) vec <- iconv
  vec
}

clean_send <- function(vec){
  if(vec %>% unique %>% `%in%`(c(F,T)) %>% all) vec <- as.numeric(vec)
  if(F & is.character(vec)) {
    # Encoding(vec)="UTF-8"
    vec <- stri_enc_toascii(vec)
  }
  vec
}


# the deal with statement_id??
# source_id and statement_code which together define statement_id
# but there is little chance of them coming separated. they are never redefined.
# so we just save statement_id in the form source_id|statement_code.
# we also provide statement codes as a convenience
make_statement_id <- function(row){paste0(row$source_id," | ",row_statement_code)}
get_statement_code <- function(statement_ids){str_remove_all(statement_ids,"^.* \\| ")}
get_source_id <- function(statement_ids){str_remove_all(statement_ids," \\| .*$")}




get_file <- function(fil,conn){ ### always take the last if any duplicates. but this is dodgy because what is last #fixme
#  browser()
  # browser()

    # note I took out the step in each which takes the last slice of each combination cos it seems slow



  statements <- tbl(conn,"cm3statements") %>%
    filter(file==fil) %>%
    collect
  files <- tbl(conn,"cm3files") %>%
      filter(file==fil)  %>%
      collect
    sources <-  tbl(conn,"cm3sources") %>%
      filter(file==fil) %>%
      collect
    links <- tbl(conn,"cm3links") %>%
      filter(file==fil) %>%
      distinct(file,link_id,.keep_all = T) %>%
      collect
    settings  <-  tbl(conn,"cm3settings") %>%
      filter(file==fil) %>%
      collect

  statements <-
    statements %>%
    distinct(file,statement_id,.keep_all = T) %>%
    select(-any_of("row_names"))

  files <-
    files   %>%
      distinct(file,.keep_all = T) %>%
      ungroup %>%
      select(-any_of("row_names"))

# browser()
    sources <-
    sources %>%
      select(-any_of("row_names"))
# browser()
    if(nrow(sources)>0) sources <-
      sources %>%
      distinct(source_id,name,.keep_all = T) %>%
      pivot_wider(id_cols = c(source_id,file))


    links <-
      links %>%
      select(-any_of(xc("row_names source_id")))


    if(nrow(links)>0) links <-
      links %>%
      distinct(link_id,.keep_all = T) %>%
      add_link_sources(statements,sources) %>%
      add_link_counts(stage=1)

    settings <-
      settings %>%
      select(-any_of("row_names"))


  list(
    files=files,
    links=links,
    statements=statements,
    sources=sources,
    settings=settings
  )

}

create_file <- function(fil,u=NULL,description="",conn){
  # browser()
  fil <- (fil) %>% maxclean2()
  if(fil %in% (tbl(conn,"cm3files") %>% collect %>% pull(file) %>% replace_null(""))){message("Choose another name",4);return()}
  {
    val <-  tibble(
      file=fil,
      edit=collapc(u) %>% as.character,
      copy=collapc(u) %>% as.character,
      view=collapc(u) %>% as.character,
      created=time_stamp(),
      modified=time_stamp(),
      description="",
      archived=F
    )
    # append_rows(val,tab="cm3files")
    rows_insert(tbl(conn,"cm3files"),val,by="file",copy=T,in_place = T,conflict="ignore")
  }

}


delete_rows <- function(field,val,name,field2=NULL,val2=NULL,numfield=NULL,numval=NULL,conn=conn){
  # browser()
 # browser()
  condition <- paste0(local(field)," = '", local(val),"'")

  if(!is.null(field2)){

    condition <- condition %>%  paste0(" AND ",local(field2)," = '", local(val2),"'")
  }
  if(!is.null(numfield)){

    condition <- condition %>%  paste0(" AND ",local(numfield)," IN ('", local(collap(numval,"','")),"')")
  }
  delete_query = paste0("DELETE FROM ",local(name)," WHERE (",condition, ");")
  dbExecute(conn = conn,delete_query)
  #  notify("Deleted statement",4)
}


delete_file <- function(file,conn){
  delete_rows("file",file,"cm3files",conn=conn)
  delete_rows("file",file,"cm3statements",conn=conn)
  delete_rows("file",file,"cm3links",conn=conn)
  delete_rows("file",file,"cm3sources",conn=conn)
  delete_rows("file",file,"cm3settings",conn=conn)

}

delete_fileHADLEY <- function(fil,conn){
  browser()
  walk(xc("links sources statements settings files") %>% paste0("cm3",.),function(tab){
    rows_delete(tbl(conn,local(tab)),y=tibble(file=fil),copy = T,in_place = T,by="file",unmatched="ignore")
  })
}


archive_file <- function(fil,conn){
    rows_update(tbl(conn,"cm3files"),y=tbl(conn,"cm3files") %>% filter(file==fil) %>% mutate(archived=T),copy = T,in_place = T,by="file",unmatched="ignore")
}

update_one_field <- function(table,oldvalue,newvalue,field="file",conn){
  update_query <- paste0("update ",table," SET `",field,"` = '",
                         local(newvalue),
                         "' WHERE ",
                         "`",field,"` = '",
                         local(oldvalue),
                         "'"
  )
  # browser()
  dbSendQuery(conn, update_query)
}
rename_file <- function(oldname,newname,conn){
  newname <- (newname) %>% maxclean2()
  ## NOTE THIS DOES NOT CHECK PERMISSIONS
  if(newname %in% (tbl(conn,"cm3files") %>% collect %>% pull(file) %>% unique)){message("Choose another name");return()}

  browser()
  update_one_field("cm3files",oldname,newname,conn=conn)
  update_one_field("cm3links",oldname,newname,conn=conn)
  update_one_field("cm3statements",oldname,newname,conn=conn)
  update_one_field("cm3sources",oldname,newname,conn=conn)
  update_one_field("cm3settings",oldname,newname,conn=conn)
    # rows_update(tbl(conn,"cm3files"),tbl(conn,"cm3files") %>% filter(file==oldname) %>% mutate(file=newname),in_place = T,copy=T,unmatched="ignore",by="file")
    # rows_update(tbl(conn,"cm3links"),tbl(conn,"cm3links") %>% filter(file==oldname) %>% mutate(file=newname),in_place = T,copy=T,unmatched="ignore",by="file")
    # rows_update(tbl(conn,"cm3statements"),tbl(conn,"cm3statements") %>% filter(file==oldname) %>% mutate(file=newname),in_place = T,copy=T,unmatched="ignore",by="file")
    # rows_update(tbl(conn,"cm3sources"),tbl(conn,"cm3sources") %>% filter(file==oldname) %>% mutate(file=newname),in_place = T,copy=T,unmatched="ignore",by="file")
    # rows_update(tbl(conn,"cm3settings"),tbl(conn,"cm3settings") %>% filter(file==oldname) %>% mutate(file=newname),in_place = T,copy=T,unmatched="ignore",by="file")




}
fromJSONsafe <- function(tx){
  tx %>% str_replace_all("\"\"","\"")
}
duplicate_file <- function(oldname,newname,u=NULL,conn,old){
  newname <- (newname) %>% maxclean2()
  ## NOTE THIS DOES NOT CHECK PERMISSIONS FIXME
  # browser()
  if(newname %in% (tbl(conn,"cm3files") %>% collect %>% pull(file) %>% unique)){message("Choose another name");return()}
  {
    # oldfile <- sess$my_files %>% filter(file==oldname)
    # old <- get_file(oldname,conn=conn)
    # old <- sess$file
    oldfile <- old$files
    old$links <- old$links[,tbl(conn,"cm3links") %>% select(-any_of("row_names")) %>% colnames]
    val <-  tibble(
      file=newname,
      edit=collapc(c(u,uncollapc(oldfile$edit) %>% unlist %>% unique)),
      copy=collapc(c(u,uncollapc(oldfile$copy) %>% unlist %>% unique)),
      view=collapc(c(u,uncollapc(oldfile$view) %>% unlist %>% unique)),
      created=time_stamp(),
      modified=time_stamp(),
      description="",
      locked=F,
      archived=F
    )
  # browser()
    rows_append(tbl(conn,"cm3files"),val,in_place = T,copy=T)

    rows_append(tbl(conn,"cm3links"),old$links %>% mutate(file=newname),in_place=T,copy=T)
    rows_append(tbl(conn,"cm3statements"),old$statements %>% mutate(file=newname),in_place=T,copy=T)
    rows_append(tbl(conn,"cm3sources"),old$sources %>% select(-file) %>% pivot_longer(cols=-source_id) %>% mutate(file=newname),in_place=T,copy=T)
    rows_append(tbl(conn,"cm3settings"),old$settings %>% mutate(file=newname),in_place=T,copy=T)

  }

}

delete_links <- function(ids,fil,conn){
  # browser()
  message("Deleting links")
  links <- tibble(link_id=ids,file=fil)
  res <-
    rows_delete(
    tbl(conn,"cm3links"),
    links,
    by=xc("link_id file"),
    copy=T,
    in_place = T,
    unmatched = "ignore"
    )
  return(res %>% filter(file==fil) %>% collect)
}
add_links <- function(links,fil,conn){
  # browser()
  message("Adding links from fun")
  tmp <- tbl(conn,"cm3links")
  mx <- tmp %>% filter(file==fil) %>% mutate(mx=max(link_id,na.rm = T)) %>% pull(mx) %>% first
  links <-
    links %>%
    mutate(link_id = (mx+row_number())) %>%
    mutate(file=fil)

  links <- links %>% select(any_of(colnames(tmp)))

  res <- rows_append(tbl(conn,"cm3links"),links,copy=T,in_place = T)
  return(res %>% filter(file==fil))
}

### currently same as in cmfunctions
left_join_safe <- function (x, y, by = NULL, winner = "y", ...)
{
  if (is.null(by))
    by = intersect(colnames(x), colnames(y))
  if (winner == "y")
    x = x %>% select(-intersect(colnames(x), colnames(y)),
                     by)
  else y = y %>% select(-intersect(colnames(x), colnames(y)),
                        by)
  for (i in seq_along(by)) {
    y[, by[i]] <- coerceValue(unlist(y[, by[i]]), unlist(x[,
                                                           by[i]]))
  }
  left_join(x, y, by, ...)
}
# colours --------------------------------------------------------------

colorfun <- function(numvec,add_zero=T){
  # browser()
  ((scales::rescale(numvec,to=c(0,1),from=c(max(numvec),if(add_zero)0 else min(numvec))))^.8) %>%
    colorRamp(xc("#2f78bc white"),bias=1)(.) %>% apply(1,function(x)rgb(x[1]/255,x[2]/255,x[3]/255))
    # map(~modCol("#0000ff",darken=-.,saturate=1-.)) %>% unlist

}


# db actions --------------------------------------------------------------
# field="prompt"
# sess=list();sess$file="example-file"
update_settings <- function(field,value,fil,conn){
  # browser()
  # sets <- sess$file$settings$setting %>% replace_zero("[]") %>% fromJSON()
  sets <- tbl(conn,"cm3settings") %>% filter(file==fil) %>% pull(setting)  %>% replace_zero("[]") %>% fromJSON()
  new <-
    tibble("{field}":=value) %>% as.list
  combined <-
    c(sets,new) %>% toJSON() %>%
    tibble("file"=fil,setting=.)

  rows_delete(x=tbl(conn,"cm3settings"),y=combined,by="file",copy=T,in_place = T,unmatched="ignore")
  rows_insert(x=tbl(conn,"cm3settings"),y=combined,by="file",copy=T,in_place = T,conflict="ignore")
  return(tbl(conn,"cm3settings") %>% filter(file==fil))
}


# NLP ---------------------------------------------------------------------

make_MDS <- function(links,varname,max_overlaps=10,ncex=3){# note varname is unquoted
  links <-
    links %>%
    mutate(from_label=keep_top_level(from_label),to_label=keep_top_level(to_label))

  embeds <-
    links %>%
    get_both_labels() %>%
    create_new_embedding_rows(api_key=api_key) %>%
    distinct(old,.keep_all = T)


  dist_num <- dist(embeds[,-(1)], method = "canberra")
  mds_num <- cmdscale(dist_num, eig = TRUE, k = 2)

  mds_num <- data.frame(
    MDS1 = mds_num$points[, 1],
    MDS2 = mds_num$points[, 2],
    label = embeds$old,  type=1L,kind="none"
  ) %>%
    as_tibble()

  from_means <-
    links %>%
    select(label=from_label,{{varname}}) %>%
    left_join(mds_num,by="label") %>%
    group_by({{varname}}) %>%
    summarise(MDS1=mean(MDS1),MDS2=mean(MDS2)) %>%
    mutate(type=5L) %>%
    mutate(kind="from")

  to_means <-
    links %>%
    select(label=to_label,{{varname}}) %>%
    left_join(mds_num) %>%
    group_by({{varname}}) %>%
    summarise(MDS1=mean(MDS1),MDS2=mean(MDS2)) %>%
    mutate(type=5L) %>%
    mutate(kind="to")


  dat <- bind_rows(mds_num,from_means,to_means) %>%
    arrange(label) %>%
    mutate(label=ifelse(lag(label)==label,NA,label))


  froms <- dat %>% filter(kind=="from")
  tos <- dat %>% filter(kind=="to")
  # browser()
  arrows <- bind_rows(froms,tos) %>% filter(!is.na({{varname}})) %>% arrange({{varname}},type)

  ggplot(dat, aes(
    x = MDS1, y = MDS2,
    label = label, col = {{varname}},shape=kind,size=type
  )) +
    geom_path(data=arrows,aes(group={{varname}}),size=2,arrow = arrow(ends = "last", type = "closed"))+
    # geom_point(size=5,colour="black")+
    # ggrepel::geom_text_repel(cex = ncex,max.overlaps = max_overlaps)+
    scale_size_continuous(guide = "none")+
    scale_y_reverse(breaks=NULL,name="'Involvement with MENASP ➡️'")+
    scale_x_reverse(labels = NULL,name="'Outcome-ness' ➡️")+
    ggtitle("Average causal links for different groups",
            sub="Based on multi-dimensional scaling of the factor labels"
            )



}

make_prompt <- function(prompts,presence_penalty=0,frequency_penalty=0,model="4", api_key=api_key){
  # browser()
  message("Doing coding with model: " %>% paste0(model))
  row_index(prompts) %>%
    map(function(i){
      is_second <- "answer" %in% colnames(prompts)
      is_short <- prompts$text[i] %>% unlist %>% collap %>% nchar %>% `<`(28000*4)

      #message(prompts$text[i])


      messages <- list(
        list(
          role="user",content=prompts$text[i]  #%>% as.character %>% str_sub(1,3000)
        ))

      if(is_second) {
        message("Doing second pass of coding with model: " %>% paste0(model))
        # browser()
        messages[[2]]<- list(role="assistant",content=prompts$answer[i] )
        # messages[[2]]<- list(role="assistant",content="here they are")
        messages[[3]]<- list(role="user",content=prompts$auto_coding_second[i])
        # messages[[3]]<- list(role="user",content="thanks, you can stop now")
      }


      request("https://api.openai.com/v1/chat/completions") %>%
        req_headers(
          Authorization = paste0("Bearer ", api_key=api_key)
          ,
          "Content-Type"="application/json",
          method="POST"
        ) %>%
        req_body_json(
          list(
            messages = messages,

            model = ifelse(as.character(model)=="4"
                           ,
                           ifelse(is_short,("gpt-4"),"gpt-4-32k")
                           ,
                           "gpt-3.5-turbo"
            )
            ,
            presence_penalty=presence_penalty,
            frequency_penalty=frequency_penalty,

            temperature = 0
          )
        )
    }
    )
}

multi_ask <- function(prompts,model="3.5", api_key=api_key) {

  reqs <-make_prompt(prompts,presence_penalty=0,frequency_penalty=0,model=model, api_key=api_key)
  # browser()
  respsi <- multi_req_perform(reqs)

  fail <-
    vapply(respsi, inherits, "error", FUN.VALUE = logical(1))

  # if(any(fail)){message("Failed");return()}
  # resps <- respsi[!fail]
  # browser()
  seq_along(respsi) %>%
    map(function(s){
      if(!fail[[s]])resp_body_json(respsi[[s]])$choices[[1]]$message$content
      else "Failed"
    })
}
ask_chatgpt <- function(question,api_key,model="gpt-4") {
  # browser()
  response <- POST(
    url =  "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,
      messages = list(list(role="user",content=question)),
      temperature=0,
      presence_penalty=0,frequency_penalty=0
    )
  )

  # browser()
  notify("Received response",1)
  (content(response)$choices[[1]]$message)
}

get_links_from_answers <- function(conn,survey){

  answers_raw <-
    tbl(conn,"ss3answers") %>%
    filter(tolower(survey)==local(tolower(survey))) %>%
    filter(!is_admin) %>%
    collect

  answers <-
    answers_raw %>%
    filter(role=="assistant") %>%
    filter(str_detect(content,">>")) %>%
    arrange(time_stamp) %>%
    select(source=user,content,time_stamp)%>%
    group_by(source) %>%
    slice(n()) %>%
    ungroup
  # browser()
  if(nrow(answers)==0)return()

  get_links_from_arrows(answers)

}
get_links_from_arrows <- function(answers){
  answers %>%
    row_index() %>%
    map(function(i){
      row <- answers[i,]
      str_split(row$content,"\n") %>%
        unlist %>%
        keep(~str_detect(.,">>")) %>%
        str_remove("^- *") %>%
        str_split(">>") %>%
        map(str_trim) %>%
        map(tolower) %>%
        chains_to_links(source=row$source)
    }) %>%
    bind_rows %>%
    select(source_id=statement_id,from_label=from,to_label=to) %>%
    mutate(quote="")
}


make_simple_bundle <- function(vec){
  paste0(vec$from_label," ➡️ ",vec$to_label)
}
NLP_get_response <- function(df,start,toplevel=NULL,characters_per_prompt=NULL,prompts_per_batch=3,auto_coding_n_statements=1,control,presence_penalty=0,frequency_penalty=0,model="3.5",api_key=api_key,combine_small=F){
  if(
    is.null(start)){message("fail")
    return()
  }
  df <- df %>% filter(!is.na(text))

  message(paste0("NLP_get_response says: ",nrow(df)," original statements"))
  tx <- chunkify(df,characters_per_prompt = characters_per_prompt,combine_small=combine_small,prompts_per_batch=prompts_per_batch) # but in fact by default the statements are just under that length anyway
  message(paste0("NLP_get_response says: after chunkify, number of new prompts is ",nrow(tx)))
  html("auto_coding_warning",paste0("NLP_get_response says: after chunkify, number of new prompts is ",nrow(tx)))
  addCssClass("auto_coding_warning","show_me")

  # browser()

  original_prompts <-
    tx %>%
    select(statement_id,text,batch) %>%
    mutate(text=paste0(start,"\n",text,"\n\n"))


  all_resps=NULL
  all_statements <- NULL
  all_prompts <- NULL

  message("NLP_get_response says: number of chunkfied batches: " %>% paste0(max(original_prompts$batch)))
  html("auto_coding_warning",add = T,"</br>NLP_get_response says: number of chunkfied batches: " %>% paste0(max(original_prompts$batch)))

  for(batch in unique(original_prompts$batch)){
    prompts <- original_prompts[original_prompts$batch==batch,]

  message("NLP_get_response says: doing batch: " %>% paste0(batch))
  html("auto_coding_warning",add = T,"</br>NLP_get_response says: doing batch: " %>% paste0(batch))

  resps=NULL
  fail <- 1
  counter <- 0
  slice_size=100
  usage=tibble()
  counter_limit <- max(5,5*nrow(original_prompts)/slice_size)
  counter_limit <- 3 #############################################!!!!!!!!!!!


  message("NLP_get_response says: number of chunkfied prompts in this batch: " %>% paste0(nrow(prompts)))
  html("auto_coding_warning",add = T,"</br>NLP_get_response says: number of chunkfied prompts in this batch: " %>% paste0(nrow(prompts)))

  while(nrow(prompts)>0  & counter<counter_limit ){
    counter <- counter+1

  # browser()
    # dbListTables(conn)#
    # message("keeping local connection alive")

    message(paste0("NLP_get_response loop says: current nrow prompts in this loop:",nrow(prompts) ))
    html("auto_coding_warning",add = T,paste0("</br>Submitting ",nrow(prompts)," prompts simultaneously" ))
    message(paste0("NLP_get_response loop says: coding counter: ",counter))
    html("auto_coding_warning",add = T,paste0("</br>Attempt: ",counter))
  addCssClass("auto_coding_warning","show_me")


    reqs <-make_prompt(prompts,presence_penalty=presence_penalty,frequency_penalty=frequency_penalty,model=model %>% replace_null("4"),api_key = api_key)
    respsi <- multi_req_perform(reqs)

    statements <- prompts$statement_id #%>% head(slice_size)

    message(paste0("NLP_get_response attempt says: Getting response ",counter))
    message()
    html("auto_coding_warning",add = T,paste0("</br>Getting response: ",counter))
    html("auto_coding_warning",add = T,paste0("NLP_get_response attempt says: nrow prompts is now ",nrow(prompts)))

    fail <-
      vapply(respsi, inherits, "error", FUN.VALUE = logical(1))


    # browser()
    if(sum(fail)>0){
      message(paste0(sum(fail )," requests not complete. Request number is: ",paste0(which(fail),collapse=", ")))
      html("auto_coding_warning",add = T,paste0("</br>Requests not complete: ",paste0(which(fail),collapse=", ")))
      addCssClass("auto_coding_warning","show_me")
      }
    respsi <- respsi[!fail]

    usage <-
      respsi %>% map(~(resp_body_json(.x) %>% .$usage %>% .$total_tokens)) %>% unlist %>% tibble(tokens=.,model=model) %>%
      bind_rows(usage)
    statements <- statements[!fail]
    all_statements <- c(all_statements,statements)
    resps <- c(resps,respsi)

    prompts <-
      prompts %>% filter(!any(uncollapc(statement_id) %in% uncollapc(statements)))

  }
  all_resps <- c(all_resps,resps)
  }
  # if(auto_coding_second!="")
  # browser()
  tmp <-
    seq_along(all_resps) %>%
    map(function(s){
      resp_body_json(all_resps[[s]])$choices[[1]]$message$content %>%
        str_split("\n") %>%
        pluck(1) %>%
        tibble(answer=.,statement_id = original_prompts$statement_id[s]) %>%
        filter(answer!="")
    }) %>%
    bind_rows

  attr(tmp,"usage") <- usage

  write_rds(tmp,"tmp")

  html("auto_coding_warning",add = T,paste0("</br>====Completed autocoding"))
  addCssClass("auto_coding_warning","show_me")

  tmp


}
multi_import <- function(inFile){
  statements <-
    tibble(text=read_document(inFile$datapath) ) %>%
    mutate(source_id=inFile$name)

  # browser()

  if(str_detect(statements$text,"^--.*")  %>% any){

    statements <-
      statements %>%
      mutate(text= str_split(text,"\n" %>% unlist)) %>%
      unnest(cols=c(text))

    # browser()
    statements <-
      statements %>%
      filter(text!="^ *$") %>%
      mutate(section=str_detect(text,"^-- *") %>% cumsum) %>%
      group_by(section,source_id) %>%
      summarise(text=paste0(text,collapse="\n\n") %>% str_remove_all("--\n\n")) %>%
      ungroup %>%
      select(-section) %>%
      mutate(text=str_replace_all(text,"\n\n","\n")) %>%
      filter(text!="^-- *$") %>%
      mutate(source_id=str_extract(text,"source_id: *.*") %>% str_remove("source_id: *")) %>%
      mutate(question_id=str_extract(text,"question_id: *[^ |\n]*") %>% str_remove("question_id: *")) %>%
      mutate(respondent_id=str_extract(text,"respondent_id: *[^ |\n]*") %>% str_remove("respondent_id: *"))%>%
      mutate(question_id=if_else(is.na(question_id),lag(question_id), question_id) ) %>%
      mutate(respondent_id=if_else(is.na(respondent_id),lag(respondent_id), respondent_id) )
    statements  <-
      statements %>%
      filter(!str_detect(question_id %>% replace_na("asdflaskdfjasdfadsf"),"!IGNORE"))

  }
  # browser()
  statements

}

get_text <- function(inFile,x){
  # browser()
  pdf_text(inFile$datapath[x])%>%
    map(~str_replace_all(.," {2,}"," ")) %>%
    # map(~str_replace_all(.,"\r\n"," ")) %>%
    unlist %>%
    tibble(text=.,page=seq_along(.),source_id=inFile$name[[x]])#,author=pdf_info(inFile$datapath[[x]])$author,modified=pdf_info(inFile$datapath[[x]])$modified)
}



# tensiSplit <- function(string,size) {
#   str_extract_all(string, paste0('.{1,',size,'}'))
# }

str_break <- function(tex,size) { #brutal, should respect paragraphs #FIXME

  map(tex,function(string){
    # browser()
    substring(string,                     # Apply substring function
            seq(1, max(size,nchar(string)), size),
            seq(size, max(size,nchar(string)), size))}
  )
}

chunkify <- function(df,characters_per_prompt=7000,combine_small=F,prompts_per_batch=5){
if(combine_small){

  # browser()
  message("Chunkify says: combining small")
  df <-
  df %>%
    filter(!is.na(text)) %>%
   {if(T)mutate(.,text=paste0("New testimony:\n",text))} %>%
    mutate(text=strsplit(text, "(?<=\\.)\\s(?=[A-Z])", perl = T)) %>%
    mutate(nchara=map(text,~sum(unlist(nchar(.)))) %>% unlist) %>%
    mutate(grp=cumsum(nchara) %/% characters_per_prompt) %>%
    group_by(grp) %>%
        summarise(text=collap(unlist(text)),statement_id=collap(statement_id,","))

}
  df <-
    df %>%
    mutate(text=str_break(text,characters_per_prompt)) %>%
    unnest(text)
  df %>%
    mutate(batch=1+((row_number()-1) %/% (prompts_per_batch)))
}
afind2 <- function(text,from,to){
  # browser()
  from1 <- afind(text,from)$location[1]
  from2 <- from1+nchar(from)
  to1 <- afind(text,to)$location[1]
  to2 <- to1+nchar(to)
  str_sub(
    text,
    min(from1,from2,to1,to2),
    max(from1,from2,to1,to2)
  )

}
strip_higher <- function(text)str_remove_all(text,"^.*;") %>% str_trim
# strip_higher <- function(text)str_extract(text,"[^::]*$") %>% str_trim





