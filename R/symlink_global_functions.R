## remember this is really in the shared functions folder
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




# db basics ----------------------------------------------------------------------

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

# NLP ---------------------------------------------------------------------
make_prompt <- function(prompts,presence_penalty=0,frequency_penalty=0,model="4", api_key=api_key){
  # browser()
  row_index(prompts) %>%
    map(function(i){
      # browser()
      is_short <- prompts$text[i] %>% unlist %>% collap %>% nchar %>% `<`(28000*4)

      message(prompts$text[i])
      request("https://api.openai.com/v1/chat/completions") %>%
        req_headers(
          Authorization = paste0("Bearer ", api_key=api_key)
          ,
          "Content-Type"="application/json",
          method="POST"
        ) %>%
        req_body_json(
          list(

            #            messages = list(list(role="user",content="what time is it?")),
            messages = list(list(role="user",content=prompts$text[i])),

            model = ifelse(as.character(model)=="4"
                           ,
                           ifelse(is_short,("gpt-4"),"gpt-4-32k")
                           ,
                           "gpt-3.5-turbo"
            )
            # max_tokens = 2000,
            # n = 1,
            # stop = "\n"
            ,
            presence_penalty=presence_penalty,
            frequency_penalty=frequency_penalty,
            # top_p=0.0000000000000000001,

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
      model = "gpt-4",
      # model = "gpt-3.5-turbo",
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

