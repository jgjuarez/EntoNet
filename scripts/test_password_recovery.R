library(httr2)
e <- new.env(parent = globalenv())
for (expr in parse('shiny_app/app.R')) {
  if (is.call(expr) && identical(expr[[1]], as.name('<-')) && is.symbol(expr[[2]]) &&
      as.character(expr[[2]]) %in% c('value_or_default', '%||%', 'supabase_auth_send_password_recovery', 'supabase_auth_update_password')) eval(expr, e)
}
e$storage_project_url <- function() 'https://example.supabase.co'
e$auth_redirect_url <- 'https://portal.example.org/'
e$supabase_auth_api_key <- 'test-key'
e$profile <- data.frame(email='registered@example.org', activo=TRUE, user_id='test-user')
e$fetch_usuario_perfil <- function(...) e$profile
e$calls <- list()
e$req_perform <- function(req) {
  e$calls[[length(e$calls) + 1L]] <- req
  response(200, headers=list('content-type'='application/json'), body=charToRaw('{}'))
}
invisible(e$supabase_auth_send_password_recovery(' REGISTERED@example.org '))
stopifnot(length(e$calls) == 1L)
r <- e$calls[[1]]
stopifnot(identical(url_parse(r$url)$query$redirect_to, e$auth_redirect_url))
stopifnot(identical(r$body$data, list(email='registered@example.org')))
for (profile in list(data.frame(), transform(e$profile, activo=FALSE),
                     transform(e$profile, email='other@example.org'),
                     transform(e$profile, user_id=NA_character_))) {
  e$profile <- profile
  e$supabase_auth_send_password_recovery('registered@example.org')
  stopifnot(length(e$calls) == 1L)
}
invisible(e$supabase_auth_update_password('test-access-token', 'test-new-password'))
r <- e$calls[[2]]
stopifnot(identical(r$method, 'PUT'), endsWith(r$url, '/auth/v1/user'),
          'Authorization' %in% names(r$headers),
          identical(r$body$data, list(password='test-new-password')))
cat('PASS: redirect query, normalized email, existing active profile gate, user password update request\n')
# Exercise the browser callback without a browser or real credentials.
if (requireNamespace('V8', quietly=TRUE)) {
  ctx <- V8::v8()
  ctx$eval("var events={}, sent=[], stored={}; var document={title:'EntoNet',addEventListener:function(n,f){events[n]=f;}}; var window={location:{hash:'#access_token=test-token&type=recovery',search:'',pathname:'/'},sessionStorage:{setItem:function(k,v){stored[k]=v;},getItem:function(k){return stored[k]||null;},removeItem:function(k){delete stored[k];}},history:{replaceState:function(){window.location.hash='';}}}; function $(d){return {on:function(n,f){events[n]=f;}};} function setTimeout(f){f();} function URLSearchParams(s){this.s=s;this.get=function(k){var pairs=this.s.split('&');for(var i=0;i<pairs.length;i++){var p=pairs[i].split('=');if(p[0]===k)return decodeURIComponent(p[1]||'');}return null;};this.toString=function(){return this.s;};} var Shiny={shinyapp:{$socket:{readyState:0}},setInputValue:function(n,v){sent.push({name:n,value:v});}};")
  lines <- readLines('shiny_app/app.R')
  first <- which(grepl('(function storeInitialSupabaseAuthParams', lines, fixed=TRUE))
  last <- first + which(trimws(lines[(first+1):length(lines)]) == '\")),')[1]
  ctx$eval(paste(lines[first:(last-1)], collapse='\n'))
  ctx$eval("events['DOMContentLoaded'](); if(sent.length!==0)throw Error('sent before connection'); Shiny.shinyapp.$socket.readyState=1; events['shiny:connected'](); if(sent.length!==1||sent[0].value.access_token!=='test-token')throw Error('lost recovery callback'); events['shiny:connected'](); if(sent.length!==1)throw Error('replayed callback'); window.location.hash='#error_description=expired'; events['shiny:connected'](); if(sent[1].name!=='supabase_auth_error'||window.location.hash!=='')throw Error('error handling failed');")
  cat('PASS: delayed Shiny connection, recovery delivery, no replay, expired-link callback\n')
}
