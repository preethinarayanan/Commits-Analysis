# library(httr)
# library(jsonlite)
# url_jira_ini<-"https://jira.host/jira/rest/auth/1/session "
# url_jira<-"https://jira.host/jira/rest/api/2/long_name "
# credentials<-"username:password"
# credentials <- base64_enc(credentials)
# header_auth <- paste0("Basic ",credentials)
# GET(url_jira,
#     add_headers(Authorization = header_auth),
#     set_cookies( atlassian.xsrf.token = "long_cookie_1",
#                  JSESSIONID = "long_cookie_2"),
#     authenticate(user = "pnarayanan@arrowheadgrp.com",password = "Parknight1!",type="basic"),use_proxy("proxy.host",8080,username="myusername",password="mypassword", auth="basic"),verbose(),accept_json())


library("httr")
library("jsonlite")

my_UN <- ("pnarayanan@arrowheadgrp.com")
my_PW <- ("1wLXv7nZKCN8GxJAtgJ1DC01")

alldata <-  {
  
  req <- GET("https://bitbucket.org/",
             path = "rest/api/2/search?jql=your jql query",
             authenticate(user = my_UN,password = my_PW,type="basic"),
             verbose()
  )
  
  api_request_content <- httr::content(req, as = "text")
  api_request_content_flat <- jsonlite::fromJSON(api_request_content)
  as.data.frame(api_request_content_flat$issues, flatten=T)
}

