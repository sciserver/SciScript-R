#require(httr)
#require(jsonlite)

#source('Config.r')

# returns a dictionary with name/id for the user corresponding to the specified token
Authentication.getKeystoneUserWithToken<-function(token){
  
  loginURL = paste(Config.AuthenticationURL,"/",token,sep='')
  tryCatch({
    r=GET(loginURL,encode="json",accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      #print("Error")
      #r=content(r, encoding="UTF-8")
      #print(r$error$message)
      stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
    } else {
      r= content(r)
      user={}
      user$name=r$token$user$name
      user$id=r$token$user$id
      return(user)
    }
  }, error = function(e) {
    stop(e)
  })
}

# login and return token which is then also set as the Sys environment variabl "sciservertoken"
Authentication.login<-function(UserName, Password){
  loginURL = Config.AuthenticationURL
  authJson = list(auth=list(identity=list(password=list(user=list(name=unbox(UserName),password=unbox(Password))))))
  r=POST(loginURL ,encode="json",body=authJson,accept("text/plain"),content_type_json())
  if(r$status_code != 200) {
    #print("Error")
    #r2=content(r, encoding="UTF-8")
    #print(r2$error$message)
    stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
  } else {
    token=headers(r)$`x-subject-token`
    Authentication.setToken(token)
    return(token)
  }
}

Authentication.getToken<-function(){
  token = Sys.getenv("sciservertoken")
  if (is.null(token) || token == "") {
    token=NULL
#   next secific to sciserver compute environment
    f = '/home/idies/keystone.token'
    if(file.exists(f)){
      token = readLines(f)
      Authentication.setToken(token)
    }
  }
  token
}

Authentication.setToken<-function(token){
  Sys.unsetenv("sciservertoken")
  Sys.setenv(sciservertoken=token)
}