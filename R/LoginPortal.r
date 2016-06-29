require(httr)
require(jsonlite)

#source('Config.r')

# returns a dictionary with name/id for the user corresponding to the specified token
LoginPortal.getKeystoneUserWithToken<-function(token){
  loginURL = paste(Config.LoginPortalURL,"/",token,sep='')
  tryCatch({
    r=GET(loginURL,encode="json",accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
    r= content(r)
    user={}
    user$name=r$token$user$name
    user$id=r$token$user$id
    return(user)
  }, error = function(e) {
    print(e)
    return (NULL)
  })
}

# login and return token which is then also set as the Sys environment variabl "sciservertoken"
LoginPortal.login<-function(UserName, Password){
  loginURL = Config.LoginPortalURL
  authJson = list(auth=list(identity=list(password=list(user=list(name=unbox(UserName),password=unbox(Password))))))
  r=POST(loginURL ,encode="json",body=authJson,accept("text/plain"),content_type_json())
  if(r$status_code != 200) {
    print("Error")
    print(content(r))
    return (NULL)
  } else {
    token=headers(r)$`x-subject-token`
    LoginPortal.setToken(token)
    return(token)
  }
}

LoginPortal.getToken<-function(){
  token = Sys.getenv("sciservertoken")
  if (is.null(token) || token == "") {
    token=NULL
#   next secific to sciserver compute environment
    f = '/home/idies/keystone.token'
    if(file.exists(f)){
      token = readLines(f)
      LoginPortal.setToken(token)
    }
  }
  token
}

LoginPortal.setToken<-function(token){
  Sys.unsetenv("sciservertoken")
  Sys.setenv(sciservertoken=token)
}