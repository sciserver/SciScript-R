#require(httr)
#require(jsonlite)

#source('Config.r')

# this variable stores the Authentication token for the user's session
Authentication.token = NULL

# returns a dictionary with name/id for the user corresponding to the specified token
Authentication.getKeystoneUserWithToken<-function(token){
  
  taskName = ""
  if(Config.isSciServerComputeEnvironment()){
    taskName = "Compute.SciScript-R.Authentication.getKeystoneUserWithToken"
  }else{
    taskName = "SciScript-R.Authentication.getKeystoneUserWithToken"
  }
  
  loginURL = paste(Config.AuthenticationURL,"/",token,"?TaskName=",taskName,sep='')
  r=GET(loginURL,encode="json",accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
  if(r$status_code != 200) {
    stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
  } else {
    r= content(r)
    user={}
    user$userName=r$token$user$name
    user$id=r$token$user$id
    return(user)
  }
}

# login and return token which is then also set as the Sys environment variable "sciservertoken" and the variable Authentication.token
Authentication.login<-function(UserName, Password){
  
  taskName = ""
  if(Config.isSciServerComputeEnvironment()){
    taskName = "Compute.SciScript-R.Authentication.login"
  }else{
    taskName = "SciScript-R.Authentication.login"
  }
  
  loginURL = paste(Config.AuthenticationURL,"?TaskName=",taskName,sep="")
  authJson = list(auth=list(identity=list(password=list(user=list(name=unbox(UserName),password=unbox(Password))))))
  r=POST(loginURL ,encode="json",body=authJson,accept("text/plain"),content_type_json())
  if(r$status_code != 200) {
    stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
  } else {
    token=headers(r)$`x-subject-token`
    Authentication.setToken(token)
    return(token)
  }
}

Authentication.getToken<-function(){
  
  if(Config.isSciServerComputeEnvironment() == TRUE){
    
    f = Config.KeystoneTokenFilePath
    if(file.exists(f)){
      token = readLines(f)
      if (!is.null(token) && token != ""){
        unlockBinding("Authentication.token", as.environment("package:SciServer"))
        assign("Authentication.token",token,envir=as.environment("package:SciServer"))
        Sys.unsetenv("sciservertoken")
        Sys.setenv(sciservertoken=token)
        return(token)
      }else{
        warning(paste("In Authentication.getToken: Cannot find token in system token file ",Config.KeystoneTokenFilePath, sep=""))
        return(NULL)
      }
    }else{
      warning(paste("In Authentication.getToken: Cannot find system token file ",Config.KeystoneTokenFilePath,sep=""))
      return(NULL)
    }
    
  }else{
    token <- NULL
    if(exists("Authentication.token")){
      token <- Authentication.token
    }
    if (is.null(token)) {
      token = Sys.getenv("sciservertoken")
      if (!is.null(token) && token != ""){
        Authentication.setToken(token)
        return(token)
      }else{
        warning("In Authentication.getToken: Authentication token is not defined: the user did not log in with the Authentication.login function")
        return(NULL)
      }
    }
  }
}

Authentication.setToken<-function(token){
  
  if( Config.isSciServerComputeEnvironment() == TRUE){
    warning("Authentication token cannot be set to arbitary value when inside SciServer-Compute environment.")
  }else{
    unlockBinding("Authentication.token", as.environment("package:SciServer"))
    assign("Authentication.token",token,envir=as.environment("package:SciServer"))
    
    Sys.unsetenv("sciservertoken")
    Sys.setenv(sciservertoken=token)
  }
}