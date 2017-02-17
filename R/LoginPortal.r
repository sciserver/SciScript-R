#require(httr)
#require(jsonlite)

#source('Config.r')
#source('Authentication.r')

# Deprecated. Use Authentication.getKeystoneUserWithToken instead. Returns a dictionary with name/id for the user corresponding to the specified token
LoginPortal.getKeystoneUserWithToken<-function(token){
  .Deprecated("Authentication.getKeystoneUserWithToken")
  return (Authentication.getKeystoneUserWithToken(token))
}

# Deprecated. Use Authentication.login instead. Login and return token which is then also set as the Sys environment variabl "sciservertoken"
LoginPortal.login<-function(UserName, Password){
  .Deprecated("Authentication.login")
  return (Authentication.login(UserName, Password))
}

#  Deprecated. Use Authentication.getKeystoneUserWithToken instead. 
LoginPortal.getToken<-function(){
  .Deprecated("Authentication.getToken")
  Authentication.getToken()
}
# Deprecated. Use Authentication.setToken instead. 
LoginPortal.setToken<-function(token){
  .Deprecated("Authentication.setToken")
  Authentication.setToken()
}