#require(httr)
#require(jsonlite)

#source('Config.r')

# Deprecated. Use Authentication.getKeystoneUserWithToken instead. Returns a dictionary with name/id for the user corresponding to the specified token
LoginPortal.getKeystoneUserWithToken<-function(token){
  .Deprecated("Authentication")
  return Authentication.getKeystoneUserWithToken(token)
}

# Deprecated. Use Authentication.login instead. Login and return token which is then also set as the Sys environment variabl "sciservertoken"
LoginPortal.login<-function(UserName, Password){
  .Deprecated("Authentication")
  return Authentication.login(UserName, Password){
}

#  Deprecated. Use Authentication.getKeystoneUserWithToken instead. 
LoginPortal.getToken<-function(){
  .Deprecated("Authentication")
  Authentication.getToken()
}
# Deprecated. Use Authentication.setToken instead. 
LoginPortal.setToken<-function(token){
  .Deprecated("Authentication")
  Authentication.setToken()
}