library(SciServer) 

test.Authentication_allMethods<- function(){
  
  newToken1 = "myToken1"

  token1 = Authentication.login(Authentication_loginName, Authentication_loginPassword);
  token2 = Authentication.getToken()
  checkEquals(token1, token2)
  token4 = Authentication.token
  checkEquals(token1, token4)

  user = Authentication.getKeystoneUserWithToken(token1)
  checkEquals(Authentication_loginName, user$userName)
  
  Authentication.setToken(newToken1)
  checkEquals(newToken1, Authentication.getToken())
  
  token1 = Authentication.login(Authentication_loginName, Authentication_loginPassword);
}
