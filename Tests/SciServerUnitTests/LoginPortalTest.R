library(SciServer) 

test.LoginPortal_allMethods<- function(){
  
  newToken1 = "myToken1"
  
  token1 = LoginPortal.login(Authentication_loginName, Authentication_loginPassword);
  token2 = LoginPortal.getToken()
  checkEquals(token1, token2)

  user = LoginPortal.getKeystoneUserWithToken(token1)
  checkEquals(Authentication_loginName, user$userName)
  
  LoginPortal.setToken(newToken1)
  checkEquals(newToken1, LoginPortal.getToken())
  
}


