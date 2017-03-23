library('RUnit')
library(SciServer)

#source('sample.R')

Authentication_loginName = 'manu9';
Authentication_loginPassword = '12345'


CasJobs_TestTableName1 = "MyNewtable1"
CasJobs_TestTableName2 = "MyNewtable2"
CasJobs_TestDatabase = "MyDB"
CasJobs_TestQuery = "select 4 as Column1, 5 as Column2 "
CasJobs_TestTableCSV = "Column1,Column2\n4,5\n"
CasJobs_TestFitsFile = "SciScriptTestFile.fits"
CasJobs_TestCSVFile = "SciScriptTestFile.csv"



test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'AuthenticationTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'LoginPortalTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'CasJobsTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
#test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'SkyServerTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
#test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'SkyQueryTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
#test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'SciDriveTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)