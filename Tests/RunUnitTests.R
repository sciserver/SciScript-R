library('RUnit')
library(SciServer)
library(data.table)

#======================================================================================================================================
# CONFIGURATION
#======================================================================================================================================

# Define login Name and password before running the tests:
Authentication_loginName = '***';
Authentication_loginPassword = '***'

CasJobs_TestTableName1 = "MyNewtable1"
CasJobs_TestTableName2 = "MyNewtable2"
CasJobs_TestDatabase = "MyDB"
CasJobs_TestQuery = "select 4 as Column1, 5 as Column2 "
CasJobs_TestTableCSV = "Column1,Column2\n4,5\n"
CasJobs_TestFitsFile = "SciScriptTestFile.fits"
CasJobs_TestCSVFile = "SciScriptTestFile.csv"

SkyServer_TestQuery = "select top 1 specobjid, ra, dec from specobj order by specobjid"
SkyServer_DataRelease = "DR13"
SkyServer_ImageSideLength = 64
SkyServer_QueryResultCSV = "specobjid,ra,dec\n299489677444933632,146.71421,-1.0413043\n"
SkyServer_RadialSearchResultCSV = 'objid,run,rerun,camcol,field,obj,type,ra,dec,u,g,r,i,z,Err_u,Err_g,Err_r,Err_i,Err_z\n1237671939804561654,6162,301,3,133,246,3,258.250804,64.051445,23.339820,22.319400,21.411050,21.119710,20.842770,0.664019,0.116986,0.076410,0.080523,0.238198\n'
SkyServer_RadialSearchResultNumColumns = 19
SkyServer_RectangularSearchResultCSV = 'objid,run,rerun,camcol,field,obj,type,ra,dec,u,g,r,i,z,Err_u,Err_g,Err_r,Err_i,Err_z\n1237671939804628290,6162,301,3,134,1346,6,258.304721,64.006203,25.000800,24.500570,22.485400,21.103450,20.149990,0.995208,0.565456,0.166184,0.071836,0.124986\n'
SkyServer_RectangularSearchResultNumColumns = 19
SkyServer_ObjectSearchResultObjID = 1237671939804561654
SkyServer_RadialSearchResultObjID = 1237671939804561654
SkyServer_RectangularSearchResultObjID = 1237671939804628290

SciDrive_Directory = "/SciScriptR"
SciDrive_FileName = "TestFile.csv"
SciDrive_FilePath = "./TestFile.csv"
SciDrive_FileContent = "Column1,Column2\n4.5,5.5\n"

SkyQuery_TestTableName = "TestTable_SciScript_R"
SkyQuery_TestTableCSV = "Column1,Column2\n4.5,5.5\n"
SkyQuery_TestTableCSVdownloaded = "ID,Column1,Column2\n1,4.5,5.5\n"
SkyQuery_Query = "select 4.5 as Column1, 5.5 as Column2"

FileService_NewDirName = "UnitTestDir"
FileService_NewFileName = "MyNewFile.txt"
FileService_newFileContent = "#ID,Column1,Column2\n1,4.5,5.5\n"
FileService_RemoteFileName = "persistent/" + FileService_NewDirName + "/" + FileService_NewFileName
FileService_RemoteDirName = "persistent/" + FileService_NewDirName

ComputeJobs_NotebookName = "TestNotebook.ipynb"
ComputeJobs_LocalNotebookPath = "./" + ComputeJobs_NotebookName
ComputeJobs_RemoteNotebookPath = "/home/idies/workspace/persistent/" + ComputeJobs_NotebookName
ComputeJobs_JobAlias = "MyJobAlias"
ComputeJobs_Parameters = "param1"
ComputeJobs_ShellCommand = "ls";
ComputeJobs_ImageNameLike = "python"  # must be all lowercase


#======================================================================================================================================
# RUNNING TESTS
#======================================================================================================================================

test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'AuthenticationTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'LoginPortalTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'CasJobsTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'SkyServerTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'SciDriveTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'SkyQueryTest.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
#test.suite <- defineTestSuite("example",dirs = file.path("SciServerUnitTests"),testFileRegexp = 'SciServerUnitTests.R');test.result <- runTestSuite(test.suite); printTextProtocol(test.result)
