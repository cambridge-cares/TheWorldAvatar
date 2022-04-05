from py4jps.resources import JpsBaseLib

#stdout_file_handle = open('std_out_err.txt','w')

jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()
#**{'redirect_stdout':stdout_file_handle,'redirect_stderr':stdout_file_handle}