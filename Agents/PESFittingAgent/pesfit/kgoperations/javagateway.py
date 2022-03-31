from py4jps.resources import JpsBaseLib

jpsBaseLibGW = JpsBaseLib(**{'eager_load': True,'gateway_parameters': {'auto_field': True}})
#stdouterr_file_handle = open('C:\\Users\LPAS01\stdouterr_file.out','w')
#jpsBaseLibGW.launchGateway(**{'redirect_stdout':stdouterr_file_handle,'redirect_stderr':stdouterr_file_handle})
jpsBaseLibGW.launchGateway(**{})