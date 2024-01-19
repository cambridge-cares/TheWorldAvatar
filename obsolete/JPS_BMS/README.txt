change line 26 of rdfTest.py, according to test or production environment, i.e.

--in test environment--
outputDir = config['TEST']['OUTPUT_DIR']

--in production environment--
outputDir = config['PROD']['OUTPUT_DIR']
