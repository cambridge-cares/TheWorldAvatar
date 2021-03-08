--INSTALLATION--
1) Create a virtual environment in python for use with the parser. This can be done easily using conda/Anaconda.
2) Open a command prompt with python. 
3) Activate the virtual environment that you want to store the parser code in. 
4) Change directory to thermochemistry/CoMoCompChemParser   
5) Run "pip install -e ." (This installs the code in develop version so your changes will be linked) 
6) Hope that all the necessary packages install successfully. 

--RUNNING THE TESTS-- 
1) Change director into the 'tests' folder (or point the path to the test_gaussian.py script, either will work).
2) Run "python -m unittest test_gaussian.py" 
3) Let the tests run. If the tests run sucessfully you will see "Ran 1 test in 5.899 s" and "OK" - the actual time may be different.
4) The test should check the parsing of a variety of Gaussian log files - 16 in total consisting of different job types, 
   print directives, and log file structures, generate JSON files for each of these logs and compare them to the reference JSON files. 
5) If the tests passed - all good. 
6) If you delete the reference JSON files, you will need to re-generate them to run the comparison test. 
   Uncomment lines 45 and 46 in test_gaussian.py to generate the files, but remember to comment them back out when checking new changes.

--PARSING A LOG FILE-- 
1) Simply run "ccparse -f PATH/LOGFILENAME.log" to parse the log file. If successful, you will see "finished!" message. 
   Note that you should be able to run this command from anywhere and it should work as long as the virtual environment
   with the parser is active.  
2) You can also generate a JSON file for the log, although this has been left as optional 
   (as we want to upload the info to the knowledge graph directly after parsing). To do this, 
   use the -j option: "ccparse -f PATH/LOGFILENAME.log -j True". This will generate JSON file(s) with LOGFILENAME.json 
   or in the case of a log file containing multiple jobs LOGFILENAME#1.json,LOGFILENAME#2.json ... LOGFILENAME#N.json
   with #N being the number of linked jobs in the log file. 

--EDITING--
1) With the -e option specified in installation, the code should be linked in develop, and so changes you make should be 
   active without the need to re-install. 
2) The exception is if you change the main.py file. In this case, you will most likely have to run "pip uninstall" to remove the parser 
   and then re-run "pip install -e ." to update the changes.   
   
 

 