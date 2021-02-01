# JPS MEN

## GAMS
 	- Have an installation of gams in your system environment if you want to deploy this locally. Any gams would do; I tried it with v24 and v30, and even if there's an error that's logged in Tomcat, it doesn't matter because it will still run. 
 	- If you need to install Gams, CARES has a license. 

 	- However, there's a little thing you need to add in if you're deploying anywhere except for Claudius. We've got a license for a min linear problem solver in Claudius, but we don't have one locally. So in Line 153 of /conf/MENGAMSCode.txt, insert a line: 
 	` option minlp = bonmin;`
 	  This would allow us to run minlp without encountering an error. 

## Java Dependencies. 
 1. jps-base-lib
 2. Junit
 3. javax servlet
 4. jena-arq (for query builder)
 5. commons-validator (for input validation)
 6. com.gams.api (This jar file has to be downloaded from Claudius as it's proprietary. Hunt in the libraries in .m2 folder/repositories.)

## What to do for deployment? 
 - Have GAMS installed. Have java installed. Both should be added to your system environment. Have the Gams jar file in your .m2 folder. 
 - mvn clean install JPS MEN
 - Expected Result: a table after clicking 'start simulation'. If locally deployed, this would be different from the paper as the paper is using the default minlp solver that we don't possess locally. 

### TODO: 
 - [x] Finish backup of visualization
 - [ ] Once MEN fails, the other resources can't be accessed (i.e. once MEN stalls, I can't execute a call to Tomcat)
 - [ ] \(Optional) virtual environment for python

### What if the visualization isn't working?
 - You don't have Gams installed. Or the Gams API jar file downloaded in your m2 folder. 
 - **Quick Fix** : In WebContent/scripts/start.js, there are two lines: Line 29, and Line 117. Currently, the default is for Line 29 to be called when start runs. Disable that by modifying it from ` start` to `startv2` and modifying Line 117 from `startv2` to `start`
 - **Long Fix** : Check if Gams is running. I cannot emphasize this enough; this is the most common problem. How to do so is to check if you have a gams folder under JPS_DATA\workingdir\JPS_MEN and check the last run date. Then check the tests if they're working. 
 	
