# README for Waste To Energy Agent
## Why are there no results from WasteToEnergySystemAgent? 
1. Check if you have a Gurobi License
	1. go to this website: https://www.gurobi.com/login/
	2. Once you've created an account (sign in with your Cambridge email!) go to licenses page. Check if your licenses are available and whether they've expired: https://www.gurobi.com/downloads/licenses/
	3. get yourself a license if your pre-existing license has expired: 
	https://www.gurobi.com/downloads/free-academic-license/
	4. Download it by typing it into your CMD line
	5. Downloaded!
2. Check if you have Matlab downloaded
	1. Download using your Cambridge license
	2. add matlab to your system environment

## I've ran Waste To Energy Agent, but after the Matlab simulation runs, nothing happens?
1. Check if your JPS_AWS is running
	- How do you check? Check if something like this: Response:uk.ac.cam.cares.jps.aws.AsynchronousWatcherService$ResponseBody@XXXXXXXX is there when matlab starts
	- After matlab runs to *completion*, this line shows: New file observed: year by year_NPV.txt
	- Then this line: Calling back http://HOST/JPS_WTE/processresult would call back your next value

