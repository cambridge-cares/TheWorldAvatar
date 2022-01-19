# README for Python Custom Libraries 
## Where is this used? 
1. JPS DES
2. JPS SHIP 
3. Anywhere you use for logging to Java or feeding back to Java purposes. 

## How to deploy? 
1. In Line 24 of pythonLogger.py, there is this line: `self.logServerURL = 'http://localhost:8080/JPS_BASE/LogServer'` which you can change to 8081 if you have another port running there. This is what is currently done in Claudius
2. In your virtual environment, or system wide, run `python3 setup.py install`


## What does it do? 
Pretty useful if you want to return results to Java from a Python Script. Typically used for logging results. 

