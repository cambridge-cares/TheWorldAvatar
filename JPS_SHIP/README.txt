A Python Virtual Environment will now be created during 'mvn clean install' on the JPS_SHIP project. 

Prerequisite for non-Windows user:
Please install Python 3.6.8 via preferred download management tool (e.g. Homebrew for MacOS) or the installer available on https://www.python.org/downloads/release/python-368/ before running 'mvn clean install' on the JPS_SHIP project. 

To use this virtual environment for testing, please do the following:
1) cd to JParkSimulator-git\JPS_SHIP\python\ADMS-speed-load-map in your workarea.
2) Execute the command 'python -m pipenv --venv' (or just 'pipenv --venv' on non-Windows).
3) You should now see a path where your virtual environment is. 
4) Copy the path and replace the value of speed.load.map.venv.dir in jpstest.properties located in JPS_BASE_LIB.
	e.g. speed.load.map.venv.dir = C:\\Users\\RFOO01\\.virtualenvs\\ADMS-speed-load-map-KcR3PzvU
5) Run 'mvn clean install' on JPS_BASE_LIB.

