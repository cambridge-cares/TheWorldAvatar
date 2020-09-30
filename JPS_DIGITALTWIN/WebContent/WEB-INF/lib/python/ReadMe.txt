Necessary custom installations for running JPS_Arbitrage

1. Selenium webdriver/PhantomJS

When installing selenium you need to download geckodriver.exe and add itself and its path to the environment variables.
In order to run in a headless mode (i.e. no separate browser window) you need to download phantomjs and add its and path to the environment variables.
After adding paths the OS needs a restart.
Also, when running the script in python console it may need to be run in admin mode (at least on Windows 10).


2. csv_funs.py

Add csv_funs.py to \Lib\site-packages within your Python installation folder