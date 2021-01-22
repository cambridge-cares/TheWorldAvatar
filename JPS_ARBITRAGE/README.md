# JPS Arbitrage

## Python Dependencies
 1. caresjpsutil (for logging, see installation notes in JPS_BASE/PythonCustomLibraries/setup.py) 
 2. matplotlib.pyplot
 3. json
 4. requests
 5. lxml (for scraping through a website)
 6. urllib
 7. selenium (and phantomJS/chromium webserver to do scraping. Recommend phantomJS because it's currently in the code despite being no longer maintained)

## Java dependencies? 
1. jps-base-lib
2. Junit
3. javax servlet
4. jena-arq (for query builder)
5. com.cmclinnovations.mods **.Mods_Java_APIv0.3.3** (This jar file has to be downloaded from Claudius as it's proprietary. Hunt in the libraries in .m2 folder/repositories. )

## PhantomJS? 
 - You'll have to install phantomJS from this web address: [https://phantomjs.org/download.html ] and download from there. 

## What to do for deployment? 
 - Have python installed. Have java installed. Both should be added to your system environment. 
 - mvn clean install JPS arbitrage
 - open it in Chrome. The graphs don't display nicely in other browsers. 
 - Expected Result: After clicking on the button, two graphs and a paragraph is shown on the webpage

### Websites referenced: 
If the format of the websites change, then this part no longer works. The maintainer has to change it if they think they can fix this. (last accessed 22/1/21)
How do we know if it's not working? If the python logger logging to tomcat server shows start of XX.py but doesn't end, it's not working
 1. CPO_to_FAME.py: 
 	- https://www.oanda.com/currency/converter/
 	- http://www.usinflationcalculator.com/
 	- https://www.icis.com/resources/news/2013/11/08/9723077/se-asia-to-china-palm-oil-freight-rates-may-fall-on-weak-demand/ 
 	- https://www.icis.com/resources/news/2013/11/08/9723077/se-asia-to-china-palm-oil-freight-rates-may-fall-on-weak-demand/
 	- https://www.ema.gov.sg/Non_Residential_Programmes_Electricity_Tariffs.aspx

 2. exchange_rates.pyw
 	-  http://apilayer.net/api/live?access_key=402d77f0850c35adfa5a797e325262dd&currencies=CNY,SGD&source=USD&format=1

 3. FAME_download.pyw
 	-  http://www.cmegroup.com/trading/energy/refined-products/fame-0-argus-biodiesel-fob-rdam-red-compliant-swap-futures.html 
 4. HNG_download.pyw
 	- http://www.cmegroup.com/trading/energy/natural-gas/natural-gas_quotes_globex.html 
 5. NG_to_MeOH_MoDS.py
 	- https://business.directenergy.com/understanding-energy/energy-tools/conversion-factors.
 6. ZCE_download.pyw
 	- http://english.czce.com.cn/enportal/DFSStaticFiles/Future/EnglishFutureQuotesMA.htm
  	
### TODO: 
 - [x] Finish backup of visualization
 - [ ] \(Optional) virtual environment for python


### What if the visualization isn't working?
 - It's likely one of those pyw functions no longer work. 

 - **Quick fix**: Comment Line 39, uncomment line 40 of WebContent/scripts/mods-handler.js. If that still doesn't work; it's a problem with D3 library. 

 - **Longer fix**: Run the Junit tests. 
 	
 	1. If a testretrieving works, but not testRetrieving in TestMarketPackage.java, it's most likely your python installation. Like it can't run. 
 	
 	2. If both doesn't work, it's a problem with your python libraries. More specifically, phantomJS webdriver isn't installed and/or placed in your environment path. Search stackoverflow to do so. 
 	
 	3. Then check if your python is able to download data. The way to do so is to check that after calling on the agent, see of your Map<String, String[]> variables are all non-null. If they're null, then your download of webpage using selenium is probably going haywire. 
 	
 	4. What if your TestMoDsAnalysis is not working? 
 	- Check if you have your Mods API jar file in the right folder. I have it in Users\\.m2\repository\com\cmclinnovations\mods\MoDS_Java_API\0.3.3, and if yours doesn't have the jar file but a lastUpdated file, then it means you tried to download it via Maven. But you can't. 
