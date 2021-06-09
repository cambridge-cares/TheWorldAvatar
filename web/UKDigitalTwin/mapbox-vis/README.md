MapBox version of the Power System visualisation.

Once you're ready to run the HTML, you may get an error if you just double click on the html (F12 and see if there's an issue getting the file as it's not HTML, but a file through C:). 
If so open console, go to the folder the HTML and other files are in (cd FOLDERPATH/FOLDERNAME). 
Then: python -m http.server 
It should state the localhost port (eg. 8000), so then open a browser and go to localhost:8000 
Note that sometimes it's python3, not python. 
