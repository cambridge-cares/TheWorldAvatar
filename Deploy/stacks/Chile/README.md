# Cross-domain representation of Chilean energy-related information

UPDATED ON 29 February 2024. Reproduced by Feroz on Tuesday 16 of January, 2024. (from Dropbox)
Note: You need to have Windows, WSL and Ubuntu, Visual Studio Code, Docker, Java 11 all installed in the computer, 
and perhaps Git, Tortoise, and TomCat.

These instructions should allow to see a cross-domain representation of Chilean information.

Step 1. Create a clone of TWA.
Step 2. Populate the following folders with the files found in the corresponding folders at the following paths: 
https://www.dropbox.com/scl/fo/e54346qxk8juk9qm5u3cf/h?rlkey=85wdkgt2kaewaam5wsamx9ozm&dl=0 (here you will find the stack-data-uploader data files)
https://www.dropbox.com/scl/fo/sgitoeiwa4wiisgmhorwx/h?rlkey=4jx5078qb3zhxox6daotxmq09&dl=0 (here you will find the stack-manager data files)
TheWorldAvatar/Deploy/stacks/Chile/ (in the repository at GitHub)
(Files are also in: https://www.dropbox.com/work/IRP3%20CAPRICORN%20shared%20folder/_TWA_Shared_Data/Chile)

The folders to populate will be found in TheWorldAvatar/Deploy/stacks/dynamic/
You must populate:
TheWorldAvatar/Deploy/stacks/dynamic/stack-data-uploader/inputs/config/
TheWorldAvatar/Deploy/stacks/dynamic/stack-data-uploader/inputs/data/
TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/
TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/  (this one allows to access the geoserver with all the permissions)
TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/data/webspace/
TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/data/webspace/data/ (here must go all the icons)
TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/secrets/   (this is were you upload your passwords for geoserver, postgis and mapbox)


Details:
TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/secrets/ 
File names:
geoserver_password   (any password for admin)
postgis_password     (any password for postgres, server chileVi-postgis:5432, system PostgreSQL)
mapbox_api_key        (get a token at the mapbox website)
mapbox_username       (get a username at the mapbox website)


Step 3. When the folders are populated, use Visual Studio Code to access the folder where TWA is cloned.
Go to the folder: TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/
Type in the terminal: ./stack.sh start chileViv4 3851
Step 4. Once all the containers are running (normally 7 or 8), then type:
cd ..
cd stack-data-uploader 
./stack.sh start chileViv4
(This should upload all the information. It might take 10-30 minutes. Be patient.)

Step 5. If everything goes well, you should be able to see the visualisation at:
http://YOUR-IP:3851/visualisation/welcome.html

The geoserver at: 
http://YOUR-IP:3851/geoserver/index.html
Access with: Your geoserver_password, username admin 

The adminer at:
http://YOUR-IP:3851/adminer/ui/
Access with: Your postgis_password, username postgres, server chileVi-postgis:5432, system PostgreSQL

Step 6. If the visualisation is not working, try changing 
146.190.86.59:3851 by your new IP like this: YOUR-IP:3851 
in all the json and html files in the folder:
TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/data/webspace/

Step 7. In the code folder you can find useful queries to be runned in the adminer. 

Step 8. To activate all the options of Geoserver, then update the following line:
  "GEOSERVER_CSRF_WHITELIST=146.190.86.59"
In the following file:
TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/geoserver

GOOD LUCK!





