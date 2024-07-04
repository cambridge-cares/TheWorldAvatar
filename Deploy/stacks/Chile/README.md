# Cross-domain representation of Chilean energy-related information

Last update: 4th of July 2024. 

        * Reproduced by Feroz on Tuesday 16 of January 2024 (from Dropbox). 
        * Reproduced by Andrea on 03 July 2024 from dropbox and GitHub. 
        * Reproduced again by Feroz on 05 July 2024 in a different machine.

## Prerequisites
You need to have Windows, WSL and Ubuntu, Visual Studio Code, Docker, Java 11 all installed in the computer, 
and perhaps Git, Tortoise, and TomCat. Good luck.

## Instructions

These instructions should allow to see a cross-domain representation of Chilean information.


### Step 1. Create a clone of TWA.
### Step 2. Populate following folders (the best way to do it is using Visual Studio Code to transfer the files in the correct folder):

* TheWorldAvatar/Deploy/stacks/dynamic/stack-data-uploader/inputs/config/
* TheWorldAvatar/Deploy/stacks/dynamic/stack-data-uploader/inputs/data/
* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/
* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/  (this one allows to access the geoserver with all the permissions)
* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/data/webspace/
* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/data/webspace/data/ (here must go all the icons)
* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/secrets/   (this is were you upload your passwords for geoserver, postgis and mapbox)

With the data files found in the dropbox archive folder, which can be requested to the group.
With the configuration files found in GitHub at:

* TheWorldAvatar/Deploy/stacks/Chile/ (in the repository at GitHub)


The folders to populate will be found in TheWorldAvatar/Deploy/stacks/dynamic/

Details:

* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/secrets/

  
File names:

* geoserver_password   (any password for admin)
* postgis_password     (any password for postgres, server chileVi-postgis:5432, system PostgreSQL)
* mapbox_api_key        (get a token at the mapbox website)
* mapbox_username       (get a username at the mapbox website)


### Step 3. When the folders are populated, use Visual Studio Code to access the folder where TWA is cloned.

* Go to the folder:
  
        TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/
* Type in the terminal:

        ./stack.sh start chileViv4 3851

### Step 4. Once all the containers are running (normally 7 or 8), then type:

        cd ..
        cd stack-data-uploader 
        ./stack.sh start chileViv4
  
(This should upload all the information. It might take 10-30 minutes. Be patient.)

### Step 5. If everything goes well, you should be able to see the visualisation at:
* http://YOUR-IP:3851/visualisation/welcome.html   (forwards to website)
* http://YOUR-IP:3851/visualisation/index_stack.html  (stack)
  
The geoserver at: 
* http://YOUR-IP:3851/geoserver/index.html
* Access with: Your geoserver_password, username admin 

The adminer at:
* http://YOUR-IP:3851/adminer/ui/
* Access with: Your postgis_password, username postgres, server chileVi-postgis:5432, system PostgreSQL

### Step 6. If the visualisation is not working, try changing 
146.190.86.59:3851 by your new IP like this: YOUR-IP:YOUR-PORT 
in all the json and html files in the folder:

TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/data/webspace/

And then run the stach again:
* Go to the folder: TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/
* Type in the terminal:
  
        ./stack.sh start chileViv4 3851

If you need to delete the stack before that:

        ./stack.sh rm chileViv4 -v

### Step 7. In the code folder you can find useful queries to be runned in the adminer. 

### Step 8. To activate all the options of Geoserver.
* In the following file:

  TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/geoserver

* Update the following line:

        "GEOSERVER_CSRF_WHITELIST=146.190.86.59"


### Good luck!





