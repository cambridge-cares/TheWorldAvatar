# Cross-domain representation of Chilean energy-related information

Last update: 4th of July 2024. 

        * Reproduced by Feroz on Tuesday 16 of January 2024 (from Dropbox). 
        * Reproduced by Andrea on 03 July 2024 from dropbox and GitHub. 
        * Reproduced again by Feroz on 05 July 2024 in a different machine.

## Prerequisites
You need to have Windows, WSL and Ubuntu, Visual Studio Code, Docker, Java 11 all installed in the computer, 
and perhaps Git, Tortoise, and TomCat. Good luck.

In the case of Linux, you need Visual Studio Code, Docker, Java 11 all installed in the computer, and perhaps Git, and TomCat. Good luck.

## Instructions

These instructions should allow to see a cross-domain representation of Chilean information.


### Step 1. Clone TheWorldAvatar, which is a GitHub repository available at https://github.com/cambridge-cares/TheWorldAvatar.git.
### Step 2. Populate following folders (the best way to do it is using Visual Studio Code to transfer the files in the correct folder):

* TheWorldAvatar/Deploy/stacks/dynamic/stack-data-uploader/inputs/config/
* TheWorldAvatar/Deploy/stacks/dynamic/stack-data-uploader/inputs/data/
* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/
* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/  (this one allows to access the geoserver with all the permissions)
* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/data/webspace/
* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/data/webspace/data/ (here must go all the icons, if you copy all the webspace folder, it is already included)

With the data files and folders found in the dropbox archive folder, which can be requested to the group.

With the configuration files found in GitHub at:

* TheWorldAvatar/Deploy/stacks/Chile/ (in the repository at GitHub)


The folders to populate will be found in TheWorldAvatar/Deploy/stacks/dynamic/

Go to the following folder:

* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/secrets/    (this is were you upload your passwords for geoserver, postgis and mapbox)

  
Transfer the following files to the secrets folder and replace the content accordingly:

* geoserver_password   (any password for admin)
* postgis_password     (any password for postgres, server <STACK_NAME>-postgis:5432 for example chileViv4-postgis:5432, system PostgreSQL)
* mapbox_api_key        (get a token at the mapbox website)
* mapbox_username       (get a username at the mapbox website)


### Step 3. Go to the folder:

* TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/data/webspace/

And execute:

     sed -i 's\<IP_ADDRESS>:<PORT_NUMBER>\<IP_ADDRESS>:<PORT_NUMBER>\g' *.*

For example:

     sed -i 's\<IP_ADDRESS>:<PORT_NUMBER>\127.0.0.1:3838\g' *.*



### Step 4. When the folders are populated, use Visual Studio Code to access the folder where TheWorldAvatar is cloned.


* Go to the folder:
  
        TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/

* Run the following command with the actual stack name and port number to spin up a stack:

         ./stack.sh start <STACK_NAME> <PORT_NUMBER>

For example:

        ./stack.sh start chileViv4 3838

The default name of the stack is chileViv4

### Step 5. Once all the containers are running (normally 7 or 8), then type:

        cd ..
        cd stack-data-uploader 
        ./stack.sh start <STACK_NAME>
        
For example:

        cd ..
        cd stack-data-uploader 
        ./stack.sh start chileViv4
  
(This should upload all the information. It might take 10-30 minutes. Be patient.)

### Step 6. If everything goes well, you should be able to see the visualisation at:
* http://<IP_ADDRESS>:<PORT_NUMBER>/visualisation/welcome.html   (forwards to website)
* http://<IP_ADDRESS>:<PORT_NUMBER>/visualisation/index_stack.html  (stack)
  
The geoserver at: 
* http://<IP_ADDRESS>:<PORT_NUMBER>/geoserver/index.html
* Access with: Your geoserver_password, username admin 

The adminer at:
* http://<IP_ADDRESS>:<PORT_NUMBER>/adminer/ui/
* Access with: Your postgis_password, username postgres, server <STACK_NAME>-postgis:5432 for example chileViv4-postgis:5432, system PostgreSQL

### Step 7. If you need to delete the stack before that:

        ./stack.sh rm <STACK_NAME> -v

For example:

        ./stack.sh rm chileViv4 -v

### Step 8. In the code folder you can find useful queries to be runned in the adminer. 

### Step 9. To activate all the options of Geoserver.
* In the following file:

  TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/geoserver

* Update the following line:

        "GEOSERVER_CSRF_WHITELIST=<IP_ADDRESS>"

For example:

        "GEOSERVER_CSRF_WHITELIST=127.0.0.1"


### Good luck!





