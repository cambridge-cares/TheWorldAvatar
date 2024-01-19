# Nginx Reverse Proxy

The folder contains the required files to build a Docker image for the Nginx reverse proxy. This handles redirects, certifications, and authentications for all web-accessed container across the Docker stacks. The "Dockerfile" file contains the instructions to build the image; before making any changes to it, please consult the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

Please note the caveats below before continuing:
	- The site hosted within the Docker image will be based on the current commit of this repository, please ensure you're on the right one.
	- Local changes to the proxy's files will not be reflected until the image is rebuilt.
	- The provided scripts and stack configurations should be used to build and run the Image.
	

### Prerequisites

Certain URL paths redirect to containers that need to have a measure of authentication (namely those used for the CReDo project). As such the build process can create a htpasswd file to generate a 'credo-user' account using an input password. To set this password, create a plain-text `credentials/password-file.txt` file containing the password. The presence of this file should be detected at built time and the `setup-credo.sh` script will be run to generate the associated account. The Nginx server has already been configured to only allow access to certain URLs if the account details are used.