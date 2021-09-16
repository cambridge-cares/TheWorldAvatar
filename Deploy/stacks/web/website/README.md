The folder contains the required files to build a Docker image for the KG Website. The "Dockerfile"
file contains the instructions to build an image; before making any changes to it, please consult
the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, 
Owen Parry <oparry@cmclinnovations.com>).


Please note the caveats below before continuing:

	- The site hosted within the Docker image will be based on the current commit 
	of this repository, please ensure you're on the right one.

	- The "docker build" command should be run from within the website directory. 
	
	- Local changes to the site's files will not be reflected on the website until the image is rebuilt.
	

To build the image:
	docker build --rm --target dev -t docker.cmclinnovations.com/website:1.0.0-SNAPSHOT-dev .
	
To run the image and generate a container:
	docker run -d --restart always --network dev-web --name "website" -it docker.cmclinnovations.com/website:1.0.0-SNAPSHOT-dev


===== Development ======
	
All website files are stored within the Deploy/web/website/site directory within the JPS repository; please refer to the Grav documentation for more information on the file hierarchy. We've made a design choice to follow the below procedure for KG service development and the website:

- KG services should be run within their own Docker container, no services should be included within the website container.
- KG services should contain minimal UI elements within their own code:
	- It's the intention that KG services will be embedded within/called from pages on the website wherever possible; this should ensure we have a consistent style for all services. This means that if you create your KG service with existing UI content, it may no longer be possible for use in the overall website. If required, individual UIs can be created for local testing/development, but a version should exist without them; we've tried to mitigate this need by making it easy to host a version of the KG website, so you can develop and test with that instead of a bespoke UI for that service.
	- For example, to support the integration of the UK Digital Twin on the site, the header banner was removed so that the service itself only shows the map element. This map element can then be loaded into an existing web page (that already has it's own header and footer) using an iframe.
- Each KG service/demo should have it's own individual page:
	- Whilst we can link to many services from a single page, each service demo should be within it's own page.
- Each KG service should list the development partners:
	- For now this is most likely CMCL, CoMo, and CARES. Using the prebuild partners page module should do.
- Any additional entries in the CSS files should be commented, as should any JS files added to the site.

Once you've made the desired changes to the website on your local clone of the repository, these changes can be seen on the hosted site by rebuilding the website image. This can either be done for the website on it's own using the individual docker commands shown later, or you can use the refresh-stack.sh script in Bash to reload the entire environment (see the README files within the repository for more details).