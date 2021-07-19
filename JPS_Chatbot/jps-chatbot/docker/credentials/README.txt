As part of the Image build process, the chatbot currently required SSH access to
the Vienna server to download the pre-trained model files. To achieve this without
storing credentials within the Image itself (or within the Git repository), please
follow the below steps BEFORE attempting to build the Image.

Future developers should probably look into updating this system to support the 
use of SSH keys & passphrases.

NOTE: Once the Docker Image has been built once, Docker will cache the downloaded
model files; this means that simply rebuilding the Image does not guarantee you've 
downloaded the latest model files. To assure this, you'll have to delete your cached
Image before rebuilding or use the --no-cache argument.

1) Within this directory, create and populate the below files. Ensure that these files
are NEVER committed to the repository (a .gitignore file has been added to assure this).

	- 'host.txt'
		- This file should contain a single line with the full SSH address of the host, including your prepended user name.
		For example, "username@vienna-server.com"
		
	- 'password.txt'
		- This file should contain a single line with your SSH password.
		For example, "my-password-123"
		
		
2) Continue building the Docker Image as before (see README in ../docker).

4) If you don't plan on building the Image again soon, it's good practice to remove your host and password files.