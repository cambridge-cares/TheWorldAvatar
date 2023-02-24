# SensorLoggerMobileAppAGent

## To deploy this agent with the stack
1) Spin up the stack-manager
2) Input the necessary credentials in the folders

You'll need to provide  your credentials in single-word text files located like this:
```
./docker/
    credentials/
        repo_username.txt
        repo_password.txt
```
3) ./stack.sh build in this folder
4) Enter vscode debug mode (This will fail but will spin up the agent within the stack) but press again Reattach and debug
5) Edit the local address and send a POSTRequest to the the local port under resources

   POST http://<LOCAL_URL>/SensorLoggerMobileAppAgent/mb
