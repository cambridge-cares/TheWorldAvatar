0. In the Docker Desktop General Settings you need to enable the "Expose daemon on tcp://localhost:2375 without TLS" option.
1. Open this folder in VSCode.
2. Create a file called "postgis_password" with a password in it in the "inputs/secrets/" directory.
3. Add a breakpoint at the start of the "com.cmclinnovations.TempTestCalls::doStuff" method.
4. In the "Run and Debug" panel run the "Launch and attach Docker" configuration.
5. The Adminer and Ontop GUI endpoints should be avaliable at "http://localhost:8080/adminer/ui/" and "http://localhost:8080/ontop/sparql/" respectivly.