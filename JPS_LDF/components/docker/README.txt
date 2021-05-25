This docker image is for deploying the LDF server for fast query.
Created by Xiaochi Zhou (xz378@cam.ac.uk)

## docker create image 
1. put your vienna username in credential/host.txt 
2. put your vienna password in creadential/password.txt 
3. docker build -t <image_name>:<image_tag> -f docker/Dockerfile .

## run the docker container 
0. make sure that port 3000 is available on the server 
1. docker container run -dit -p 3000:3000 <image_name>:<image_tag>
2. request localhost:3000, you should see "Cannot GET /"
3. run the python script JPS_Chatbot\UI\source\rasa_jps\JPS_query_constructor_test.py, the request should take around 60 seconds and return a reaction 
4. run the same python script again, it should take around 3 seconds and return the same result 



