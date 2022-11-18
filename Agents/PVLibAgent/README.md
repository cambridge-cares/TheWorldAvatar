# Example: Python agent

## What this image is for

This example demonstrates how Docker can be used to deploy a Python app behind a [gunicorn](https://gunicorn.org/) server.
It's intended as a template to turn Python code into an agent.

## How it works

The Dockerfile includes three key components:

1. A light-weight base image with Python preinstalled
2. A directive to install library dependencies using pip
3. A directive to execute a gunicorn server to handle HTTP requests

Spinning up a container from the image sets up a simple agent that evaluates the polynomial:
```
f(x) = 3x^2 - x + 2
````
The values of *x*, and optionally a maximum order, *n*, are passed to the agent as parameters in an http request.
The result is returned in json format.


## Using the template

To build and start the agent, you need to spin up a container from the image.
In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run

```
docker-compose up -d
```

The agent is reachable on [localhost port 55002]("http://localhost:55002") by default (you can change this in docker-compose.yml).
To see instructions for making a call to the API, open your browser to `"http://localhost:55002"`.
In VSCode, you can right-click on the container in the Docker extension and choose "Open in Browser".

An example query is
```
http://localhost:55002/api/v1/evaluate?val=3.25&order=2
```
which returns the result in json format
```
{"result":30.4375}
```

## Adapting the template

To convert your own code into an agent using this template:

* Make a copy of this directory.
* Preserving `app.py`, delete the rest of the contents of the python_agent sub-directory and replace it with your own code.
* Modify `app.py` to add the required routes, removing references to the 'poly_model' module and adding references to your own modules/packages.
* Replace requirements.txt with your own list of dependencies.

You'll almost certainly want to change the name of the "python_agent" sub-directory to that of your own module or package.
In doing so, you'll also need to change references to that directory in the Dockerfile, including the location of the Flask application object (the last argument to the gunicorn server in the entrypoint).