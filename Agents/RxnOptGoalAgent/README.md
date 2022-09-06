# Environment setup
Set up WSL2/VS Code/Docker, follow the steps [here](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment).

Install below recommended VS Code extensions:
 - Remote - WSL
 - Docker
 - Exetension Pack for Java
 - Python Extension Pack
 - Markdown All in One
 - Turtle Language Server

Get access to docker image registry, follow the steps [here](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry).

Install Java 11 in WSL2
```sh
$ sudo apt update
$ sudo apt install openjdk-11-jdk
```

Install anaconda in WSL2, follow the steps [here](https://gist.github.com/kauffmanes/5e74916617f9993bc3479f401dfec7da).

Set up conda environment for python packages
```sh
$ conda create --name <venv_name> python=3.8
$ conda activate <venv_name>
(<venv_name>) $ conda install -c conda-forge pythonnet
(<venv_name>) $ python -m pip install docker
(<venv_name>) $ python -m pip install docker-compose
(<venv_name>) $ python -m pip install -r requirements.txt 
```

# Authors #

Jiaru Bai (jb2197@cam.ac.uk)
