#!/bin/bash

if [[ "$OSTYPE" == "msys" ]]; 
then
	pip install pipenv;
	pipenv install --python 3.6 --ignore-pipfile;

else
	pip install pipenv;
	rm Pipfile*;
	pipenv install --python 3.6 -r requirements.txt;
fi

