# UK flooding

### How to create a Python virtual environment and install required packages (in Windows):

1) Open `cmd` terminal and navigate into project repository

2) Create virtual environment `name` using venv (`python` might need to be replaced with `py` depending on whether Python is specified in system's `PATH` variable):
```
python -m venv <name>
```

3) Navigate into virtual environemnt repository, activate it, and navigate back:
```
cd <name>\Scripts
activate
cd ..\..
```

4) Install requirements listed in `requirements.txt`:
```
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```