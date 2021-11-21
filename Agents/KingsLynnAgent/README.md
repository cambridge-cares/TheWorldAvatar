# UK flooding

## Preparation
### 1. Create a Python virtual environment and install required packages (in Windows):

1) Open `cmd` terminal and navigate into project's root repository (referred to as `<root>` in the following)

2) Create virtual environment `<venv_name>` using venv (`python` command might need to be replaced with `py` depending on whether Python is specified in system's `PATH` variables):
```
python -m venv <venv_name>
```

3) Activate virtual environment by running:
```
<venv_name>\Scripts\activate
```

4) Install requirements listed in `requirements.txt`:
```
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```