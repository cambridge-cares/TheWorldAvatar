@echo off
set op=%CD%
FOR /F "tokens=*" %%a IN ('where python') do (SET VAR=%%a)
cd %var:~0,-11%
pip install virtualenv
pip install virtualenvwrapper-win
cd %op%
python -m venv jps_ontomatch
.\jps_ontomatch\Scripts\activate & pip install -r requirements.txt
