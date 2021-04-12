:;( #
  :;  python -m pip install pipenv #
  :;  python -m pipenv --rm #
  :;  mkdir .venv # 
  :;  python -m pipenv install --python 3.6 -r requirements_unix.txt  #
  :;  mkdir ~/Sensor_venv #
  :;  cp .venv ~/Sensor_venv -r #
:; );<<'Executing on Unix-like OS'
(
	   del Pipfile.lock
	   del Pipfile
	   python -m pip install --upgrade pip
	   python -m pip install pipenv
       python -m pipenv --rm
	   mkdir .venv
       python -m pipenv install --python 3.6 --ignore-pipfile -r requirements_windows.txt
	   mkdir "%systemdrive%%homepath%\Sensor_venv"
	   xcopy ".venv" "%systemdrive%%homepath%\Sensor_venv" /s /Y >NUL
) & rem ^
 Executing on Windows.