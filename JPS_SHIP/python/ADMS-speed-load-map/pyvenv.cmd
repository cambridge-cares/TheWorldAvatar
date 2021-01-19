:;( #
  :;  python -m pip install pipenv #
  :;  python -m pipenv --rm #
  :;  mkdir .venv # 
  :;  python -m pipenv install --python 3.6 -r requirements.txt  #
:; );<<'Executing on Unix-like OS'
(
       python -m pip install pipenv
       python -m pipenv --rm
       mkdir .venv
       python -m pipenv install --python 3.6 --ignore-pipfile
) & rem ^
 Executing on Windows.