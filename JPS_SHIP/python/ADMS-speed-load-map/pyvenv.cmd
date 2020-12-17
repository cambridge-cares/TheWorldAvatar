:;( #
  :;  pip install pipenv  #
  :;  rm Pipfile*  #
  :;  pipenv install --python 3.6 -r requirements.txt  #
:; );<<'Executing on Unix-like OS'
(
       pip install pipenv
       pipenv install --python 3.6 --ignore-pipfile
) & rem ^
 Executing on Windows.