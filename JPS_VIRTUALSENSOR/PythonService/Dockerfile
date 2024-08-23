# Install the stdc app
#==================================================================================================
FROM python:3.6.8 as base
# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Set the default shell
SHELL ["/bin/bash", "-c"]
RUN python -m pip install --upgrade pip
RUN python -m pip install gunicorn

# Set the default working directory, then copy the Python source code into it
WORKDIR /app

COPY ./requirements.txt /app/.
# Install the required Python libraries
RUN python -m pip install -r requirements.txt

COPY ./speedLoadMap /app/speedLoadMap
COPY ./aermod /app/aermod
COPY ./app_entry_point.sh /app/.
COPY ./app.py /app/.

# Expose port
EXPOSE 5000
ENV FLASK_APP=app.py

FROM base as debug
RUN pip install debugpy --trusted-host pypi.org --trusted-host files.pythonhosted.org

WORKDIR /app
CMD python -m debugpy --listen 0.0.0.0:5678 --wait-for-client -m flask run -h 0.0.0.0 -p 5000

FROM base as production
#------------------------------------
# entry point setup
#------------------------------------
# Set the entrypoint
ENTRYPOINT /app/app_entry_point.sh
#==================================================================================================