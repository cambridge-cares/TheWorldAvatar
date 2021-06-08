# This Dockerfile has been adapted from the VS Code example at https://code.visualstudio.com/docs/containers/quickstart-python

# Base image is a lightweight version of Python
FROM python:3.8-slim-buster

# Expose the port on which our server will run
EXPOSE 5000

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install the required Python libraries
COPY requirements.txt .
RUN python -m pip install -r requirements.txt

# Set the default working directory, then copy the Python source code into it
WORKDIR /app
ADD python_agent /app/python_agent

# Switch to a non-root user before running the server, for security reasons
# (See https://code.visualstudio.com/docs/containers/python-user-rights)
RUN useradd appuser && chown -R appuser /app
USER appuser

# Start the gunicorn server on port 5000, using a Flask object called 'app' imported from the 'python_agent' module
CMD ["gunicorn", "--bind", "0.0.0.0:5000", "python_agent:app"]