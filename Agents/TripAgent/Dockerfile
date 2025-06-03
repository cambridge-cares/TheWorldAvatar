FROM ghcr.io/theworldavatar/stack-client:1.47.0 AS stackclients
#==================================================================================================
FROM python:3.13.3-slim AS base
# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Set the default shell
RUN python -m pip install --upgrade pip && \
    python -m pip install gunicorn && \
    apt update &&\
    apt install -y openjdk-17-jre-headless git &&\
    rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

# Set the default working directory, then copy the Python source code into it
WORKDIR /app

# Install the required Python libraries
COPY ./requirements.txt .
RUN python -m pip install -r requirements.txt 

# Copy Stack-Client resource from published Docker image
COPY --from=stackclients /app ./tmp_stack
RUN stack_clients_jar=$(find ./tmp_stack/stack-clients*.jar) \
    && stack_clients_jar=${stack_clients_jar##*/} \
    && jpsrm uninstall StackClients \
    && jpsrm install StackClients ./tmp_stack --jar "$stack_clients_jar"

COPY ./agent ./agent

# Expose port
EXPOSE 5000
ENV FLASK_APP=agent/app.py

FROM base AS debug
RUN pip install debugpy

WORKDIR /app
CMD [ \
    "python", "-m", "debugpy", \
    "--listen", "0.0.0.0:5678", \
    "--wait-for-client", \
    "-m", "flask", "run", \
    "-h", "0.0.0.0", \
    "-p", "5000" \
    ]

FROM base AS production
#------------------------------------
# entry point setup
#------------------------------------
# Set the entrypoint
CMD ["gunicorn", "--bind", "0.0.0.0:5000", "--timeout", "600", "agent.app:app"]
#==================================================================================================