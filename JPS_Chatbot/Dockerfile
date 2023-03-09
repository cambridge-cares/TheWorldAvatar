# First stage: download dependenies with Maven
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as dependency_fetcher

# Copy all files into root's home, including the dependencies pom file and ./m2 directory
ADD docker /root

# Populate Maven settings templates with credentials
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_URL|${repo_url}|g;s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Download Maven dependencies
WORKDIR /root/
RUN --mount=type=cache,target=/root/.m2/repository mvn -f pom.xml org.apache.maven.plugins:maven-dependency-plugin:unpack-dependencies

# Download geckodriver
RUN apt-get update && apt-get install wget -y
RUN wget -q https://github.com/mozilla/geckodriver/releases/download/v0.24.0/geckodriver-v0.24.0-linux64.tar.gz
RUN tar -xvzf geckodriver*.gz
#==================================================================================================

# Second stage: Build app, copying in dependency from first stage
#==================================================================================================
FROM python:3.7-slim-buster as app

WORKDIR /app

# Install packages required for mysql
RUN apt-get update \
 && apt-get install -y default-libmysqlclient-dev \
 && apt-get install -y default-mysql-server

# Copy in dependencies from first stage
COPY --from=dependency_fetcher /root/dependencies/models_wiki ./UI/source/Wikidata_Query
COPY --from=dependency_fetcher /root/dependencies/model_jps ./UI/source/JPS_Query
COPY --from=dependency_fetcher /root/dependencies/models_agent ./UI/source/Agent_Query
COPY --from=dependency_fetcher /root/geckodriver /usr/local/bin

# Setup venv
# (Prepending the venv location to 'PATH' is simpler and more robust than activating the venv)
ENV VENV_PATH=/app/venv
RUN python -m venv $VENV_PATH
ENV PATH="$VENV_PATH/bin:$PATH"

# Install requirements
COPY jps-chatbot/requirements.txt .
RUN pip install -r requirements.txt

# Download NLTK data
RUN python -c "import nltk; nltk.download('stopwords'); nltk.download('punkt')"

# Add all source code; might be able to copy less here
ADD jps-chatbot .

# Add top-level dir to the pythonpath so that gunicorn can find the modules.
# Should be replaced by writing a proper setup.py and installing the chatbot as a package.
ENV PYTHONPATH="/app"

ENTRYPOINT  ["gunicorn", "--bind", "0.0.0.0:5000", "UI:app"]

# #==================================================F================================================
# # Set the locale to UK 
# RUN apt-get -y install locales
# RUN sed -i '/en_GB.UTF-8/s/^# //g' /etc/locale.gen && \
#     locale-gen
# ENV LANG en_GB.UTF-8  
# ENV LANGUAGE en_GB:en  
# ENV LC_ALL en_GB.UTF-8  