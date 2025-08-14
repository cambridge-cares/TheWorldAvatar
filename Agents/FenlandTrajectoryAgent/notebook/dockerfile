FROM jupyter/base-notebook

ENV DEBIAN_FRONTEND=noninteractive
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Install Java
USER root
RUN apt update && apt install -y openjdk-11-jdk-headless git

# Install python
USER ${NB_UID}
WORKDIR /opt/app
COPY requirements.txt /opt/app/
RUN python -m pip install --upgrade pip
RUN pip install --no-cache-dir -r requirements.txt

WORKDIR /home/jovyan

CMD ["start-notebook.sh", "--ip=0.0.0.0", "--NotebookApp.token=''"]