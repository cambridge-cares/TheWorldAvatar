FROM tiangolo/uwsgi-nginx-flask:python3.8 as flask
ENV UWSGI_CHEAPER 0
ENV UWSGI_PROCESSES 1
WORKDIR /app

COPY Marie/ ./Marie
COPY uwsgi.ini ./uwsgi.ini
COPY static/ ./static
COPY templates/ ./templates
COPY main.py ./main.py
COPY requirements_linux.txt ./requirements_linux.txt
COPY DATA/ ./DATA


#COPY requirements_linux.txt requirements_linux.txt
RUN apt-get update && \
    apt-get install -y openjdk-11-jdk && \
    apt-get install -y ant && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/ && \
    rm -rf /var/cache/oracle-jdk8-installer
ENV JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64/
RUN export JAVA_HOME
RUN pip --default-timeout=10000 install --no-cache-dir -r requirements_linux.txt
RUN echo "uwsgi_read_timeout 300s;" > /etc/nginx/conf.d/custom_timeout.conf
RUN echo "uwsgi_connect_timeout 300s;" > /etc/nginx/conf.d/custom_timeout.conf
RUN echo "uwsgi_send_timeout 300s;" > /etc/nginx/conf.d/custom_timeout.conf
#RUN chmod +x ./script.sh
#CMD [ "/bin/bash","-c","./script.sh"]