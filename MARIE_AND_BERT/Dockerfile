FROM tiangolo/uwsgi-nginx-flask:python3.8 as flask
# copy the code for Marie 
COPY ./Marie/ Marie
# copy the supporting data for Marie
COPY ./DATA/ DATA
COPY ./main.py .
COPY requirements_linux.txt .
RUN pip install -r requirements_linux.txt
# let chemcialdataextractor download the binaries 
RUN cde data download








