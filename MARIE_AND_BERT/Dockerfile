FROM tiangolo/uwsgi-nginx-flask:python3.8 as flask
ADD requirements_linux.txt .
RUN pip install -r requirements_linux.txt

# let chemcialdataextractor download the binaries

ADD http://www.theworldavatar.com/MARIE_DEPLOYMENT/cde_models.zip .
RUN mkdir -p /root/.local/share/ChemDataExtractor/
RUN unzip cde_models.zip -d /root/.local/share/ChemDataExtractor/
COPY ./Marie/ Marie


# copy the supporting data for Marie
# COPY ./DATA/Deployment DATA/Deployment
COPY ./DATA/Dictionaries DATA/Dictionaries
COPY ./DATA/Test DATA/Test
COPY static static
COPY templates templates
COPY ./main.py .
ADD http://www.theworldavatar.com/MARIE_DEPLOYMENT/Deployment.zip .
RUN unzip Deployment.zip -d ./DATA/Deployment
COPY ./uwsgi.ini .