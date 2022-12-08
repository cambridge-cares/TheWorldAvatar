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

ADD http://www.theworldavatar.com/MARIE_DEPLOYMENT/DATA.zip .
RUN unzip DATA.zip -d ./DATA

# COPY ./DATA/Dictionaries DATA/Dictionaries
# COPY ./DATA/Test DATA/Test
COPY static static
COPY templates templates
COPY ./main.py .
ADD http://www.theworldavatar.com/MARIE_DEPLOYMENT/CrossGraph.zip .
RUN unzip CrossGraph.zip -d ./DATA/CrossGraph
ADD http://www.theworldavatar.com/MARIE_DEPLOYMENT/Dictionaries.zip .
RUN unzip Dictionaries.zip -d ./DATA/Dictionaries
COPY ./uwsgi.ini .