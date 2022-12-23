FROM tiangolo/uwsgi-nginx-flask:python3.8 as flask

# COPY ./Marie/ Marie
RUN git clone --branch dev-marie-and-bert https://github.com/cambridge-cares/TheWorldAvatar
RUN cp -r TheWorldAvatar/MARIE_AND_BERT/Marie ./Marie
RUN cp -r TheWorldAvatar/MARIE_AND_BERT/uwsgi.ini ./uwsgi.ini
RUN cp -r TheWorldAvatar/MARIE_AND_BERT/static ./static
RUN cp -r TheWorldAvatar/MARIE_AND_BERT/templates ./templates
RUN cp -r TheWorldAvatar/MARIE_AND_BERT/main.py ./main.py
RUN cp -r TheWorldAvatar/MARIE_AND_BERT/requirements_linux.txt ./requirements_linux.txt
RUN pip install --no-cache-dir -r requirements_linux.txt
RUN mkdir ./DATA
RUN mkdir ./DATA/CrossGraph
ADD http://www.theworldavatar.com/MARIE_DEPLOYMENT/cross_graph_model_new ./DATA/CrossGraph
RUN mkdir ./DATA/CrossGraph/pubchem
RUN mkdir ./DATA/CrossGraph/ontokin
RUN mkdir ./DATA/CrossGraph/ontospecies
RUN mkdir ./DATA/CrossGraph/ontocompchem

RUN wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/pubchem.zip && unzip pubchem.zip -d ./DATA/CrossGraph/pubchem && rm pubchem.zip
RUN wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/ontokin.zip && unzip ontokin.zip -d ./DATA/CrossGraph/ontokin && rm ontokin.zip
RUN wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/ontospecies.zip && unzip ontospecies.zip -d ./DATA/CrossGraph/ontospecies && rm ontospecies.zip
RUN wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/ontocompchem.zip && unzip ontocompchem.zip -d ./DATA/CrossGraph/ontocompchem && rm ontocompchem.zip

ADD http://www.theworldavatar.com/MARIE_DEPLOYMENT/Dictionaries.zip .
RUN unzip Dictionaries.zip -d ./DATA/Dictionaries
ADD http://www.theworldavatar.com/MARIE_DEPLOYMENT/cde_models.zip .
RUN mkdir -p /root/.local/share/ChemDataExtractor/
RUN unzip cde_models.zip -d /root/.local/share/ChemDataExtractor/
