git clone --branch dev-marie-and-bert https://github.com/cambridge-cares/TheWorldAvatar
mkdir /app/DATA


wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/CrossGraph.zip && unzip CrossGraph.zip -d /app/DATA/ && rm CrossGraph.zip
wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/Evaluation.zip && unzip Evaluation.zip -d /app/DATA/ && rm Evaluation.zip
wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/EntityLinking.zip && unzip EntityLinking.zip -d /app/DATA/ && rm EntityLinking.zip
wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/bert_pretrained.zip && unzip bert_pretrained.zip -d /app/ && rm bert_pretrained.zip

wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/Dictionaries.zip && unzip Dictionaries.zip -d /app/DATA/Dictionaries && rm Dictionaries.zip
wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/cde_models.zip
RUN mkdir -p /root/.local/share/ChemDataExtractor/
RUN unzip cde_models.zip -d /root/.local/share/ChemDataExtractor/
rm cde_models.zip

sh /start.sh