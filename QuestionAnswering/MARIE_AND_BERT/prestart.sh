mkdir  -p /app/DATA
mkdir -p /root/.local/share/ChemDataExtractor/

if [ -d /app/DATA/CrossGraph ];
then
  echo "File already exists"
else
  wget -nc http://159.223.42.53:8080/CrossGraph.zip && unzip CrossGraph.zip -d /app/DATA/ && rm CrossGraph.zip
  wget -nc http://159.223.42.53:8080/Evaluation.zip && unzip Evaluation.zip -d /app/DATA/ && rm Evaluation.zip
  wget -nc http://159.223.42.53:8080/EntityLinking.zip && unzip EntityLinking.zip -d /app/DATA/ && rm EntityLinking.zip
  wget -nc http://159.223.42.53:8080/bert_pretrained.zip && unzip bert_pretrained.zip -d /app/ && rm bert_pretrained.zip
  wget -nc http://159.223.42.53:8080/Dictionaries.zip && unzip Dictionaries.zip -d /app/DATA/ && rm Dictionaries.zip
  wget -nc http://159.223.42.53:8080/label_dict.js /app/static/js/
  wget -nc http://159.223.42.53:8080/cde_models.zip && unzip cde_models.zip -d /root/.local/share/ChemDataExtractor/ && rm cde_models.zip
fi

if [ -d /root/.data/STOUT-V2/models ];
then
  echo "File already exists"
else
  mkdir  -p /root/.data/STOUT-V2/
  wget -nc http://159.223.42.53:8080/models.zip && unzip models.zip -d /root/.data/STOUT-V2/ && rm models.zip
fi

#wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/CrossGraph.zip && unzip CrossGraph.zip -d /app/DATA/ && rm CrossGraph.zip
#wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/Evaluation.zip && unzip Evaluation.zip -d /app/DATA/ && rm Evaluation.zip
#wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/EntityLinking.zip && unzip EntityLinking.zip -d /app/DATA/ && rm EntityLinking.zip
#wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/bert_pretrained.zip && unzip bert_pretrained.zip -d /app/ && rm bert_pretrained.zip
#wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/Dictionaries.zip && unzip Dictionaries.zip -d /app/DATA/ && rm Dictionaries.zip
#wget http://www.theworldavatar.com/MARIE_DEPLOYMENT/cde_models.zip


pip --default-timeout=10000 install --no-cache-dir -r requirements_linux.txt