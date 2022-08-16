import os

_THIS_FILE = os.path.dirname(__file__)
_RES_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__),'..','..','resources','data'))

XML_TEMPLATE_SRC = os.path.join(_RES_DIR,'Processed','templates','hopv15_template.xml')
TXT_SRC = os.path.join(_RES_DIR,'Raw','HOPV_15_revised_2.data')
CSV_SRC = os.path.join(_RES_DIR,'Processed','csv_data','HOPV_15_revised_2_processed.csv')
XML_DIR = os.path.join(_RES_DIR,'Processed','xml_data')
OWL_DIR = os.path.join(_RES_DIR,'Processed','owl_data')
OWL_SRC_DIR = os.path.join(_RES_DIR,'Processed','owl_data','kb')

DEF_NAMESPACE = "ontomops"
#
LOCAL_KG = "http://localhost:8080/blazegraph"
LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'
#
REMOTE_KG = "http://theworldavatar.com/blazegraph"
REMOTE_KG_SPARQL = REMOTE_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'


