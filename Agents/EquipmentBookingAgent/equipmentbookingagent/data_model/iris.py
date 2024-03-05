from pyderivationagent.data_model.iris import *

RDFS_COMMENT = RDFS_BASE_URL + 'comment'
OWL = 'http://www.w3.org/2002/07/owl#'
OWL_VERSION = OWL + 'versionInfo'

FOAF = 'http://xmlns.com/foaf/0.1/'
FIBO = 'https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/'
OMG = 'https://www.omg.org/spec/Commons/Designators/'

###--- Common Base URL ---###
ONTOSPECIES = 'http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#'
ONTOLAB = 'https://www.theworldavatar.com/kg/ontolab/'
ONTOVAPOURTEC = 'https://www.theworldavatar.com/kg/ontovapourtec/'
ONTOHPLC = 'https://www.theworldavatar.com/kg/ontohplc/'
ONTOBPR = 'https://www.theworldavatar.com/kg/ontobpr/'
ONTOASSETMANAGEMENT = 'https://www.theworldavatar.com/kg/ontoassetmanagement/'
ONTOTECHNICALSYSTEM = 'https://www.theworldavatar.com/kg/ontotechnicalsystem/'
ONTODEVICE = 'https://www.theworldavatar.com/kg/ontodevice/'
ONTOBIM = 'https://www.theworldavatar.com/kg/ontobim/'


###--- Some IRIs from ONTOASSETMANAGEMENT ---###
OAM_BOOKINGSYSTEM = ONTOASSETMANAGEMENT + 'BookingSystem'
OAM_HASBOOKINGSYSTEM = ONTOASSETMANAGEMENT + 'hasBookingSystem'
OAM_BOOKING = ONTOASSETMANAGEMENT + 'BOOKING'
OAM_HASBOOKING = ONTOASSETMANAGEMENT + 'hasBOOKING'

###--- Some IRIs from DBPEDIA ---###
DBPEDIA_XLSFILE = 'http://dbpedia.org/resource/Microsoft_Excel' # NOTE: <DBPEDIA_XLSFILE> <rdf:type> <DBPEDIA_WIKICATFILENAMEEXTENSIONS>.
DBPEDIA_CSVFILE = 'http://dbpedia.org/resource/Comma-separated_values' # NOTE: <DBPEDIA_CSVFILE> <rdf:type> <DBPEDIA_WIKICATFILENAMEEXTENSIONS>.
DBPEDIA_TXTFILE = 'http://dbpedia.org/resource/Text_file' # NOTE: <DBPEDIA_TXTFILE> <rdf:type> <DBPEDIA_WIKICATFILENAMEEXTENSIONS>.
DBPEDIA_WIKICATFILENAMEEXTENSIONS = 'http://dbpedia.org/class/yago/WikicatFilenameExtensions'
DBPEDIA_MANUFACTURER = DBPEDIA + 'manufacturer'
DBPEDIA_ORGANISATION = DBPEDIA + 'Organisation'


###--- Some IRIs from SAREF ---###
SAREF_CONSISTSOF = SAREF + 'consistsOf'
SAREF_DEVICE = SAREF + 'Device'
SAREF_COMMAND = SAREF + 'Command'
SAREF_ACCOMPLISHES = SAREF + 'accomplishes'
SAREF_ACTSUPON = SAREF + 'actsUpon'
SAREF_HASCOMMAND = SAREF + 'hasCommand'
SAREF_HASFUNCTION = SAREF + 'hasFunction'
SAREF_HASSTATE = SAREF + 'hasState'
SAREF_ISACCOMPLISHEDBY = SAREF + 'isAccomplishedBy'
SAREF_ISCOMMANDOF = SAREF + 'isCommandOf'
SAREF_ISOFFEREDBY = SAREF + 'isOfferedBy'
SAREF_OFFERS = SAREF + 'offers'
SAREF_REPRESENTS = SAREF + 'represents'
SAREF_FUNCTION = SAREF + 'Function'
SAREF_TASK = SAREF + 'Task'
SAREF_SERVICE = SAREF + 'Service'
SAREF_STATE = SAREF + 'State'

###--- Some IRIs from OntoSpecies ---###
ONTOSPECIES_HASUNIQUESPECIES = ONTOSPECIES + 'hasUniqueSpecies'
ONTOSPECIES_HASMOLECULARWEIGHT = ONTOSPECIES + 'hasMolecularWeight'
ONTOSPECIES_UNITS = ONTOSPECIES + 'units'
ONTOSPECIES_VALUE = ONTOSPECIES + 'value'
ONTOSPECIES_SPECIES = ONTOSPECIES + 'Species'
ONTOSPECIES_GRAMPERMOLUNIT = 'http://www.theworldavatar.com/kg/ontospecies/Unit_a501ff0e-7cbc-4e3c-ba2e-211da22cac2b'
ONTOSPECIES_KILOGRAMPERMOLUNIT = 'http://www.theworldavatar.com/kg/ontospecies/Unit_671a1cb7-9962-4f9d-a0f7-ba5857383106'