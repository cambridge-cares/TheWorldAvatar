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
OAM_BOOKING = ONTOASSETMANAGEMENT + 'Booking'
OAM_HASBOOKING = ONTOASSETMANAGEMENT + 'hasBooking'
OAM_HASBOOKER = ONTOASSETMANAGEMENT + 'hasBooker'
OAM_HASBOOKINGPERIOD = ONTOASSETMANAGEMENT + 'hasBookingPeriod'

###--- Some IRIs from FIBO ---###
FIBO_PERSON = FIBO + 'Person'

###--- Some IRIs from TIME ---###
TIME_INSTANT = W3C_TIME + 'Instant'
TIME_INTERVAL = W3C_TIME + 'Interval'
TIME_TIMEPOSITION = W3C_TIME + 'TimePosition'
TIME_HASBEGINNING = W3C_TIME + 'hasBeginning'
TIME_HASEND = W3C_TIME + 'hasEnd'

###--- Some IRIs from DBPEDIA ---#
DBPEDIA_UNIX = 'http://dbpedia.org/resource/Unix_time'