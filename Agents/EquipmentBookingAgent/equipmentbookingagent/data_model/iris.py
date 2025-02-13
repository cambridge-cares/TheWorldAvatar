from pyderivationagent.data_model.iris import *

OWL = 'http://www.w3.org/2002/07/owl#'
FOAF = 'http://xmlns.com/foaf/0.1/'
FIBO_P = 'https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/'
FIBO_O = 'https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/FormalOrganizations/'
OMG_D = 'https://www.omg.org/spec/Commons/Designators/'
OMG_R = 'https://www.omg.org/spec/Commons/RolesAndCompositions/'

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
BOT = 'https://w3id.org/bot#'

###--- Some IRIs from ONTOASSETMANAGEMENT ---###
OAM_BOOKINGSYSTEM = ONTOASSETMANAGEMENT + 'BookingSystem'
OAM_HASBOOKINGSYSTEM = ONTOASSETMANAGEMENT + 'hasBookingSystem'
OAM_BOOKING = ONTOASSETMANAGEMENT + 'Booking'
OAM_HASBOOKING = ONTOASSETMANAGEMENT + 'hasBooking'
OAM_HASBOOKER = ONTOASSETMANAGEMENT + 'hasBooker'
OAM_HASBOOKINGPERIOD = ONTOASSETMANAGEMENT + 'hasBookingPeriod'
OAM_ISMANUFACTUREDBY = ONTOASSETMANAGEMENT + 'isManufacturedBy'
OAM_ISSUPPLIEDBY = ONTOASSETMANAGEMENT + 'isSuppliedBy'
OAM_HASITEMINVENTORYIDENTIFIER = ONTOASSETMANAGEMENT + 'hasItemInventoryIdentifier'
OAM_ASSIGNEDTO = ONTOASSETMANAGEMENT + 'assignedTo'

###--- Some IRIs from TIME ---###
TIME_INSTANT = W3C_TIME + 'Instant'
TIME_INTERVAL = W3C_TIME + 'Interval'
TIME_TIMEPOSITION = W3C_TIME + 'TimePosition'
TIME_HASBEGINNING = W3C_TIME + 'hasBeginning'
TIME_HASEND = W3C_TIME + 'hasEnd'

###--- Some IRIs from FIBO ---###
FIBO_PERSON = FIBO_P + 'Person'
FIBO_PERSONNAME = FIBO_P + 'PersonName'
FIBO_ORGANIZATION = FIBO_O + 'FormalOrganization'

###--- Some IRIs from OMG ---###
OMG_HASNAME = OMG_D + 'hasName'
OMG_PLAYSROLE = OMG_R + 'playsRole'
OMG_FUNCTIONALROLE = OMG_R + 'FunctionalRole'

###--- Some other IRIs ---#
DBPEDIA_UNIX = 'http://dbpedia.org/resource/Unix_time'
BOT_CONTAINSELEMENT = BOT + 'containsElement'
ONTOCAPE_TECHNICALSYSTEM = 'http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#TechnicalSystem'
ONTOBIM_ROOM = ONTOBIM + 'Room'
OTS_COMPOSEDOF = ONTOTECHNICALSYSTEM + 'composedOf'
ONTOBIM_HASIFCREPRESENTATION = ONTOBIM + 'hasIfcRepresentation'