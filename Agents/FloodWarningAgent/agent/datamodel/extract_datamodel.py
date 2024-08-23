# Purpose of this helper script is to extract the datamodel from a given .owl input file
# and create printed output which can be used to create the actual datamodel.py file

import requests

# URL to ontology .owl files
TBOX_URL = 'https://github.com/cambridge-cares/TheWorldAvatar/raw/main/JPS_Ontology/ontology/ontoflood/OntoFlood TBox.owl'

# Import ontologies
ontologies = {
    # CARES/COMO
    "FLOOD": "https://www.theworldavatar.com/kg/ontoflood/",
    "TS": "https://www.theworldavatar.com/kg/ontotimeseries/",
    # External
    "RDFS": "https://www.w3.org/2000/01/rdf-schema#",
    "TIME": "https://www.w3.org/2006/time#",
    "OM": "http://www.ontology-of-units-of-measure.org/resource/om-2/",
    "RT": "http://environment.data.gov.uk/flood-monitoring/def/core/",
    "ENVO": "http://purl.obolibrary.org/obo/",    
    "SOPH": "http://sweetontology.net/phen/",
    "SOPHHY": "http://sweetontology.net/phenHydro/"
}
# Initialise list of missing links
LINKS = []

def get_datamodel(prefix, ontology):
    """
    Extract the datamodel of the agent
    --> to be reviewed to create datamodel.py
    """

    # Initilaise list of extracted concepts
    concepts = []

    # Read online .owl file
    owl = requests.get(TBOX_URL).text
    for line in owl.splitlines():
         if line.strip().startswith("<!--") and line.strip().endswith("-->"):
              l = line.strip().replace("<!-- ", "").replace(" -->", "")
              if l == "":
                   continue               
              if l.startswith(ontology):
                   rest_link =  l.replace(ontology, "")
                   if "#" in rest_link:
                        name = rest_link.split("#")[1]
                   else:
                        name = rest_link
                   concepts.append("{}_{} = {} + '{}'".format(prefix, name.upper(), prefix, rest_link))
              else:
                   if l not in LINKS:
                        # check if the link is in ontologies values
                        included = False
                        for v in ontologies.values():
                             if l.startswith(v):
                                  included = True
                                  break
                        if not included:
                             LINKS.append(l)
    concepts.sort()
    print("\n".join(concepts))


# Print ontology definitions
for prefix, ontology in ontologies.items():
     print(f"{prefix} = '{ontology}'")
print('\n')

# Print definitions per ontology
for prefix, ontology in ontologies.items():
     print('### {} ###'.format(prefix))
     get_datamodel(prefix, ontology)
     print('')

# Print not yet included links
LINKS.sort()
print("Rest of the links: \n{}".format("\n".join(LINKS)),)    
