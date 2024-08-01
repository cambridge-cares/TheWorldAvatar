# assume the user has nodejs and npm installed
# install the client-end of comunica

npm install @comunica/actor-init-sparql
node wiki_retrieve_species.js # get the URI and SMILES of species

# create a list containing all URIs of species
python CREATE_FULL_URI_LIST.python


echo 'start to retrieve species from wiki'
pause

