##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 24 Nov 2020                      #
##########################################

"""This module has an execution entry point where an endpoint and a query are defined
to extract data and semantics from a SPARQL endpoint."""

import SparqlQuery as sq
import json

"""The following block of code runs first if this module is executed"""
if __name__ == '__main__':

    """Sets the URL of the triple store deployed on CoMo to query UK power plants"""
    endpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlant"

    """Sets the current SPARQL query"""
    query = """
    SELECT ?s ?p ?o
    WHERE
    {
    ?s ?p ?o .
    } LIMIT 10
    """
    results = sq.query_endpoint(endpoint, query)

    """Creates and opens a file to write the query output"""
    f = open("query-output.csv", "w")

    """Writes the column headers of the CSV file"""
    f.write("?s, ?p, ?o \n")

    print ('We print the retrieved information so that you can see what is returned by the query:')
    for result in results["results"]["bindings"]:
        print (result["s"]['value'], result["p"]['value'], result["o"]['value'])
        """Writes the values of the CSV file"""
        f.write(result["s"]['value'] + "," + result["p"]['value'] + "," + result["o"]['value'] + '\n')

    """Saves the file"""
    f.close()

    """Saves the query output also in JSON to provide an example so that you can learn"""
    with open('query-output.json', 'w') as outputfile:
        json.dump(results, outputfile)
