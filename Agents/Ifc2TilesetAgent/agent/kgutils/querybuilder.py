"""
# Author: qhouyee #

This module provides functionality to build a SPARQL query or update.
"""

# Third party imports
from py4jps import agentlogging

# Self imports
from agent.exceptions.exceptions import QueryBuilderError

logger = agentlogging.get_logger('prod')


class QueryBuilder:
    """
    A class to build all types of SPARQL queries.

    Attributes:
        query_type: int
            Indicate which type of query to build. 0 is default SELECT query.
    """
    string = None
    prefix = None
    select_string = None
    where_string = None
    whitespace = " "

    def __init__(self, query_type: int = 0):
        if query_type == 0:
            self.prefix = ""
            self.select_string = "SELECT "
            self.where_string = "WHERE {"
        else:
            logger.error("Invalid query_type parameter! Valid parameter includes 0.")
            raise QueryBuilderError("Invalid query_type parameter! Valid parameter includes 0.")

    def __str__(self):
        return self.prefix + self.select_string + self.where_string + "}"

    def add_prefix(self, namespace: str, prefix: str = "base"):
        """
        This function adds the prefixes for SPARQL query.

        Arguments:
            namespace - uri namespace of ontology
            prefix - prefix reference for namespace (optional) defaults to base if omitted
        """
        if prefix in self.prefix:
            logger.error(prefix + " has already been added. Please input a new prefix.")
            raise QueryBuilderError(prefix + " has already been added. Please input a new prefix.")
        elif namespace in self.prefix:
            logger.error(namespace + " exists. Please input a new namespace.")
            raise QueryBuilderError(namespace + " exists. Please input a new namespace.")
        self.prefix += "PREFIX " + prefix + ":<" + namespace + ">\n"

        return self

    def add_select_var(self, *variable_vars: str):
        """
        This function adds the variable for the SELECT query.

        Arguments:
            *variable_vars - a variable number of SELECT variables to be added
                            Eg "subject". Do not add "?"
        """
        for var in variable_vars:
            self.select_string += "?" + var + self.whitespace

        return self

    def add_where_triple(self, subject: str, predicate: str, objectnode: str, var_ind: int = 0):
        """
        This function adds the triples to the WHERE query.
        Valid iri format include "prefix:class" or "<namespace#class>"

        Arguments:
            subject - subject node in the form "prefix:class" [iri] or "var" [variable]
            predicate - predicate node in the form "prefix:property" [iri] or "var" [variable]
            object - object node in the form "prefix:class" [iri] or "var" [variable]
            var_ind - an indicator determining which nodes are variables. 0 indicates no variable,\
                1 references subject, 2 references predicate, 3 references object,\
                4 references subject and predicate, 5 references subject and object,\
                6 references predicate and object, 7 references all as variables
        """
        if var_ind == 0:
            self.where_string += subject + self.whitespace
            self.where_string += predicate + self.whitespace + objectnode + ".\n"
        elif var_ind == 1:
            self.where_string += "?" + subject + self.whitespace
            self.where_string += predicate + self.whitespace + objectnode + ".\n"
        elif var_ind == 2:
            self.where_string += subject + self.whitespace
            self.where_string += "?" + predicate + self.whitespace + objectnode + ".\n"
        elif var_ind == 3:
            self.where_string += subject + self.whitespace
            self.where_string += predicate + self.whitespace + "?" + objectnode + ".\n"
        elif var_ind == 4:
            self.where_string += "?" + subject + self.whitespace
            self.where_string += "?" + predicate + self.whitespace + objectnode + ".\n"
        elif var_ind == 5:
            self.where_string += "?" + subject + self.whitespace
            self.where_string += predicate + self.whitespace + "?" + objectnode + ".\n"
        elif var_ind == 6:
            self.where_string += subject + self.whitespace
            self.where_string += "?" + predicate + self.whitespace + "?" + objectnode + ".\n"
        elif var_ind == 7:
            self.where_string += "?" + subject + self.whitespace
            self.where_string += "?" + predicate + self.whitespace + "?" + objectnode + ".\n"
        else:
            logger.error("Invalid var_index! Only 0 - 7 is valid.")
            raise QueryBuilderError("Invalid var_index! Only 0 - 7 is valid.")

        return self
    
    def add_optional_triple(self, subject: str, predicate: str, objectnode: str, var_ind: int = 0):
        """
        This function adds optional triples to the WHERE query.
        Valid iri format include "prefix:class" or "<namespace#class>"

        Arguments:
            subject - subject node in the form "prefix:class" [iri] or "var" [variable]
            predicate - predicate node in the form "prefix:property" [iri] or "var" [variable]
            object - object node in the form "prefix:class" [iri] or "var" [variable]
            var_ind - an indicator determining which nodes are variables. 0 indicates no variable,\
                1 references subject, 2 references predicate, 3 references object,\
                4 references subject and predicate, 5 references subject and object,\
                6 references predicate and object, 7 references all as variables
        """
        self.where_string += "OPTIONAL {"
        if var_ind == 0:
            self.where_string += subject + self.whitespace
            self.where_string += predicate + self.whitespace + objectnode
        elif var_ind == 1:
            self.where_string += "?" + subject + self.whitespace
            self.where_string += predicate + self.whitespace + objectnode
        elif var_ind == 2:
            self.where_string += subject + self.whitespace
            self.where_string += "?" + predicate + self.whitespace + objectnode
        elif var_ind == 3:
            self.where_string += subject + self.whitespace
            self.where_string += predicate + self.whitespace + "?" + objectnode
        elif var_ind == 4:
            self.where_string += "?" + subject + self.whitespace
            self.where_string += "?" + predicate + self.whitespace + objectnode
        elif var_ind == 5:
            self.where_string += "?" + subject + self.whitespace
            self.where_string += predicate + self.whitespace + "?" + objectnode
        elif var_ind == 6:
            self.where_string += subject + self.whitespace
            self.where_string += "?" + predicate + self.whitespace + "?" + objectnode
        elif var_ind == 7:
            self.where_string += "?" + subject + self.whitespace
            self.where_string += "?" + predicate + self.whitespace + "?" + objectnode
        else:
            logger.error("Invalid var_index! Only 0 - 7 is valid.")
            raise QueryBuilderError("Invalid var_index! Only 0 - 7 is valid.")
        self.where_string += ".}\n"
        return self

    def add_values_where(self, var: str, *values):
        """
        This function adds a VALUES clause to the WHERE query.
        Note only one VALUES clause can be added.

        Arguments:
            var - variable for filtering
            *values - a variable number of values to be added for filtering \
                if values are classes, please input as "prefix:class"
        """
        if "VALUES" in self.where_string:
            logger.warning("VALUES clause has already been defined!")
            raise QueryBuilderError("VALUES clause has already been defined!")
        # Add start of VALUES clause inside the WHERE clause
        self.where_string += "VALUES ?" + var + self.whitespace + "{"
        # Add each value to the string
        for value in values:
            # If value is a string and is not in prefix:iri format
            if isinstance(value, str) and ":" not in value:
                self.where_string += "'" + (value) + "'" + self.whitespace
            else:
                self.where_string += str(value) + self.whitespace
        # Close the VALUES clause
        self.where_string += "} \n"

        return self

    def build(self):
        return self.__str__()
