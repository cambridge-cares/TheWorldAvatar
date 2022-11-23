###############################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)  #
# Date: 11 Okt 2022                           #
###############################################
# The purpose of this file is to provide a    #
# set of functions to create SPARQL updates   #
# for the instantiation of the DH ontology    #
###############################################

from forecasting.datamodel.iris import *
from forecasting.utils.properties import *
from darts import concatenate
from darts.utils.timeseries_generation import datetime_attribute_timeseries as dt_attr

import numpy as np
from darts import TimeSeries
from darts.dataprocessing.transformers import Scaler


def get_data_cov(df, col):
    """
    It takes a dataframe and a column name as input, and returns a scaled time series
    
    :param df: the dataframe containing the data
    :param col: the column name of the dataframe that you want to use
    :return: A time series object
    """

    cov = TimeSeries.from_dataframe(df, time_col='Date', value_cols=col)
    scaler_cov = Scaler()
    cov_scaled = scaler_cov.fit_transform(cov)
    return cov_scaled

def get_time_cov(df, cov:dict):
    """
    It takes a df and returns a covariate matrix with the following columns:
    
    - day of year (cyclic)
    - day of week (cyclic)
    - hour of day (cyclic)
    
    The `cyclic` keyword argument is used to indicate that the covariate is cyclic. This is important
    for the model to learn the correct periodicity
    
    :param series: the time series to be modeled
    :return: A covariate matrix with the following columns:
        - day of year (cyclic)
        - day of week (cyclic)
        - hour of day (cyclic)
    """

    series = TimeSeries.from_dataframe(df, time_col = 'Date' ) #, fill_missing_dates =True

    covs = concatenate(
        [
            dt_attr(series.time_index, k, dtype=np.float32, cyclic=v) for k,v in cov.items()

        ],
        axis="component",
    )
    return covs
def add_insert_data(update):
    """
    It takes a string of SPARQL update statements and returns a string of SPARQL update statements that
    will insert the data described by the original string

    :param update: the update query
    :return: the string "INSERT DATA\n{\n" + update + "}"
    """

    return "INSERT DATA\n{\n" + update + "}"


def create_sparql_prefix(iri):
    """
        Constructs proper SPARQL Prefix String from given IRI
        Arguments:
               iri: IRI of the prefix
        Returns:
            SPARQL update prefix string in the form "PREFIX ns: <full IRI>".
    """

    if not iri.startswith('<'):
        iri = '<' + iri
    if not iri.endswith('>'):
        iri = iri + '>'

    return 'PREFIX ' + iri.__name__ + ': ' + iri + ' '


def add_enclosing(s):
    """
         Adds enclosing < > to a string if it does not have it already.
         Arguments:
              s: string
         Returns:
              string with enclosing < >
    """
    if not isinstance(s, str):
        s = str(s)
    if not s.startswith('<'):
        s = '<' + s
    if not s.endswith('>'):
        s = s + '>'
    return s


def create_triple(subject, predicate, obj):
    """
         Constructs object properties from given subject, predicate and object.
         Literals are handled by create_triple_literal
         Arguments:
              subject: subject of the triple
              predicate: predicate of the triple
              obj: object of the triple
         Returns:
              SPARQL triple string in the form "<subject> <predicate> <object>."
    """

    # typechecking
    # if varibles do not have enclosing <> add them
    subject = add_enclosing(subject)
    predicate = add_enclosing(predicate)
    obj = add_enclosing(obj)

    output = subject + ' ' + predicate + ' ' + obj + ' . \n'
    return output


def create_triple_literal(subject, predicate, literal, literal_type=None):
    """
         Constructs data properties triple string from given subject, predicate and literal.
         Arguments:
              subject: subject of the triple
              predicate: predicate of the triple
              literal: literal of the triple
              literal_type: type of the literal, e.g. XSD_STRING (optional)
         Returns:
              data properties string in the form "<subject> <predicate> 'literal' . "
    """
    # if literal is not a string, convert it
    if not isinstance(literal, str):
        literal = str(literal)
    if literal_type is None:
        # return without type
        output = '<' + subject + '> <' + predicate + '> "' + literal + '" . \n'
    else:
        # return with type
        output = '<' + subject + '> <' + predicate + '> "' + \
            literal + '"^^<' + literal_type + '> . \n'
    return output


def get_properties_for_subj(subj: str, verb_obj: dict = {}, verb_literal: dict = {}):
    """
         Constructs proper SPARQL update string from given name, verb-object and verb-literal dictionary.
         Arguments:
              subj: subject
              verb_obj:  object properties: dictionary of verb-object pairs, if value is a list, then it will be treated as multiple objects for the same verb
              verb_literal: data properties: dictionary of verb-literal pairs, if value is a list, then it will be treated as a literal with type
         Returns:
              SPARQL update string
    """
    output = f''''''
    #  object properties
    for verb, obj in verb_obj.items():
        # turn obj to list if it is not to iterate over it
        if not isinstance(obj, list):
            obj = [obj]
        for o in obj:
            output += f'''{create_triple(subj, verb, o)}'''

    # data properties
    for verb, literal in verb_literal.items():
        if not isinstance(literal, list):
            literal = [literal]
        if len(literal) == 1:
            literal.append(None)
        # check if len is max 2 for [literal, type]
        if len(literal) > 2:
            raise ValueError(
                f'Literal {literal} has more than two elements expected [literal, type] or [literal]')
        if not isinstance(literal[0], str):
            literal[0] = str(literal[0])
        # check that literal does not start with < and end with >
        if literal[0].startswith('<') and literal[0].endswith('>'):
            raise ValueError(f'Literal {literal} starts with < or ends with >')
        output += f'''{create_triple_literal(subj, verb, literal[0], literal[1])}'''

    return output

