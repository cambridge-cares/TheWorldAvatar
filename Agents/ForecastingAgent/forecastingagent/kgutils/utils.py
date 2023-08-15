################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 03 Aug 2023                            #
################################################

# The purpose of this module is to provide frequently used utility functions
# for knowledge graph interactions

import uuid
import pandas as pd
from dateutil.parser import isoparse
from distutils.util import strtobool

from py4jps import agentlogging

from forecastingagent.datamodel import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def remove_unnecessary_whitespace(query: str) -> str:
    """
    Remove unnecessary whitespaces
    """
    query = ' '.join(query.split())
    return query


def get_list_of_unique_values(res: list, key: str) -> list:
    """
    Unpacks a query result list (i.e., list of dicts) into a list of 
    unique values for the given dict key.
    """    
    res_list =  list(set([r.get(key) for r in res]))
    res_list = [v for v in res_list if v is not None]
    return res_list


def get_unique_value(res: list, key: str, cast_to=None) -> str:
    """
    Unpacks a query result list (i.e., list of dicts) into unique 
    value for the given dict key (returns None if no unique value is found)

    Tries to cast result in case 'cast_to' datatype is provided
    """
    
    res_list =  get_list_of_unique_values(res, key)
    if len(res_list) == 1:
        # Try to cast retrieved value to target type (throws error if not possible)
        if cast_to and issubclass(cast_to, bool):
            res_list[0] = bool(strtobool(res_list[0]))
        elif cast_to and issubclass(cast_to, (int, float)):
            res_list[0] = cast_to(res_list[0])
        return res_list[0]
    else:
        if len(res_list) == 0:
            msg = f"No value found for key: {key}."
        else:
            msg = f"Multiple values found for key: {key}."
        logger.warning(msg)
        return None


def add_enclosing(s):
    """
    Adds IRI enclosing < > to a string if it does not have it already.
    
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


def create_triple(subj, pred, obj):
    """
    Constructs object property triple from given subject, predicate and object.
    (literals are handled by create_triple_literal)
    
    Arguments:
        subj {str} - subject of the triple
        pred {str} - predicate of the triple
        obj {str} - object of the triple
    Returns:
        SPARQL triple string in the form "<subject> <predicate> <object>."
    """

    # Ensure instances have enclosing < >
    subj = add_enclosing(subj)
    pred = add_enclosing(pred)
    obj = add_enclosing(obj)
    triple = f'{subj} {pred} {obj} . '

    return triple


def create_triple_literal(subj, pred, literal, literal_type=None):
    """
    Constructs data property triple from given subject, predicate and literal.
    
    Arguments:
        subj {str} - subject of the triple
        pred {str} - predicate of the triple
        literal - literal of the triple
        literal_type - type of the literal, e.g. XSD_STRING (optional)
    Returns:
        SPARQL triple string in the form "<subject> <predicate> 'literal' . "
    """

    # Ensure instances have enclosing < >
    subj = add_enclosing(subj)
    pred = add_enclosing(pred)
    
    # Cast literal if not string
    if not isinstance(literal, str):
        literal = str(literal)

    if literal_type is None:
        # Return without explicit type declaration (default string)
        triple = f'{subj} {pred} "{literal}" . '
    else:
        literal_type = add_enclosing(literal_type)
        # Return with explicit type declaration
        triple = f'{subj} {pred} "{literal}"^^{literal_type} . '
    return triple


def create_properties_for_subj(subj: str, pred_obj: dict = {}, pred_literal: dict = {}):
    """
    Constructs SPARQL INSERT DATA triples for given subject, predicate-object and
    predicate-literal dictionary.

    Arguments:
        subj {str} - instance IRI
        pred_obj {dict} - object properties: dictionary of pred-object pairs
                          (if value is a list, then it will be treated as multiple
                          objects for the same predicate)
        pred_literal {dict} - data properties: dictionary of pred-literal pairs
                              (if value is a list, then it will be treated as a 
                              literal explicit type declaration)
    Returns:
        SPARQL INSERT DATA body string
    """

    insert_body = ''
    
    #  Object properties
    for pred, obj in pred_obj.items():
        # cast obj to list if it is not to iterate over it
        if not isinstance(obj, list):
            obj = [obj]
        for o in obj:
            insert_body += f'{create_triple(subj, pred, o)}'

    # Data properties
    for pred, literal in pred_literal.items():
        if not isinstance(literal, list):
            literal = [literal]
        if len(literal) == 1:
            literal.append(None)
        # check if len is max 2 for [literal, type]
        if len(literal) > 2:
            raise ValueError(f'Literal {literal} has more than two elements! Expected are [literal, type] or [literal].')
        if not isinstance(literal[0], str):
            literal[0] = str(literal[0])
        # check that literal does not start with < or end with >
        if literal[0].startswith('<') or literal[0].endswith('>'):
            raise ValueError(f'Literal {literal} starts with < or ends with >')
        insert_body += f'{create_triple_literal(subj, pred, literal[0], literal[1])}'

    return insert_body


def create_time_instant(time):
    """
    Create SPARQL INSERT DATA triples to create a new time:Instant instance for
    provided timestamp

    Returns:
        instant_iri: IRI of new time:Instant instance
        update: SPARQL INSERT DATA body string
    """

    # Get UNIX timestamp (in s) of provided time
    time_stamp = convert_time_to_timestamp(time)

    # Create IRIs for new instances
    instant_iri = KB + 'Instant_' + str(uuid.uuid4())
    timePosition_iri = KB + 'TimePosition_' + str(uuid.uuid4())

    update = ''
    update += create_properties_for_subj(subj=instant_iri, pred_obj={
        RDF_TYPE: TIME_INSTANT,
        TIME_INTIMEPOSITION: timePosition_iri
        })
    update += create_properties_for_subj(subj=timePosition_iri, pred_obj={
        RDF_TYPE: TIME_TIMEPOSITION,
        TIME_HASTRS: UNIX_TIME
    }, pred_literal={
        TIME_NUMERICPOSITION: [time_stamp, XSD_DECIMAL]
    })

    return instant_iri, update


def convert_time_to_timestamp(time):
    """
    Converts a time to a unix timestamp (in seconds)

    Arguments:
        time: a time (int, str, pd.Timestamp)
    Returns:
        unix timestamp (in s)
    """

    # convert time to unix timestamp
    if isinstance(time, int):
        time_stamp = time
    elif isinstance(time, str):
        time_stamp = pd.Timestamp(
            isoparse(time)).tz_convert('UTC').tz_localize(None).timestamp()
    elif isinstance(time, pd.Timestamp):
        time_stamp = time.timestamp()
    else:
        raise ValueError(
            f'Unknown time format: {time}. Please use int, str or pd.Timestamp')
    return int(time_stamp)