'''
This module defines iterators on candidate matching pairs between the individuals
of two ontologies.
'''

import collections
import itertools
import logging
from typing import List, Union

import pandas as pd
import rdflib
from tqdm import tqdm

from ontomatch.utils.ontologyWrapper import Ontology

class FullPairIterator(collections.Iterable, collections.Sized):

    """
    Iterator on the Cartesian product of the ordered positions of individuals
    in the source ontology and the target ontology
    """

    def __init__(self, dframe_src:pd.DataFrame, dframe_tgt:pd.DataFrame):
        """
        Init the iterator on the Cartesian product of the ordered positions of individuals
        in the source ontology and the target ontology
        """

        len_src = len(dframe_src)
        len_tgt = len(dframe_tgt)
        self.length = len_src * len_tgt
        self.iterator = itertools.product(range(len_src), range(len_tgt))

    def __iter__(self):
        return self.iterator

    def __len__(self):
        return self.length


class TokenBasedPairIterator(collections.Iterable, collections.Sized):

    """
    Iterator on a filtered subset of the Cartesian product of the ordered positions of individuals
    in the source ontology and the target ontology. The filter reduces the number of candidate
    matching pairs by creating an inverted index on the tokenized data of individuals and
    considering only pairs of individuals that have at least one token in common.
    """

    df_src = None
    df_tgt = None
    df_index_tokens_unpruned = None
    df_index_tokens = None
    candidate_matching_pairs = None

    def __init__(self, dframe_src:pd.DataFrame, dframe_tgt:pd.DataFrame,
                min_token_length:int =3, max_token_occurrences_src:int =20, max_token_occurrences_tgt:int =20,
                blocking_properties: List[str] =None, reset_index:bool =False, use_position:bool =False):

        """
        Init a token-based iterator.

        Args:
            src_onto (Ontology): source ontology
            tgt_onto (Ontology): target ontology
            min_token_length (int, optional): minimum length of a token to be considered for candidate matching pairs. Defaults to 3.
            max_token_occurrences_src (int, optional): discard the token if the number of source individuals containing the token is larger than this value. Defaults to 20.
            max_token_occurrences_tgt (int, optional): discard the token if the number of target individuals containing the token is larger than this value. Defaults to 20.
            blocking_properties (List[str], optional): the names of the properties which string values are tokenized. If None all properties with string values are used. Defaults to None.
            reset_index (bool, optional): create the index only once if False else create it from scratch. Defaults to False.
        """


        # create the index only once
        if (TokenBasedPairIterator.candidate_matching_pairs is None) or reset_index:
            logging.info('creating index, mtf_src=%s, mtf_tgt=%s', max_token_occurrences_src, max_token_occurrences_tgt)
            # create the index as pandas DataFrame with additional information about length of tokens
            # and number of entities related to a given token
            df_index, df_src, df_tgt = self._create_index(dframe_src, dframe_tgt, blocking_properties)
            TokenBasedPairIterator.df_src = df_src
            TokenBasedPairIterator.df_tgt = df_tgt
            TokenBasedPairIterator.df_index_tokens_unpruned = df_index
            # discard tokens that are too small or too frequent
            mask = (df_index['len'] >= min_token_length) & (df_index['count_1'] <= max_token_occurrences_src) & (df_index['count_2'] <= max_token_occurrences_tgt)
            TokenBasedPairIterator.df_index_tokens = df_index[mask]
            logging.info('finished creating index')
            TokenBasedPairIterator.candidate_matching_pairs = self._get_candidate_matching_pairs(TokenBasedPairIterator.df_index_tokens, use_position)
            logging.info('number of tokens in inverted index=%s', len(TokenBasedPairIterator.df_index_tokens))
            logging.info('number of candidate matching pairs=%s', len(TokenBasedPairIterator.candidate_matching_pairs))

    def __iter__(self):
        return iter(TokenBasedPairIterator.candidate_matching_pairs)

    def __len__(self):
        return len(TokenBasedPairIterator.candidate_matching_pairs)

    def _create_index_internal(self, dframe, column, dataset_id, tokenize_fct, index):

        if column in dframe.columns:

            column_count = 'count_' + str(dataset_id)
            column_links = 'links_' + str(dataset_id)

            for i, row in tqdm(dframe.iterrows()):
                parsed = tokenize_fct(row[column])
                # i is the index from the original dataset
                # pos is the position of the corresponding entity in the list of entities
                pos = row['pos']
                entry = (pos, i)
                for p in parsed:
                    found = index.get(p)
                    if found:
                        links = found.get(column_links)
                        if links:
                            links.add(entry)
                        else:
                            found[column_links] = {entry}
                    else:
                        index[p] = {'len': len(p), column_links: {entry}}

            for key, value in index.items():
                links = value.get(column_links)
                if links:
                    length = len(links)
                    index[key][column_count] = length

            df_index = pd.DataFrame.from_dict(index, orient='index')
            logging.info('total number of tokens after processing property %s is %s', column, len(df_index))
        else:
            df_index = pd.DataFrame.from_dict(index, orient='index')
            logging.warning('column not found:%s', column)

        return index, df_index

    def _create_index(self, dframe_src, dframe_tgt, blocking_properties):

        index = {}

        # create token index for source ontology
        logging.info('columns dataset 1 = %s', [ str(c) for c in dframe_src.columns ])
        if blocking_properties is None:
            blocking_properties = dframe_src.columns
        for prop in blocking_properties:
            index, df_index = self._create_index_internal(dframe_src, prop, dataset_id=1, tokenize_fct=tokenize, index=index)

        # add tokens for target ontology
        logging.info('columns dataset 1 = %s', [ str(c) for c in dframe_tgt.columns ])
        if blocking_properties is None:
            blocking_properties = dframe_tgt.columns
        for prop in blocking_properties:
            index, df_index = self._create_index_internal(dframe_tgt, prop, dataset_id=2, tokenize_fct=tokenize, index=index)

        logging.info('columns=%s', df_index.columns)

        return df_index, dframe_src, dframe_tgt

    def _get_candidate_matching_pairs(self, df_index, use_position):
        # reorder the data in the inverted index dataframe such that
        # 'pairs' has the form { (pos_1, idx_1) : { token: [ (pos_2, idx_2) ] } }
        # pos_1 and pos_2 are the ordered position of the individuals in the source
        # and target ontology
        pairs = {}
        for token, row in tqdm(df_index.iterrows()):

            links_1 = row['links_1']
            links_2 = row['links_2']

            if isinstance(links_1, set) and isinstance(links_2, set):
                for entry in links_1:
                    dict_pairs = pairs.get(entry)
                    if dict_pairs is None:
                        pairs[entry] = {token: links_2}
                    else:
                        dict_pairs.update({token: links_2})

        # pairs_positions = set of tuples (pos_1, pos_2)
        pairs_positions = set()
        for entry, token_links in tqdm(pairs.items()):
            for token, links in token_links.items():
                if use_position:
                    pos_1 = entry[0]
                    product = [ (pos_1, l[0]) for l in links ]
                else: # use index
                    idx_1 = entry[1]
                    product = [ (idx_1, l[1]) for l in links ]
                pairs_positions.update(product)

        return pd.MultiIndex.from_tuples(pairs_positions, names=['idx_1', 'idx_2'])

def preprocess_string(s:str) -> str:
    if not isinstance(s, str):
        return None
    for c in ['.', '-', '(', ')', ',', "'", '_', '/', '   ', '  ']:
        s = s.replace(c, ' ')
    return s.lower().strip()

def tokenize(s:str) -> List[str]:
    if isinstance(s, str) and s != '':
        s = preprocess_string(s)
        return s.split(' ')
    return []

def create_iterator(src_onto:Union[pd.DataFrame, Ontology], tgt_onto:Union[pd.DataFrame,Ontology], params_blocking:dict, use_position:bool =False):
    params_copy = params_blocking.copy()
    name = params_copy.pop('name')
    params_model_specific = params_blocking.get('model_specific')
    if not params_model_specific:
        params_model_specific = {}
    if name == 'TokenBasedPairIterator':
        params_model_specific.update({'reset_index': True, 'use_position': use_position})

    if isinstance(src_onto, Ontology):
        dframe_src = create_dataframe_from_ontology(src_onto)
    else:
        dframe_src = src_onto
    if isinstance(tgt_onto, Ontology):
        dframe_tgt = create_dataframe_from_ontology(tgt_onto)
    else:
        dframe_tgt = tgt_onto

    it_instance = globals()[name](dframe_src, dframe_tgt, **params_model_specific)
    return it_instance

def uri_to_string(s):
    s = str(s).replace('http://dbpedia.org/ontology/', 'dbo:').replace('http://dbpedia.org/property/', 'dbp:')
    s = s.replace('http://www.w3.org/2003/01/geo/', 'geo:')
    #i = s.rfind('/')
    #return (s if i is None else s[i+1:])
    return s

def create_dataframe_from_ontology(onto):
    rows = []

    logging.info('number of entities=%s', len(onto.valueMap.items()))

    for props in tqdm(onto.valueMap.items()):

        row = {}

        # get the position of the subject
        pos = props[0]

        rdf_types = onto.icmap[pos]
        if isinstance(rdf_types, str):
            rdf_types = [rdf_types]
        # discard general types
        for general_type in ['NamedIndividual', 'Thing']:
            if rdf_types is None:
                logging.warning('an individual is skipped since no rdf types found, individual=%s', props)
                #this is a hack, e.g. '.../Germany' is both an indiviual of its own and occurs as value of 'hasAddress'
                continue
            for rdf_type in rdf_types.copy():
                if rdf_type.endswith(general_type):
                    rdf_types.remove(rdf_type)
        row['type'] = rdf_types

        iri = onto.individualList[pos]
        row['iri'] = iri
        print(iri)

        # TODO-AE 211026 add rdfs:label or replace name by rdfs:label; this is too specialized (will not work for DBPedia)
        # also idx will not work
        subj = onto.individualNames[pos]
        subj = subj.replace('PowerPlant', '')
        first = subj.find('_')
        if first > 0:
            last = subj.rfind('_')
            idx = subj[:first]
            subj = subj[first+1:last]
        else:
            idx = subj
            subj = None

        row['pos'] = pos
        row['idx'] = idx
        if subj:
            row['name'] = subj
        else:
            # empty name as None
            row['name'] = None

        # predicate object tuples
        p_o_tuples = props[1]
        for p, o in p_o_tuples:
            if isinstance(o, rdflib.Literal):

                if isinstance(p, list):
                    # list contains the properties of a property path
                    # they are merged to get a name for the dataframe column
                    p_str = [ uri_to_string(q) for q in p]
                    column = '/'.join(p_str)
                else:
                    column = uri_to_string(p)

                row[column] = o.toPython()

            else:
                raise RuntimeError('not supported yet, s=', onto.individualNames[pos], ', p=', p, ', o=', o )

        rows.append(row)

    dframe = pd.DataFrame(rows)
    dframe.set_index(['idx'], inplace=True)
    logging.info('number of rows=%s', len(dframe))
    return dframe
