'''
This module defines iterators on candidate matching pairs between the individuals
of two ontologies.
'''

import collections
import itertools
import logging
import typing

import pandas as pd
import rdflib
from tqdm import tqdm

from ontologyWrapper import Ontology

class FullPairIterator(collections.Iterable, collections.Sized):

    """
    Iterator on the Cartesian product of the ordered positions of individuals
    in the source ontology and the target ontology
    """

    def __init__(self, src_onto:Ontology, tgt_onto:Ontology):
        """
        Init the iterator on the Cartesian product of the ordered positions of individuals
        in the source ontology and the target ontology

        Args:
            src_onto (Ontology): source ontology
            tgt_onto (Ontology): target ontology
        """

        len_src = len(src_onto.individualList)
        len_tgt = len(tgt_onto.individualList)
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

    candidate_matching_pairs = None

    def __init__(self, src_onto:Ontology, tgt_onto:Ontology,
                min_token_length:int =3, max_token_occurrences_src:int =20, max_token_occurrences_tgt:int =20,
                blocking_properties: typing.List[str] =None, reset_index:bool =False):

        """
        Init a token-based iterator.

        Args:
            src_onto (Ontology): source ontology
            tgt_onto (Ontology): target ontology
            min_token_length (int, optional): minimum length of a token to be considered for candidate matching pairs. Defaults to 3.
            max_token_occurrences_src (int, optional): discard the token if the number of source individuals containing the token is larger than this value. Defaults to 20.
            max_token_occurrences_tgt (int, optional): discard the token if the number of target individuals containing the token is larger than this value. Defaults to 20.
            blocking_properties (typing.List[str], optional): the names of the properties which string values are tokenized. If None all properties with string values are used. Defaults to None.
            reset_index (bool, optional): create the index only once if False else create it from scratch. Defaults to False.
        """


        # create the index only once
        if (TokenBasedPairIterator.candidate_matching_pairs is None) or reset_index:
            logging.info('creating inverted index ...')
            # create the index as pandas DataFrame with additional information about length of tokens
            # and number of entities related to a given token
            dframe = self._create_index(src_onto, tgt_onto, blocking_properties)
            # discard tokens that are too small or too frequent
            mask = (dframe['len'] >= min_token_length) & (dframe['count_1'] <= max_token_occurrences_src) & (dframe['count_2'] <= max_token_occurrences_tgt)
            dframe = dframe[mask]
            logging.info('finished creating index')
            logging.info('number of tokens in inverted index=%s', len(dframe))
            TokenBasedPairIterator.candidate_matching_pairs = self._get_candidate_matching_pairs(dframe)
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

        else:
            logging.warn('column not found:%s', column, )

        df_index = pd.DataFrame.from_dict(index, orient='index')

        return index, df_index

    def _create_index(self, src_onto, tgt_onto, blocking_properties):

        index = {}

        # create token index for source ontology
        dframe = create_dataframe_from_ontology(src_onto)
        if blocking_properties is None:
            blocking_properties = dframe.columns
        for prop in blocking_properties:
            index, df_index = self._create_index_internal(dframe, prop, dataset_id=1, tokenize_fct=tokenize, index=index)
            logging.info('total number of tokens after processing property %s is %s', prop, len(df_index))

        # add tokens for target ontology
        dframe = create_dataframe_from_ontology(tgt_onto)
        if blocking_properties is None:
            blocking_properties = dframe.columns
        for prop in blocking_properties:
            index, df_index = self._create_index_internal(dframe, prop, dataset_id=2, tokenize_fct=tokenize, index=index)
            logging.info('total number of tokens after processing property %s is %s', prop, len(df_index))

        logging.info('columns=%s', df_index.columns)

        return df_index

    def _get_candidate_matching_pairs(self, df_index):
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
                pos_1 = entry[0]
                product = [ (pos_1, l[0]) for l in links ]
                pairs_positions.update(product)

        return list(pairs_positions)

def preprocess_string(s:str) -> str:
    if not isinstance(s, str):
        return None
    for c in ['.', '-', '(', ')', ',', "'", '_', '/', '   ', '  ']:
        s = s.replace(c, ' ')
    return s.lower().strip()

def tokenize(s:str) -> typing.List[str]:
    if isinstance(s, str):
        s = preprocess_string(s)
        return s.split(' ')
    return []

def create_iterator(src_onto:Ontology, tgt_onto:Ontology, params:dict):
    params_copy = params.copy()
    name = params_copy.pop('name')
    it_instance = globals()[name](src_onto, tgt_onto, **params_copy)
    return it_instance

def create_dataframe_from_ontology(onto):
    rows = []

    logging.info('number of entities=%s', len(onto.valueMap.items()))

    for props in tqdm(onto.valueMap.items()):

        row = {}

        # get the position of the subject
        pos = props[0]
        subj = onto.individualNames[pos]
        first = subj.find('_')
        last = subj.rfind('_')
        idx = subj[:first]
        subj = subj[first+1:last]

        row['pos'] = pos
        row['idx'] = idx
        row['name'] = subj

        # predicate object tuples
        p_o_tuples = props[1]
        for p, o in p_o_tuples:
            if isinstance(o, rdflib.Literal):
                row[p] = o.toPython()
            else:
                raise RuntimeError('not supported yet, s=', onto.individualNames[pos], ', p=', p, ', o=', o )

        rows.append(row)

    dframe = pd.DataFrame(rows)
    dframe.set_index(['idx'], inplace=True)
    logging.info('number of rows=%s', len(dframe))
    logging.info('columns=%s', dframe.columns)
    return dframe
