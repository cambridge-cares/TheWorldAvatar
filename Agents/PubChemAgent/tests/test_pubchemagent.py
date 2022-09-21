from querykg import *
import pytest
import os
import json


def test_query():
    sources = []
    for inchi in ['InChI=1/C10H10/c1-2-6-10-8-4-3-7-9(10)5-1/h1-3,5-7H,4,8H2', 
                  'InChI=1/C10H10/c1-2-3-7-10-8-5-4-6-9-10/h4-6,8-9H,2H2,1H3', 
                  'InChI=1S/C24H10/c1-5-15-13-16(6-2)20-11-12-22-18(8-4)14-17(7-3)21-10-9-19(15)23(20)24(21)22/h1-4,9-14H']:
        data, source = query_with_inchi(inchi)
        sources.append(source)
    assert sources[0]=='TWA'
    assert sources[1]=='TWA'
    assert sources[2]=='PubChem'

