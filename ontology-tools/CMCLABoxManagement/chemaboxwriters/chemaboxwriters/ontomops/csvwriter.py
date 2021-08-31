# -*- coding: utf-8 -*-
"""
Created on Thu Mar  4 16:10:02 2021

@author: angir
"""

import json
import csv

from io import StringIO
import chemaboxwriters.common.commonvars as commonv
from chemaboxwriters.ontomops.prefixes import onto_spec, \
                                                 gain_pref, \
                                                 kin_pref, \
                                                 table_pref, \
                                                 unit_pref, \
                                                 onto_kb

def omops_csv_abox_from_string(data):
    data = json.loads(data)
    gen_id = data[commonv.ENTRY_UUID]

    csvfile = StringIO(newline='')

    spamwriter = csv.writer(csvfile, delimiter=',',
                            quotechar='"', quoting=csv.QUOTE_MINIMAL)

    out_id = data[commonv.ENTRY_IRI]


    spamwriter = csv.writer(csvfile, delimiter=',',
                                quotechar='|', quoting=csv.QUOTE_MINIMAL)
    spamwriter.writerow(['Source', 'Type', 'Target', 'Relation','Value','Data Type'])


    csvcontent = csvfile.getvalue()
    csvfile.close()
    return csvcontent