from enum import Enum

aboxStages = Enum('aboxStages',
    ('QC_LOG',
     'QC_JSON',
     'OC_JSON',
     'OS_JSON',
     'OPS_JSON',
     'OMINP_JSON',
     'OM_JSON',
     'OC_CSV',
     'OS_CSV',
     'OPS_CSV',
     'OM_CSV',
     'OC_OWL',
     'OS_OWL',
     'OPS_OWL',
     'OM_OWL'
    )
)