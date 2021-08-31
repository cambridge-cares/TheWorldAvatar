from enum import Enum

aboxStages = Enum('aboxStages',
    ('QC_LOG',
     'QC_JSON',
     'OC_JSON',
     'OS_JSON',
     'OPS_JSON',
     'OMOPS_INP_JSON',
     'OMOPS_JSON',
     'CSV',
     'OWL'
    )
)