from enum import Enum

aboxStages = Enum('aboxStages',
    ('QC_LOG',
     'QC_JSON',
     'OC_JSON',
     'OS_JSON',
     'OPS_JSON',
     'CSV',
     'OWL'
    )
)