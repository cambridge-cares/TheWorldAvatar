from enum import Enum

aboxStages = Enum('aboxStages',
    ('QC_LOG',
     'QC_JSON',
     'OC_JSON',
     'OPS_JSON',
     'CSV',
     'OWL'
    )
)