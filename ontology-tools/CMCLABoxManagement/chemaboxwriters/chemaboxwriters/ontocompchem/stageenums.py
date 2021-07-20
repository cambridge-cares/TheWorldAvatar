from enum import Enum

aboxStages = Enum('aboxStages',
    ('QC_LOG',
     'QC_JSON',
     'OC_JSON',
     'OC_CSV'
    )
)