import pydantic
from typing import List

from chemistry_and_robots.data_model.iris import *
from pyderivationagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import BaseOntology, OM_Quantity, OM_Duration, OM_QuantityOfDimensionOne
from chemistry_and_robots.data_model.ontolab import *

TXTFILE_EXTENSION = 'txt'
XLSFILE_EXTENSION = 'xls'
MAPPING_FILENAMEEXTENSION = {DBPEDIA_XLSFILE:XLSFILE_EXTENSION, DBPEDIA_TXTFILE:TXTFILE_EXTENSION}

class PeakArea(OM_Quantity):
    clz: str = ONTOHPLC_PEAKAREA

class RetentionTime(OM_Duration):
    clz: str = ONTOHPLC_RETENTIONTIME
    refersToSpecies: Optional[str] # only use this for <RetentionTime> when it's in triples <HPLCMethod> <hasRetentionTime> <RetentionTime>

class ResponseFactor(OM_QuantityOfDimensionOne):
    clz: str = ONTOHPLC_RESPONSEFACTOR
    refersToSpecies: str

class ChromatogramPoint(BaseOntology):
    clz: str = ONTOHPLC_CHROMATOGRAMPOINT
    indicatesComponent: OntoCAPE_PhaseComponent
    hasPeakArea: PeakArea
    atRetentionTime: RetentionTime

class HPLCReport(BaseOntology):
    clz: str = ONTOHPLC_HPLCREPORT
    remoteFilePath: str
    records: List[ChromatogramPoint]
    generatedFor: ChemicalSolution
    localFilePath: str
    lastLocalModifiedAt: float
    lastUploadedAt: float

class InternalStandard(OntoCAPE_PhaseComponent):
    clz: str = ONTOHPLC_INTERNALSTANDARD

class HPLCMethod(BaseOntology):
    clz: str = ONTOHPLC_HPLCMETHOD
    hasResponseFactor: List[ResponseFactor]
    hasRetentionTime: List[RetentionTime]
    usesInternalStandard: InternalStandard
    rdfs_comment: str
    localFilePath: Optional[str] # TODO bring back to compulsory once formalise the HPLCMethod at deployment
    remoteFilePath: Optional[str] # TODO bring back to compulsory once formalise the HPLCMethod at deployment

class HPLCJob(BaseOntology):
    clz: str = ONTOHPLC_HPLCJOB
    hasReport: HPLCReport
    characterises: ReactionExperiment
    usesMethod: HPLCMethod

class HPLC(LabEquipment):
    clz: str = ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY
    reportExtension: str # this should be DBPEDIA_WIKICATFILENAMEEXTENSIONS but we simplify as str
    hasJob: Optional[List[HPLCJob]]
    hasPastReport: Optional[List[HPLCReport]] # TODO add this relationship to OntoHPLC
