import pydantic
from typing import List

from chemistry_and_robots.data_model.iris import *
from pyderivationagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import BaseOntology, OM_Quantity, OM_Duration, OM_QuantityOfDimensionOne
from chemistry_and_robots.data_model.ontolab import *

# TODO add below IRIs to pyasyncagent.data_model.iris, also TBox CSV/OWL if applicable
ONTOHPLC_LOCALREPORTDIRECTORY = ONTOHPLC + 'localReportDirectory'
ONTOHPLC_REPORTEXTENSION = ONTOHPLC + 'reportExtension'
DBPEDIA_XLSFILE = 'http://dbpedia.org/resource/Microsoft_Excel'
DBPEDIA_CSVFILE = 'http://dbpedia.org/resource/Comma-separated_values'
DBPEDIA_TXTFILE = 'http://dbpedia.org/resource/Text_file'
# DBPEDIA_XLSFILE, DBPEDIA_CSVFILE, and DBPEDIA_TXTFILE all have rdf:type DBPEDIA_WIKICATFILENAMEEXTENSIONS
DBPEDIA_WIKICATFILENAMEEXTENSIONS = 'http://dbpedia.org/class/yago/WikicatFilenameExtensions'
ONTOHPLC_CHARACTERISES = ONTOHPLC + 'characterises'
TXTFILE_EXTENSION = 'txt'
XLSFILE_EXTENSION = 'xls'
ONTOHPLC_LASTLOCALMODIFIEDAT = ONTOHPLC + 'lastLocalModifiedAt'
ONTOHPLC_LASTUPLOADEDAT = ONTOHPLC + 'lastUploadedAt'
MAPPING_FILENAMEEXTENSION = {DBPEDIA_XLSFILE:XLSFILE_EXTENSION, DBPEDIA_TXTFILE:TXTFILE_EXTENSION}
ONTOHPLC_LOCALREPORTFILE = ONTOHPLC + 'localReportFile'

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
    hasReportPath: str
    records: List[ChromatogramPoint]
    generatedFor: ChemicalSolution
    localReportFile: str
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

class HPLCJob(BaseOntology):
    clz: str = ONTOHPLC_HPLCJOB
    hasReport: HPLCReport
    characterises: ReactionExperiment
    usesMethod: HPLCMethod

class HPLC(LabEquipment):
    clz: str = ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY
    reportExtension: str # this should be DBPEDIA_WIKICATFILENAMEEXTENSIONS but we simplify as str
    hasJob: List[HPLCJob]
