from chemistry_and_robots.data_model.ontohplc import DBPEDIA_TXTFILE, DBPEDIA_XLSFILE
from chemistry_and_robots.hardware import hplc
import pytest
from pathlib import Path
from typing import List
import uuid

HPLC_REPORT_IRI = '' # TODO add this
RESOURCES_SAMPLE_DATA = str(Path(__file__).absolute().parent.parent)+'/chemistry_and_robots/resources/sample_data'
RAW_HPLC_REPORT_XLS = RESOURCES_SAMPLE_DATA + '/raw_hplc_report_xls.xls'
RAW_HPLC_REPORT_TXT = RESOURCES_SAMPLE_DATA + '/raw_hplc_report_txt.txt'

@pytest.mark.parametrize(
    "hplc_report_iri,file_path,filename_extension",
    [
        (HPLC_REPORT_IRI, RAW_HPLC_REPORT_XLS, DBPEDIA_XLSFILE),
        (HPLC_REPORT_IRI, RAW_HPLC_REPORT_TXT, DBPEDIA_TXTFILE),
    ],
)
def test_process_raw_hplc_report_file(hplc_report_iri, file_path, filename_extension):
    # TODO hplc_report_iri and file_path should be provided as the IRI to be uploaded to the file server
    # hplc.process_raw_hplc_report_file(hplc_report_iri, file_path, filename_extension)
    pass
