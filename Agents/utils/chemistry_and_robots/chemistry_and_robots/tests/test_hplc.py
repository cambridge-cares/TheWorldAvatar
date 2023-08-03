from chemistry_and_robots.tests.conftest import TargetIRIs
import chemistry_and_robots.tests.conftest as conftest
from chemistry_and_robots.hardware import hplc
from rdflib import Graph
import filecmp
import pytest
import os

@pytest.mark.parametrize(
    "local_file_path,hplc_digital_twin",
    [
        (conftest.HPLC_XLS_REPORT_FILE, TargetIRIs.HPLC_1_POST_PROC_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE, TargetIRIs.HPLC_2_POST_PROC_IRI.value),
        (conftest.HPLC_XLS_REPORT_FILE_INCOMPLETE, TargetIRIs.HPLC_1_POST_PROC_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE_INCOMPLETE, TargetIRIs.HPLC_2_POST_PROC_IRI.value),
        (conftest.HPLC_XLS_REPORT_FILE_UNIDENTIFIED_PEAKS, TargetIRIs.HPLC_1_POST_PROC_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE_UNIDENTIFIED_PEAKS, TargetIRIs.HPLC_2_POST_PROC_IRI.value),
        (conftest.HPLC_XLS_REPORT_FILE_NO_PRODUCT, TargetIRIs.HPLC_1_POST_PROC_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE_NO_PRODUCT, TargetIRIs.HPLC_2_POST_PROC_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE_WITH_SPACE, TargetIRIs.HPLC_2_POST_PROC_IRI.value),
    ],
)
def test_process_raw_hplc_report_file(
    initialise_triples, generate_random_download_path,
    local_file_path, hplc_digital_twin
):
    sparql_client = initialise_triples
    timestamp_last_modified = os.path.getmtime(local_file_path)

    # First upload the report and upload relevent triples (as part of HPLC Agent)
    hplc_report_iri = sparql_client.upload_raw_hplc_report_to_kg(
        local_file_path=local_file_path,
        timestamp_last_modified=timestamp_last_modified,
        remote_report_subdir='D:\\' + local_file_path, # 'D:\\' is added to test the method is able to cope with a path with a drive letter
        hplc_digital_twin=hplc_digital_twin
    )
    g = Graph()
    g = sparql_client.collect_triples_for_hplc_job("http://placeholder/rxn_exp", "http://placeholder/chem_amount", hplc_digital_twin, hplc_report_iri, "http://placeholder/hplc_method", g)
    sparql_client.uploadGraph(g)

    # Second download uploaded HPLC report file, make sure the content is the same
    remote_file_path, file_extension = sparql_client.get_raw_hplc_report_remote_path_and_extension(hplc_report_iri)
    full_downloaded_path = generate_random_download_path(file_extension)
    sparql_client.download_remote_raw_hplc_report(remote_file_path=remote_file_path, downloaded_file_path=full_downloaded_path)
    assert filecmp.cmp(local_file_path,full_downloaded_path)

    # Then process the downloaded report file
    lst_chrom_pts = hplc.read_raw_hplc_report_file(hplc_report_iri, full_downloaded_path, file_extension)

    for pt in lst_chrom_pts:
        assert pt[conftest.onto.ONTOHPLC_RETENTIONTIME].instance_iri.startswith(pt[conftest.onto.ONTOHPLC_RETENTIONTIME].namespace_for_init)
        assert pt[conftest.onto.ONTOHPLC_RETENTIONTIME].hasValue.instance_iri.startswith(pt[conftest.onto.ONTOHPLC_RETENTIONTIME].hasValue.namespace_for_init)
        assert pt[conftest.onto.ONTOHPLC_RETENTIONTIME].hasValue.hasUnit is not None
        assert pt[conftest.onto.ONTOHPLC_RETENTIONTIME].hasValue.hasNumericalValue is not None

        assert pt[conftest.onto.ONTOHPLC_PEAKAREA].instance_iri.startswith(pt[conftest.onto.ONTOHPLC_PEAKAREA].namespace_for_init)
        assert pt[conftest.onto.ONTOHPLC_PEAKAREA].hasValue.instance_iri.startswith(pt[conftest.onto.ONTOHPLC_PEAKAREA].hasValue.namespace_for_init)
        assert pt[conftest.onto.ONTOHPLC_PEAKAREA].hasValue.hasUnit is not None
        assert pt[conftest.onto.ONTOHPLC_PEAKAREA].hasValue.hasNumericalValue is not None
