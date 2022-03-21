from chemistry_and_robots.tests.conftest import TargetIRIs
import chemistry_and_robots.tests.conftest as conftest
from chemistry_and_robots.hardware import hplc
import filecmp
import pytest
import os

@pytest.mark.parametrize(
    "local_file_path,hplc_digital_twin",
    [
        (conftest.HPLC_XLS_REPORT_FILE, TargetIRIs.HPLC_1_POST_PROC_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE, TargetIRIs.HPLC_2_POST_PROC_IRI.value),
    ],
)
def test_process_raw_hplc_report_file(initialise_triples, generate_random_download_path, local_file_path, hplc_digital_twin):
    sparql_client = initialise_triples
    timestamp_last_modified = os.path.getmtime(local_file_path)

    # First upload and download the report (as part of HPLCInput Agent), make sure the content is the same
    hplc_report_iri = sparql_client.upload_raw_hplc_report_to_fs_kg(local_file_path=local_file_path,
        timestamp_last_modified=timestamp_last_modified, hplc_digital_twin=hplc_digital_twin)

    # Second download uploaded HPLC report file
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
