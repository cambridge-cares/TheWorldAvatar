import pytest
from data_classes.ts_data_classes import KgAccessInfo
from kg_access.apisource_kg_client import  APISourceSparqlClient
from downloader.downloaders import Downloader

TEST_KG_INFO = KgAccessInfo(endpoint='http://localhost:9999/blazegraph/namespace/test/sparql')
TARGET_IRI_DUNIT = "https://www.theworldavatar.com/kg/test/Number_773521f9-528e-40fc-9bcb-4218e49f40e8"
TARGET_IRI_PV = "https://www.theworldavatar.com/kg/test/Number_773521f9-528e-40fc-9bcb-4218e49f23ds"
TARGET_IRI_TEMP = "https://www.theworldavatar.com/kg/test/Number_773521f9-528e-40fc-9bcb-4218e49f12eg"
EXPECTED_INFO_PV = {'dynamic_generated': False, 'url': 'https://www.ema.gov.sg/content/dam/corporate/resources/singapore-energy-statistics/excel/SES_Public_2023_tidy.xlsx.coredownload.xlsx','format': 'xlsx', 'method': 'GET', 'contenttype': 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 'value_iter': 'T6.1/inst_cap_mwp', 'time_iter': 'T6.1/year', 'calculation': 'algebra_sum', 'src_iri':"https://www.theworldavatar.com/kg/test/Number_773521f9-528e-40fc-9bcb-4218e49f23ds"}

EXPECTED_INFO_TEMP ={'dynamic_generated': True, 'method': 'GET', 'format': 'csv','contenttype': 'text/csv', 'url': 'https://api-production.data.gov.sg/v2/internal/api/datasets/d_755290a24afe70c8f9e8bcbf9f251573/initiate-download', 'method_dynamic': 'POST', 'value_iter': 'mean_temp', 'time_iter': 'month','src_iri':"https://www.theworldavatar.com/kg/test/Number_773521f9-528e-40fc-9bcb-4218e49f12eg"}

EXPECTED_INFO_DUNIT = {'dynamic_generated': False, 'url': 'https://tablebuilder.singstat.gov.sg/api/table/tabledata/M400751?seriesNoORrowNo=1', 'method': 'GET', 'contenttype': 'application/json', 'format': 'json','value_iter': "$.Data.row[?(@.seriesNo=='1')].columns[*].value", 'time_iter': "$.Data.row[?(@.seriesNo=='1')].columns[*].key",'src_iri':"https://www.theworldavatar.com/kg/test/Number_773521f9-528e-40fc-9bcb-4218e49f40e8"}


def test_get_api_source_info_from_kg():
    cl = APISourceSparqlClient(TEST_KG_INFO)
    url_info_pv = cl.get_api_info(TARGET_IRI_PV)
    url_info_temp = cl.get_api_info(TARGET_IRI_TEMP)
    url_info_dunit = cl.get_api_info(TARGET_IRI_DUNIT)
    assert url_info_pv == EXPECTED_INFO_PV
    assert url_info_temp == EXPECTED_INFO_TEMP
    assert url_info_dunit == EXPECTED_INFO_DUNIT

def test_api_source_kg_to_downloaders():
    cl = APISourceSparqlClient(TEST_KG_INFO)
    url_info_pv = cl.get_api_info(TARGET_IRI_PV)
    url_info_temp = cl.get_api_info(TARGET_IRI_TEMP)
    url_info_dunit = cl.get_api_info(TARGET_IRI_DUNIT)
    for url_info in [url_info_pv, url_info_temp, url_info_dunit]:
        d = Downloader(**url_info)
        r = d.download_tsinstance()
        print(r)
        assert len(r.times)>0
