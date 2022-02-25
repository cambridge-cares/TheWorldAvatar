import pytest
import os
from chemaboxwriters.common.assemble_pipeline import assemble_pipeline
import chemaboxwriters.common.endpoints_config as endp_conf
from chemaboxwriters.ontocompchem.pipeline import OC_PIPELINE
from chemaboxwriters.ontospecies.pipeline import OS_PIPELINE
from chemaboxwriters.ontomops.pipeline import OMOPS_PIPELINE
from chemaboxwriters.ontopesscan.pipeline import OPS_PIPELINE
from typing import List, Dict

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_CONFIG_FILES = os.path.join(THIS_DIR, "refData", "test_config_files")


OPS_bond_handler_kwargs = {
    "OC_JSON_TO_OPS_JSON": {
        "os_iris": "Species_11-111-111",
        "os_atoms_iris": "Atom_11-11-111_C1,Atom_11-11-111_C2",
        "oc_atoms_pos": "1,2",
        "random_id": "OPStestID-111-111-11",
    }
}
OPS_angle_handler_kwargs = {
    "OC_JSON_TO_OPS_JSON": {
        "os_iris": "Species_11-111-111",
        "os_atoms_iris": "Atom_11-11-111_C1,Atom_11-11-111_O1,Atom_11-11-111_H1",
        "oc_atoms_pos": "2,3,9",
        "random_id": "OPStestID-111-111-11",
    }
}

OPS_dihedral_angle_handler_kwargs = {
    "OC_JSON_TO_OPS_JSON": {
        "os_iris": "Species_11-111-111",
        "os_atoms_iris": "Atom_11-11-111_H1,Atom_11-11-111_C1,Atom_11-11-111_C2,Atom_11-11-111_H2",
        "oc_atoms_pos": "4,1,2,8",
        "random_id": "OPStestID-111-111-11",
    }
}


@pytest.mark.parametrize(
    "endpoints_config_file, pipeline_handlers_test_configs",
    [
        (
            "test_config_1.yml",
            [
                {
                    "pipeline_type": OC_PIPELINE,
                    "configs": {
                        "QC_LOG_TO_QC_JSON": {
                            endp_conf.FILE_SERVER_SUBDIR_KEY: "ocompchem",
                            endp_conf.TRIPLE_STORE_SPARQL_ENDPOINT_KEY: "ocompchem",
                            endp_conf.UPLOAD_TO_TRIPLE_STORE_KEY: [],
                            endp_conf.UPLOAD_TO_FILE_SERVER_KEY: ["qc_log"],
                        },
                        "OC_CSV_TO_OC_OWL": {
                            endp_conf.FILE_SERVER_SUBDIR_KEY: "ocompchem",
                            endp_conf.TRIPLE_STORE_SPARQL_ENDPOINT_KEY: "ocompchem",
                            endp_conf.UPLOAD_TO_TRIPLE_STORE_KEY: ["oc_owl"],
                            endp_conf.UPLOAD_TO_FILE_SERVER_KEY: [],
                        },
                    },
                },
                {
                    "pipeline_type": OS_PIPELINE,
                    "configs": {
                        "OS_CSV_TO_OS_OWL": {
                            endp_conf.FILE_SERVER_SUBDIR_KEY: "ospecies",
                            endp_conf.TRIPLE_STORE_SPARQL_ENDPOINT_KEY: "ospecies",
                            endp_conf.UPLOAD_TO_TRIPLE_STORE_KEY: ["os_owl"],
                            endp_conf.UPLOAD_TO_FILE_SERVER_KEY: [],
                        }
                    },
                },
                {
                    "pipeline_type": OMOPS_PIPELINE,
                    "configs": {
                        "OMINP_JSON_TO_OM_JSON": {
                            endp_conf.FILE_SERVER_SUBDIR_KEY: "omops",
                            endp_conf.TRIPLE_STORE_SPARQL_ENDPOINT_KEY: "omops",
                            endp_conf.UPLOAD_TO_TRIPLE_STORE_KEY: [],
                            endp_conf.UPLOAD_TO_FILE_SERVER_KEY: ["ominp_xyz"],
                        },
                        "OM_CSV_TO_OM_OWL": {
                            endp_conf.FILE_SERVER_SUBDIR_KEY: "omops",
                            endp_conf.TRIPLE_STORE_SPARQL_ENDPOINT_KEY: "omops",
                            endp_conf.UPLOAD_TO_TRIPLE_STORE_KEY: ["om_owl"],
                            endp_conf.UPLOAD_TO_FILE_SERVER_KEY: [],
                        },
                    },
                },
                {
                    "pipeline_type": OPS_PIPELINE,
                    "configs": {
                        "OPS_CSV_TO_OPS_OWL": {
                            endp_conf.FILE_SERVER_SUBDIR_KEY: "opsscan",
                            endp_conf.TRIPLE_STORE_SPARQL_ENDPOINT_KEY: "opsscan",
                            endp_conf.UPLOAD_TO_TRIPLE_STORE_KEY: ["ops_owl"],
                            endp_conf.UPLOAD_TO_FILE_SERVER_KEY: [],
                        }
                    },
                },
            ],
        ),
    ],
)
def test_abox_writer_setup(
    endpoints_config_file: str, pipeline_handlers_test_configs: List[Dict]
):
    print("========================================================")
    print("TEST CONFIG FILE: ", endpoints_config_file)
    print()
    print()

    config_file = os.path.join(TEST_CONFIG_FILES, endpoints_config_file)
    endpoints_config = endp_conf.get_endpoints_config_file(config_file=config_file)

    for test_item in pipeline_handlers_test_configs:
        pipeline_type = test_item["pipeline_type"]
        handlers_test_configs = test_item["configs"]

        pipeline = assemble_pipeline(
            pipeline_type=pipeline_type, endpoints_config=endpoints_config
        )

        for handler_name, test_configs in handlers_test_configs.items():
            handler = pipeline.get_handler_by_name(handler_name=handler_name)
            handler_configs = handler.endpoints_config  # type: ignore
            for key, value in test_configs.items():
                assert handler_configs[key] == value

    print("")

    print("========================================================")
    print()
    print()
