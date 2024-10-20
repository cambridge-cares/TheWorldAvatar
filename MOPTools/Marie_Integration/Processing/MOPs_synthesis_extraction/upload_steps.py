import uuid
import upload_utils as uputil
from OntoSyn_ontology import *



def upload_sonicate(standard_input, synthesis_client):
    standard_step                                       = standard_input["Sonicate"]
    vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value         = steps_preupload(standard_step, synthesis_client, vessel_list)
    sonication                                          = Sonicate(hasStepDuration=duration, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasVesselEnvironment=atmosphere)
    components                                          = [duration_value, duration, vessel, sonication]
    uputil.push_component_to_kg(components, synthesis_client)
    return sonication, vessel_list
