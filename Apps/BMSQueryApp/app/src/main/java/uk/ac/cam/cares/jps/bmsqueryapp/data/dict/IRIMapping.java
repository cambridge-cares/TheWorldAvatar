package uk.ac.cam.cares.jps.bmsqueryapp.data.dict;

import java.util.HashMap;

public class IRIMapping {
    HashMap<String, String> editableDataIRIToControlModeIRIMap = new HashMap<String, String>();
    HashMap<String, String> equipmentIRIToEditableDataIRIMap = new HashMap<String, String>();

    public IRIMapping() {
        //map that contains a data IRI and it's control IRI
        this.editableDataIRIToControlModeIRIMap.put("https://www.theworldavatar.com/kg/ontobms/V_VAV_E-7-1_FlowSP_CARES", "https://www.theworldavatar.com/kg/ontobms/V_VAV_E_7-1_Control_Mode");
        this.editableDataIRIToControlModeIRIMap.put("https://www.theworldavatar.com/kg/ontobms/V_VAV_E-7-2_FlowSP_CARES", "https://www.theworldavatar.com/kg/ontobms/V_VAV_E_7-2_Control_Mode");
        this.editableDataIRIToControlModeIRIMap.put("https://www.theworldavatar.com/kg/ontobms/V_CAV_E-7-7_FlowSP_CARES", "https://www.theworldavatar.com/kg/ontobms/V_CAV_E_7-7_Control_Mode");

        //map that contains an equipment IRI and it's data IRI
        this.equipmentIRIToEditableDataIRIMap.put("https://www.theworldavatar.com/kg/ontobms/VAV_E7_01_d00bd62d-9f86-4f01-8940-c52a62269f4c", "https://www.theworldavatar.com/kg/ontobms/V_VAV_E-7-1_FlowSP_CARES");
        this.equipmentIRIToEditableDataIRIMap.put("https://www.theworldavatar.com/kg/ontobms/VAV_E7_02_e4ac5287-26d8-4638-9ba2-74b9e9994814", "https://www.theworldavatar.com/kg/ontobms/V_VAV_E-7-2_FlowSP_CARES");
        this.equipmentIRIToEditableDataIRIMap.put("https://www.theworldavatar.com/kg/ontobms/CH-7-7_CAV_90f2f336-03f7-4c0e-80a0-6d85873e42a8", "https://www.theworldavatar.com/kg/ontobms/V_CAV_E-7-7_FlowSP_CARES");
    }

    public String getControlIRIFromEditableDataIRI(String dataIRI) {
        if (editableDataIRIToControlModeIRIMap.containsKey(dataIRI)) {
            String controlIRI = editableDataIRIToControlModeIRIMap.get(dataIRI);
            return controlIRI;
        } else {
            return null;
        }
    }

    public String getEditableDataIRIFromEquipmentIRI(String equipmentIRI) {
        if (equipmentIRIToEditableDataIRIMap.containsKey(equipmentIRI)) {
            String dataIRI = equipmentIRIToEditableDataIRIMap.get(equipmentIRI);
            return dataIRI;
        } else {
            return null;
        }
    }


}
