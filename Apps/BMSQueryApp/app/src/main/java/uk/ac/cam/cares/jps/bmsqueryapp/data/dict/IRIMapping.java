package uk.ac.cam.cares.jps.bmsqueryapp.data.dict;

import android.content.Context;

import java.util.HashMap;

import uk.ac.cam.cares.jps.bmsqueryapp.R;

public class IRIMapping {
    HashMap<String, String> editableDataIRIToControlModeIRIMap = new HashMap<String, String>();
    HashMap<String, String> equipmentIRIToEditableDataIRIMap = new HashMap<String, String>();

    public IRIMapping(Context context) {
        //map that contains a data IRI and it's control IRI
        this.editableDataIRIToControlModeIRIMap.put(context.getString(R.string.VAV_E7_1_flow_sp), context.getString(R.string.VAV_E7_1_control_mode));
        this.editableDataIRIToControlModeIRIMap.put(context.getString(R.string.VAV_E7_2_flow_sp), context.getString(R.string.VAV_E7_2_control_mode));
        this.editableDataIRIToControlModeIRIMap.put(context.getString(R.string.CH_7_7_CAV_flow_sp), context.getString(R.string.CH_7_7_CAV_control_mode));

        //map that contains an equipment IRI and it's data IRI
        this.equipmentIRIToEditableDataIRIMap.put(context.getString(R.string.VAV_E7_1_iri), context.getString(R.string.VAV_E7_1_flow_sp));
        this.equipmentIRIToEditableDataIRIMap.put(context.getString(R.string.VAV_E7_2_iri), context.getString(R.string.VAV_E7_2_flow_sp));
        this.equipmentIRIToEditableDataIRIMap.put(context.getString(R.string.CH_7_7_CAV_iri), context.getString(R.string.CH_7_7_CAV_flow_sp));
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
