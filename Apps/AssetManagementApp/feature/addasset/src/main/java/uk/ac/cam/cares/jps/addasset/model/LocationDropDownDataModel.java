package uk.ac.cam.cares.jps.addasset.model;

import androidx.lifecycle.MutableLiveData;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.model.building.Instance;

public class LocationDropDownDataModel extends DropDownDataModel{

    LocationDropDownDataModel(String fieldName) {
        super(fieldName);
    }

    MutableLiveData<List<Instance>> mutableInstances = new MutableLiveData<>(new ArrayList<>());

    public MutableLiveData<List<Instance>> getMutableInstances() {
        return mutableInstances;
    }

    public Instance getMatched(String input) {
        for (int i = 0; i < mutableInstances.getValue().size(); i++) {
            Instance instance = mutableInstances.getValue().get(i);
            if (instance.getLabel().equals(input)) {
                return instance;
            }
        }
        return null;
    }

    @Override
    public Instance getMatched() {
        return getMatched(getFieldValue());
    }
}
