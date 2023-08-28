package uk.ac.cam.cares.jps.addasset.model;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;

@HiltViewModel
public class AddAssetViewModel extends ViewModel {

    public Map<String, List<AssetPropertyDataModel>> getInputFieldsBySection() {
        return inputFieldsBySection;
    }

    private Map<String, List<AssetPropertyDataModel>> inputFieldsBySection = new LinkedHashMap<>();

    private List<String> mandatoryFieldKeys = Arrays.asList(TYPE, REFERENCE_LABEL, LOCATED_IN);
    // todo: not sure how to use this yet
    private List<String> dropDownFieldKeys = Arrays.asList(TYPE, ASSIGNED_TO, LOCATED_IN, SEAT_LOCATION, STORED_IN, VENDOR, MANUFACTURER, PURCHASE_REQUEST_NUMBER, PURCHASE_ORDER_NUMBER, INVOICE_NUMBER, DELIVERY_ORDER_NUMBER, ITEM_NAME, SERVICE_CODE, SERVICE_CATEGORY);
    private List<String> disallowInputForDropDown = Arrays.asList(TYPE, ASSIGNED_TO);
    private List<String> skippedFieldKeys = Arrays.asList(IRI, INVENTORY_ID, MANUFACTURE_URL);

    @Inject
    AddAssetViewModel() {
        BasicConfigurator.configure();
        initInputFieldsDataModel();
    }

    private void initInputFieldsDataModel() {
        inputFieldsBySection.put(BASIC, getInputFieldListForSection(basicInfoOrder));
        inputFieldsBySection.put(LOCATION, getInputFieldListForSection(locationInfoOrder));
        inputFieldsBySection.put(SUPPLIER, getInputFieldListForSection(supplierInfoOrder));
        inputFieldsBySection.put(ITEM, getInputFieldListForSection(itemInfoOrder));
        inputFieldsBySection.put(PURCHASE, getInputFieldListForSection(docLineInfoOrder));
    }

    private List<AssetPropertyDataModel> getInputFieldListForSection(List<String> fieldKeys) {
        List<AssetPropertyDataModel> results = new ArrayList<>();
        for (String key : fieldKeys) {
            if (skippedFieldKeys.contains(key)) {
                continue;
            }

            AssetPropertyDataModel assetPropertyDataModel;
            if (dropDownFieldKeys.contains(key)) {
                assetPropertyDataModel = new AssetPropertyDataModel(key, mandatoryFieldKeys.contains(key), disallowInputForDropDown.contains(key), AssetPropertyDataModel.ViewType.DROP_DOWN);
            } else {
                assetPropertyDataModel = new AssetPropertyDataModel(key, mandatoryFieldKeys.contains(key), disallowInputForDropDown.contains(key), AssetPropertyDataModel.ViewType.INPUT_FIELD);
            }
            results.add(assetPropertyDataModel);
        }
        return results;
    }
}
