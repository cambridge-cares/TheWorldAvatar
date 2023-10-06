package uk.ac.cam.cares.jps.addasset.model;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.model.AssetInfo;
import uk.ac.cam.cares.jps.data.otherinfo.OtherInfoRepository;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.model.building.Instance;

@HiltViewModel
public class AddAssetViewModel extends ViewModel {

    private final Logger LOGGER = Logger.getLogger(AddAssetViewModel.class);
    OtherInfoRepository otherInfoRepository;
    private String editMode = "add";


    public Map<String, List<String>> getInputFieldNamesBySection() {
        return inputFieldNamesBySection;
    }

    public Map<String, AssetPropertyDataModel> getInputFieldModels() {
        return inputFieldModels;
    }

    // define UI grouping
    private final Map<String, List<String>> inputFieldNamesBySection = new LinkedHashMap<>();
    private final Map<String, AssetPropertyDataModel> inputFieldModels = new HashMap<>();

    // use field keys to define each field view's appearance and behaviour
    private final List<String> mandatoryFieldKeys = Arrays.asList(TYPE, REFERENCE_LABEL, BUILDING);
    private final List<String> dropDownFieldKeys = Arrays.asList(TYPE, ASSIGNED_TO, VENDOR, MANUFACTURER, PURCHASE_REQUEST_NUMBER, PURCHASE_ORDER_NUMBER, INVOICE_NUMBER, DELIVERY_ORDER_NUMBER, ITEM_NAME, SERVICE_CODE, SERVICE_CATEGORY);
    private final List<String> locationFieldKeys = Arrays.asList(BUILDING, FACILITY, LOCATED_IN, SEAT_LOCATION, STORED_IN);
    private final List<String> dataSheetFieldKeys = Arrays.asList(SPEC_SHEET_FILE_URI, MANUAL_FILE_URI);
    private final List<String> disallowNewInstanceInputForDropDown = Arrays.asList(TYPE, FACILITY, LOCATED_IN, SEAT_LOCATION, STORED_IN);
    private final List<String> skippedFieldKeys = Arrays.asList(IRI, INVENTORY_ID, MANUFACTURE_URL);
    private final List<String> multiLineInputFieldKeys = Arrays.asList(ITEM_DESCRIPTION);

    @Inject
    AddAssetViewModel(OtherInfoRepository otherInfoRepository) {
        BasicConfigurator.configure();
        initInputFieldsDataModel();

        this.otherInfoRepository = otherInfoRepository;
    }

    private void initInputFieldsDataModel() {
        inputFieldNamesBySection.put(BASIC_SECTION_TITLE, initFields(basicInfoOrder));
        inputFieldNamesBySection.put(LOCATION_SECTION_TITLE, initFields(locationInfoOrder));
        inputFieldNamesBySection.put(SUPPLIER_SECTION_TITLE, initFields(supplierInfoOrder));
        inputFieldNamesBySection.put(PURCHASE_SECTION_TITLE, initFields(docLineInfoOrder));
        inputFieldNamesBySection.put(ITEM_SECTION_TITLE, initFields(itemInfoOrder));

        // assume only 1 spec sheet and 1 manual
        inputFieldNamesBySection.put(SPEC_SHEET_SECTION_TITLE, initFields(Arrays.asList(SPEC_SHEET_PAGE_NO, SPEC_SHEET_FILE_URI)));
        inputFieldNamesBySection.put(MANUAL_SECTION_TITLE, initFields(Arrays.asList(MANUAL_URL, MANUAL_FILE_URI)));
    }

    private List<String> initFields(List<String> fieldKeys) {
        List<String> fieldNames = new ArrayList<>();
        for (String key : fieldKeys) {
            if (skippedFieldKeys.contains(key)) {
                continue;
            }

            AssetPropertyDataModel assetPropertyDataModel;
            if (dropDownFieldKeys.contains(key)) {
                assetPropertyDataModel = new DropDownDataModel(key);
            } else if (dataSheetFieldKeys.contains(key)) {
                assetPropertyDataModel = new DataFileDataModel(key);
            } else if (locationFieldKeys.contains(key)) {
                assetPropertyDataModel = new LocationDropDownDataModel(key);
            } else {
                assetPropertyDataModel = new AssetPropertyDataModel(key);
            }

            assetPropertyDataModel.setRequired(mandatoryFieldKeys.contains(key));
            assetPropertyDataModel.setMultiLine(multiLineInputFieldKeys.contains(key));

            inputFieldModels.put(key, assetPropertyDataModel);
            fieldNames.add(key);
        }
        return fieldNames;
    }

    public void requestAllDropDownOptionsFromRepository() {
        Map<String, RepositoryCallback<Map<String, String>>> callbacks = new HashMap<>();
        for (String key : otherInfoFromAssetAgentKeys) {
            if (key.equals(BUILDING)) {
                continue;
            }
            callbacks.put(key, getRepositoryCallbackForKey(key));
        }

        RepositoryCallback<List<Instance>> locationCallback = getLocationCallback();
        otherInfoRepository.getAllOtherInfo(callbacks, locationCallback);
    }

    private RepositoryCallback<Map<String, String>> getRepositoryCallbackForKey(String key) {
        return new RepositoryCallback<Map<String, String>>() {
            @Override
            public void onSuccess(Map<String, String> result) {
                ((DropDownDataModel) inputFieldModels.get(key)).getMutableLabelsToIri().postValue(result);
                ((DropDownDataModel) inputFieldModels.get(key)).constructIriToLabel();
            }

            @Override
            public void onFailure(Throwable error) {
                // do nothing, update failed, or notify user?
            }
        };
    }

    private RepositoryCallback<List<Instance>> getLocationCallback() {
        return new RepositoryCallback<List<Instance>>() {
            @Override
            public void onSuccess(List<Instance> result) {
                ((LocationDropDownDataModel) inputFieldModels.get(BUILDING)).getMutableInstances().postValue(result);
            }

            @Override
            public void onFailure(Throwable error) {

            }
        };
    }

    public boolean checkMissingInput() {
        boolean hasError = false;
        for (String key : mandatoryFieldKeys) {
            if (inputFieldModels.get(key).getFieldValue().isEmpty()) {
                inputFieldModels.get(key).getIsMissingField().setValue(true);
                hasError = true;
            }
        }
        return hasError;
    }

    public boolean checkDisallowNewInstanceInputField() {
        boolean hasError = false;
        for (String key : disallowNewInstanceInputForDropDown) {
            // if field value is empty, then no need to check whether have a matched iri
            if (!inputFieldModels.get(key).getFieldValue().isEmpty() && ((DropDownDataModel)inputFieldModels.get(key)).getValueIri().isEmpty()) {
                ((DropDownDataModel) inputFieldModels.get(key)).getShowDisallowError().setValue(true);
                hasError = true;
            }
        }
        return hasError;
    }

    public AssetInfo getAssetInfo() {
        AssetInfo assetInfo = new AssetInfo();
        for (AssetPropertyDataModel field : inputFieldModels.values()) {
            if (field instanceof DataFileDataModel) {
                // todo: data sheet data
            } else {
                assetInfo.addProperties(field.getFieldName(), field.getFieldValue());
            }
        }

        return assetInfo;
    }

    public void initFieldsWithAssetInfo(AssetInfo assetInfo) {
        for (String key : assetInfo.getProperties().keySet()) {
            // todo: haven't accounted for data sheet type
            if (!inputFieldModels.containsKey(key)) {
                continue;
            }
            AssetPropertyDataModel property = inputFieldModels.get(key);
            property.setFieldValue(assetInfo.getProperty(key));
        }
    }

    public String getEditMode() {
        return editMode;
    }

    public void setEditMode(String editMode) {
        this.editMode = editMode;
    }
}
