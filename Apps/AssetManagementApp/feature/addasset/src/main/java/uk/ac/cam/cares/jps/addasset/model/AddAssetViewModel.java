package uk.ac.cam.cares.jps.addasset.model;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import android.content.Context;

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
import uk.ac.cam.cares.jps.data.assetinfo.AssetInfoRepository;
import uk.ac.cam.cares.jps.model.AssetInfo;
import uk.ac.cam.cares.jps.data.otherinfo.OtherInfoRepository;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.model.building.Instance;

@HiltViewModel
public class AddAssetViewModel extends ViewModel {

    private final Logger LOGGER = Logger.getLogger(AddAssetViewModel.class);
    OtherInfoRepository otherInfoRepository;
    AssetInfoRepository assetInfoRepository;
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
    private final List<String> dropDownFieldKeys = Arrays.asList(TYPE, ASSIGNED_TO, VENDOR, MANUFACTURER, SERVICE_CODE, SERVICE_CATEGORY);
    private final List<String> locationFieldKeys = Arrays.asList(BUILDING, FACILITY, ROOM, WORKSPACE, STORED_IN);
    private final List<String> dataSheetFieldKeys = Arrays.asList(SPEC_SHEET_FILE, MANUAL_FILE);
    private final List<String> disallowNewInstanceInputForDropDown = Arrays.asList(TYPE, FACILITY, ROOM, WORKSPACE, STORED_IN);
    private final List<String> skippedFieldKeys = Arrays.asList(IRI, INVENTORY_ID, MANUFACTURE_URL);
    private final List<String> multiLineInputFieldKeys = Arrays.asList(COMMENTS, SPEC_SHEET_COMMENT, MANUAL_COMMENT);

    @Inject
    AddAssetViewModel(OtherInfoRepository otherInfoRepository, AssetInfoRepository assetInfoRepository) {
        BasicConfigurator.configure();
        initInputFieldsDataModel();

        this.otherInfoRepository = otherInfoRepository;
        this.assetInfoRepository = assetInfoRepository;
    }

    private void initInputFieldsDataModel() {
        inputFieldNamesBySection.put(BASIC_SECTION_TITLE, initFields(basicInfoOrder));
        inputFieldNamesBySection.put(LOCATION_SECTION_TITLE, initFields(locationInfoOrder));
        inputFieldNamesBySection.put(SUPPLIER_SECTION_TITLE, initFields(supplierInfoOrder));
        inputFieldNamesBySection.put(PURCHASE_SECTION_TITLE, initFields(docLineInfoOrder));
        inputFieldNamesBySection.put(ITEM_SECTION_TITLE, initFields(itemInfoOrder));
        inputFieldNamesBySection.put(SPEC_SHEET_SECTION_TITLE, initFields(specSheetInfoOrder));
        inputFieldNamesBySection.put(MANUAL_SECTION_TITLE, initFields(manualInfoOrder));
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

        if (((LocationDropDownDataModel)inputFieldModels.get(BUILDING)).getMatched() != null) {
            if (inputFieldModels.get(FACILITY).getFieldValue().isEmpty()) {
                inputFieldModels.get(FACILITY).getIsMissingField().setValue(true);
                hasError = true;
            }

            if (inputFieldModels.get(ROOM).getFieldValue().isEmpty()) {
                inputFieldModels.get(ROOM).getIsMissingField().setValue(true);
                hasError = true;
            }
        }
        return hasError;
    }

    public boolean checkDisallowNewInstanceInputField() {
        boolean hasError = false;
        for (String key : disallowNewInstanceInputForDropDown) {
            // if field value is empty, then no need to check whether have a matched iri
            AssetPropertyDataModel property = inputFieldModels.get(key);
            if (!property.getFieldValue().isEmpty()) {
                if (property instanceof  LocationDropDownDataModel && ((LocationDropDownDataModel) property).getMatched() == null) {
                    ((LocationDropDownDataModel) property).getShowDisallowError().setValue(true);
                    hasError = true;
                } else if (((DropDownDataModel) property).getMatched() == null) {
                    ((DropDownDataModel) property).getShowDisallowError().setValue(true);
                    hasError = true;
                }
            }
        }
        return hasError;
    }

    public AssetInfo collectAssetInfoFromUI(Context context) {
        AssetInfo assetInfo = new AssetInfo();
        for (AssetPropertyDataModel field : inputFieldModels.values()) {
            if (field instanceof DataFileDataModel) {
                assetInfo.addProperties(field.getFieldName(), ((DataFileDataModel) field).getFieldValue(context));

                if (field.getFieldName().equals(SPEC_SHEET_FILE)) {
                    assetInfo.addProperties(SPEC_SHEET_FILE_URI, ((DataFileDataModel) field).getFilePath().toString());
                } else if (field.getFieldName().equals(MANUAL_FILE)) {
                    assetInfo.addProperties(MANUAL_FILE_URI, ((DataFileDataModel) field).getFilePath().toString());
                }
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

    public void setAssetInfoToRepo(AssetInfo assetInfo) {
        assetInfoRepository.setAssetInfo(assetInfo);
    }

    public AssetInfo getAssetInfoFromRepo() {
        return assetInfoRepository.getAssetInfo();
    }
}
