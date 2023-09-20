package uk.ac.cam.cares.jps.addasset.model;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.AssetInfo;
import uk.ac.cam.cares.jps.data.OtherInfoRepository;
import uk.ac.cam.cares.jps.data.RepositoryCallback;

@HiltViewModel
public class AddAssetViewModel extends ViewModel {

    private final Logger LOGGER = Logger.getLogger(AddAssetViewModel.class);
    OtherInfoRepository otherInfoRepository;


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
    private final List<String> mandatoryFieldKeys = Arrays.asList(TYPE, REFERENCE_LABEL, LOCATED_IN);
    private final List<String> dropDownFieldKeys = Arrays.asList(TYPE, ASSIGNED_TO, LOCATED_IN, SEAT_LOCATION, STORED_IN, VENDOR, MANUFACTURER, PURCHASE_REQUEST_NUMBER, PURCHASE_ORDER_NUMBER, INVOICE_NUMBER, DELIVERY_ORDER_NUMBER, ITEM_NAME, SERVICE_CODE, SERVICE_CATEGORY);
    private final List<String> dataSheetFieldKeys = Arrays.asList(SPEC_SHEET_SECTION_TITLE, MANUAL_SECTION_TITLE);
    private final List<String> disallowNewInstanceInputForDropDown = Arrays.asList(TYPE, ASSIGNED_TO);
    private final List<String> skippedFieldKeys = Arrays.asList(IRI, INVENTORY_ID, MANUFACTURE_URL);
    private final List<String> multiLineInputFieldKeys = Arrays.asList(ITEM_DESCRIPTION, SERVICE_CODE_DESCRIPTION, SERVICE_CATEGORY_DESCRIPTION);

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
        inputFieldNamesBySection.put(SPEC_SHEET_SECTION_TITLE, initFields(Collections.singletonList(SPEC_SHEET_SECTION_TITLE)));
        inputFieldNamesBySection.put(MANUAL_SECTION_TITLE, initFields(Collections.singletonList(MANUAL_SECTION_TITLE)));
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
                assetPropertyDataModel = new DataSheetDataModel(key);
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

        otherInfoRepository.getAllOtherInfo(callbacks);
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
            // if field value is empty, then no need to check whether have a mathced iri
            if (!inputFieldModels.get(key).getFieldValue().isEmpty() && ((DropDownDataModel)inputFieldModels.get(key)).getValueIri().isEmpty()) {
                ((DropDownDataModel) inputFieldModels.get(key)).getShowDisallowError().setValue(true);
                hasError = true;
            }
        }
        return hasError;
    }

    // todo: show summary

    public AssetInfo getAssetInfo() {
        AssetInfo assetInfo = new AssetInfo();
        for (AssetPropertyDataModel field : inputFieldModels.values()) {
            if (field instanceof DropDownDataModel) {
                assetInfo.addProperties(field.getFieldName(), ((DropDownDataModel) field).getValueIri());
            } else if (field instanceof DataSheetDataModel) {
                // todo
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
}
