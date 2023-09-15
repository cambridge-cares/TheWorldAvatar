package uk.ac.cam.cares.jps.addasset.model;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import android.util.Pair;

import androidx.lifecycle.MutableLiveData;
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
import uk.ac.cam.cares.jps.data.AssetInfoRepository;
import uk.ac.cam.cares.jps.data.OtherInfoModel;
import uk.ac.cam.cares.jps.data.OtherInfoRepository;
import uk.ac.cam.cares.jps.data.RepositoryCallback;

@HiltViewModel
public class AddAssetViewModel extends ViewModel {

    private final Logger LOGGER = Logger.getLogger(AddAssetViewModel.class);
    OtherInfoRepository otherInfoRepository;
    AssetInfoRepository assetInfoRepository;

    public Map<String, List<AssetPropertyDataModel>> getInputFieldsBySection() {
        return inputFieldsBySection;
    }

    private final Map<String, List<AssetPropertyDataModel>> inputFieldsBySection = new LinkedHashMap<>();
    private final Map<String, MutableLiveData<List<OtherInfoModel>>> dropDownOptionsMap = new HashMap<>();


    // use field keys to define each field view's appearance and behaviour
    private final List<String> mandatoryFieldKeys = Arrays.asList(TYPE, REFERENCE_LABEL, LOCATED_IN);
    private final List<String> dropDownFieldKeys = Arrays.asList(TYPE, ASSIGNED_TO, LOCATED_IN, SEAT_LOCATION, STORED_IN, VENDOR, MANUFACTURER, PURCHASE_REQUEST_NUMBER, PURCHASE_ORDER_NUMBER, INVOICE_NUMBER, DELIVERY_ORDER_NUMBER, ITEM_NAME, SERVICE_CODE, SERVICE_CATEGORY);
    private final List<String> dataSheetFieldKeys = Arrays.asList(SPEC_SHEET_SECTION_TITLE, MANUAL_SECTION_TITLE);
    private final List<String> disallowInputForDropDown = Arrays.asList(TYPE, ASSIGNED_TO);
    private final List<String> skippedFieldKeys = Arrays.asList(IRI, INVENTORY_ID, MANUFACTURE_URL);
    private final List<String> multiLineInputFieldKeys = Arrays.asList(ITEM_DESCRIPTION, SERVICE_CODE_DESCRIPTION, SERVICE_CATEGORY_DESCRIPTION);

    @Inject
    AddAssetViewModel(OtherInfoRepository otherInfoRepository, AssetInfoRepository assetInfoRepository) {
        BasicConfigurator.configure();
        initInputFieldsDataModel();
        initDropDownLiveData();

        this.otherInfoRepository = otherInfoRepository;
        this.assetInfoRepository = assetInfoRepository;
    }

    private void initInputFieldsDataModel() {
        inputFieldsBySection.put(BASIC_SECTION_TITLE, getInputFieldListForSection(basicInfoOrder));
        inputFieldsBySection.put(LOCATION_SECTION_TITLE, getInputFieldListForSection(locationInfoOrder));
        inputFieldsBySection.put(SUPPLIER_SECTION_TITLE, getInputFieldListForSection(supplierInfoOrder));
        inputFieldsBySection.put(PURCHASE_SECTION_TITLE, getInputFieldListForSection(docLineInfoOrder));
        inputFieldsBySection.put(ITEM_SECTION_TITLE, getInputFieldListForSection(itemInfoOrder));

        // assume only 1 spec sheet and 1 manual
        inputFieldsBySection.put(SPEC_SHEET_SECTION_TITLE, getInputFieldListForSection(Collections.singletonList(SPEC_SHEET_SECTION_TITLE)));
        inputFieldsBySection.put(MANUAL_SECTION_TITLE, getInputFieldListForSection(Collections.singletonList(MANUAL_SECTION_TITLE)));
    }

    private void initDropDownLiveData() {
        for (String key : dropDownFieldKeys) {
            dropDownOptionsMap.put(key, new MutableLiveData<>());
        }
    }

    private List<AssetPropertyDataModel> getInputFieldListForSection(List<String> fieldKeys) {
        List<AssetPropertyDataModel> results = new ArrayList<>();
        for (String key : fieldKeys) {
            if (skippedFieldKeys.contains(key)) {
                continue;
            }

            AssetPropertyDataModel assetPropertyDataModel;
            if (dropDownFieldKeys.contains(key)) {
                assetPropertyDataModel = new DropDownDataModel(key);
                ((DropDownDataModel) assetPropertyDataModel).setDisallowNewItem(disallowInputForDropDown.contains(key));
            } else if (dataSheetFieldKeys.contains(key)) {
                assetPropertyDataModel = new DataSheetDataModel(key);
            } else {
                assetPropertyDataModel = new AssetPropertyDataModel(key);
            }

            assetPropertyDataModel.setRequired(mandatoryFieldKeys.contains(key));
            assetPropertyDataModel.setMultiLine(multiLineInputFieldKeys.contains(key));

            results.add(assetPropertyDataModel);
        }
        return results;
    }

    public void requestAllDropDownOptionsFromRepository() {
        Map<String, RepositoryCallback> callbacks = new HashMap<>();
        for (String key : otherInfoFromAssetAgentKeys) {
            callbacks.put(key, getRepositoryCallbackForKey(key));
        }

        otherInfoRepository.getAllOtherInfo(callbacks);
    }

    private RepositoryCallback getRepositoryCallbackForKey(String key) {
        return new RepositoryCallback() {
            @Override
            public void onSuccess(Object result) {
                Pair<String, Integer> indexes = findIndexForFieldByKey(key);
                ((DropDownDataModel) inputFieldsBySection.get(indexes.first).get(indexes.second)).setLabelsToIri((List<OtherInfoModel>) result);
                dropDownOptionsMap.get(key).postValue((List<OtherInfoModel>) result);
            }

            @Override
            public void onFailure(Throwable error) {
                // do nothing, update failed, or notify user?
            }
        };
    }

    private Pair<String, Integer> findIndexForFieldByKey(String key) throws RuntimeException {
        if (basicInfoOrder.contains(key)) {
            return new Pair<>(BASIC_SECTION_TITLE, basicInfoOrder.indexOf(key));
        } else if (locationInfoOrder.contains(key)) {
            return new Pair<>(LOCATION_SECTION_TITLE, locationInfoOrder.indexOf(key));
        } else if (supplierInfoOrder.contains(key)) {
            return new Pair<>(SUPPLIER_SECTION_TITLE, supplierInfoOrder.indexOf(key));
        } else if (docLineInfoOrder.contains(key)) {
            return new Pair<>(PURCHASE_SECTION_TITLE, docLineInfoOrder.indexOf(key));
        }

        LOGGER.error("Key: " + key + "not found in any sections. Should not reach here!");
        throw new RuntimeException("Key: " + key + "not found in any sections. Should not reach here!");
    }

    public MutableLiveData<List<OtherInfoModel>> getDropDownLiveDataByKey(String key) {
        return dropDownOptionsMap.get(key);
    }

    public void addNewAsset() {
        AssetInfo assetInfo = new AssetInfo();
        for (List<AssetPropertyDataModel> fields : inputFieldsBySection.values()) {
            fields.forEach(field -> {

                if (field instanceof DropDownDataModel) {
                    assetInfo.addProperties(field.getFieldName(), ((DropDownDataModel) field).getValueIri());
                } else if (field instanceof DataSheetDataModel) {
                    // todo
                } else {
                    assetInfo.addProperties(field.getFieldName(), field.getFieldValue());
                }
            });
        }

        assetInfoRepository.createNewAsset(assetInfo);
    }
}
