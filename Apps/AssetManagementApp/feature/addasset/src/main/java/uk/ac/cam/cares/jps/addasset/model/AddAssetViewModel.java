package uk.ac.cam.cares.jps.addasset.model;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.OtherInfoModel;
import uk.ac.cam.cares.jps.data.OtherInfoRepository;
import uk.ac.cam.cares.jps.data.RepositoryCallback;

@HiltViewModel
public class AddAssetViewModel extends ViewModel {
    OtherInfoRepository repository;

    public Map<String, List<AssetPropertyDataModel>> getInputFieldsBySection() {
        return inputFieldsBySection;
    }

    private final Map<String, List<AssetPropertyDataModel>> inputFieldsBySection = new LinkedHashMap<>();
    private final Map<String, MutableLiveData<List<OtherInfoModel>>> dropDownOptionsMap = new HashMap<>();

    public final List<String> INPUT_TEXT_SECTIONS = new ArrayList<>();
    public final List<String> DATA_SHEET_SECTIONS = new ArrayList<>();

    private final List<String> mandatoryFieldKeys = Arrays.asList(TYPE, REFERENCE_LABEL, LOCATED_IN);
    private final List<String> dropDownFieldKeys = Arrays.asList(TYPE, ASSIGNED_TO, LOCATED_IN, SEAT_LOCATION, STORED_IN, VENDOR, MANUFACTURER, PURCHASE_REQUEST_NUMBER, PURCHASE_ORDER_NUMBER, INVOICE_NUMBER, DELIVERY_ORDER_NUMBER, ITEM_NAME, SERVICE_CODE, SERVICE_CATEGORY);
    private final List<String> disallowInputForDropDown = Arrays.asList(TYPE, ASSIGNED_TO);
    private final List<String> skippedFieldKeys = Arrays.asList(IRI, INVENTORY_ID, MANUFACTURE_URL);
    private final List<String> multiLineInputFieldKeys = Arrays.asList(ITEM_DESCRIPTION, SERVICE_CODE_DESCRIPTION, SERVICE_CATEGORY_DESCRIPTION);

    @Inject
    AddAssetViewModel(OtherInfoRepository repository) {
        BasicConfigurator.configure();
        initInputFieldsDataModel();
        initInputTextSections();
        initDataSheetSections();
        initDropDownLiveData();

        this.repository = repository;
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

    private void initInputTextSections() {
        INPUT_TEXT_SECTIONS.addAll(basicInfoOrder);
        INPUT_TEXT_SECTIONS.addAll(locationInfoOrder);
        INPUT_TEXT_SECTIONS.addAll(supplierInfoOrder);
        INPUT_TEXT_SECTIONS.addAll(docLineInfoOrder);
        INPUT_TEXT_SECTIONS.addAll(itemInfoOrder);
    }

    private void initDataSheetSections() {
        DATA_SHEET_SECTIONS.add(SPEC_SHEET_SECTION_TITLE);
        DATA_SHEET_SECTIONS.add(MANUAL_SECTION_TITLE);
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
                assetPropertyDataModel = new AssetPropertyDataModel(key, AssetPropertyDataModel.ViewType.DROP_DOWN);
            } else {
                assetPropertyDataModel = new AssetPropertyDataModel(key, AssetPropertyDataModel.ViewType.INPUT_FIELD);
            }

            assetPropertyDataModel.setDisallowInput(disallowInputForDropDown.contains(key));
            assetPropertyDataModel.setRequired(mandatoryFieldKeys.contains(key));
            assetPropertyDataModel.setMultiLine(multiLineInputFieldKeys.contains(key));

            results.add(assetPropertyDataModel);
        }
        return results;
    }

    public void requestAllDropDownOptionsFromRepository() {
//        repository.getTypes(getRepositoryCallbackForKey(TYPE));
        Map<String, RepositoryCallback> callbacks = new HashMap<>();
        for (String key: otherInfoFromAssetAgentKeys) {
            callbacks.put(key, getRepositoryCallbackForKey(key));
        }

        repository.getAllOtherInfo(callbacks);
    }

    private RepositoryCallback getRepositoryCallbackForKey(String key) {
        return new RepositoryCallback() {
            @Override
            public void onSuccess(Object result) {
                dropDownOptionsMap.get(key).postValue((List<OtherInfoModel>) result);
            }

            @Override
            public void onFailure(Throwable error) {
                // do nothing, update failed, or notify user?
            }
        };
    }

    public MutableLiveData<List<OtherInfoModel>> getDropDownLiveDataByKey(String key) {
        return dropDownOptionsMap.get(key);
    }
}
