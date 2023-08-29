package uk.ac.cam.cares.jps.addasset.model;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;

@HiltViewModel
public class AddAssetViewModel extends ViewModel {

    public Map<String, List<AssetPropertyDataModel>> getInputFieldsBySection() {
        return inputFieldsBySection;
    }

    private final Map<String, List<AssetPropertyDataModel>> inputFieldsBySection = new LinkedHashMap<>();

    public final List<String> INPUT_TEXT_SECTIONS = new ArrayList<>();
    public final List<String> DATA_SHEET_SECTIONS = new ArrayList<>();

    private final List<String> mandatoryFieldKeys = Arrays.asList(TYPE, REFERENCE_LABEL, LOCATED_IN);
    private final List<String> dropDownFieldKeys = Arrays.asList(TYPE, ASSIGNED_TO, LOCATED_IN, SEAT_LOCATION, STORED_IN, VENDOR, MANUFACTURER, PURCHASE_REQUEST_NUMBER, PURCHASE_ORDER_NUMBER, INVOICE_NUMBER, DELIVERY_ORDER_NUMBER, ITEM_NAME, SERVICE_CODE, SERVICE_CATEGORY);
    private final List<String> disallowInputForDropDown = Arrays.asList(TYPE, ASSIGNED_TO);
    private final List<String> skippedFieldKeys = Arrays.asList(IRI, INVENTORY_ID, MANUFACTURE_URL);

    @Inject
    AddAssetViewModel() {
        BasicConfigurator.configure();
        initInputFieldsDataModel();
        initInputTextSections();
        initDataSheetSections();
    }

    private void initInputFieldsDataModel() {
        inputFieldsBySection.put(BASIC, getInputFieldListForSection(basicInfoOrder));
        inputFieldsBySection.put(LOCATION, getInputFieldListForSection(locationInfoOrder));
        inputFieldsBySection.put(SUPPLIER, getInputFieldListForSection(supplierInfoOrder));
        inputFieldsBySection.put(PURCHASE, getInputFieldListForSection(docLineInfoOrder));
        inputFieldsBySection.put(ITEM, getInputFieldListForSection(itemInfoOrder));

        // todo: incorrect fields data!
        inputFieldsBySection.put(SPEC_SHEET, getInputFieldListForSection(Collections.singletonList(SPEC_SHEET)));
        inputFieldsBySection.put(MANUAL, getInputFieldListForSection(Collections.singletonList(MANUAL)));
    }

    private void initInputTextSections() {
        INPUT_TEXT_SECTIONS.addAll(basicInfoOrder);
        INPUT_TEXT_SECTIONS.addAll(locationInfoOrder);
        INPUT_TEXT_SECTIONS.addAll(supplierInfoOrder);
        INPUT_TEXT_SECTIONS.addAll(docLineInfoOrder);
        INPUT_TEXT_SECTIONS.addAll(itemInfoOrder);
    }

    private void initDataSheetSections() {
        DATA_SHEET_SECTIONS.add(SPEC_SHEET);
        DATA_SHEET_SECTIONS.add(MANUAL);
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
