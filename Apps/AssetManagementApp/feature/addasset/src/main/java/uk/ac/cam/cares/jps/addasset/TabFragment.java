package uk.ac.cam.cares.jps.addasset;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.BUILDING;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.FACILITY;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.LOCATED_IN;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.LOCATION_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.MANUAL_FILE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.MANUAL_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SEAT_LOCATION;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SPEC_SHEET_FILE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SPEC_SHEET_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.STORED_IN;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.addasset.model.AddAssetViewModel;
import uk.ac.cam.cares.jps.addasset.model.AssetPropertyDataModel;
import uk.ac.cam.cares.jps.addasset.model.DataFileDataModel;
import uk.ac.cam.cares.jps.addasset.model.DropDownDataModel;
import uk.ac.cam.cares.jps.addasset.model.LocationDropDownDataModel;
import uk.ac.cam.cares.jps.addasset.view.DataFileItemView;
import uk.ac.cam.cares.jps.addasset.view.PropertyAutoCompleteTextView;
import uk.ac.cam.cares.jps.addasset.view.PropertyBaseInputTextView;
import uk.ac.cam.cares.jps.addasset.view.PropertyGeneralInputTextView;
import uk.ac.cam.cares.jps.model.building.Instance;
import uk.ac.cam.cares.jps.utils.FileUtils;

@AndroidEntryPoint
public class TabFragment extends Fragment {
    AddAssetViewModel viewModel;
    List<String> sections;
    private Logger LOGGER = Logger.getLogger(TabFragment.class);

    private final int OPEN_SPEC_SHEET_REQUEST = 98754;
    private final int OPEN_MANUAL_REQUEST = 78945;
    private final int PERMISSION_REQUEST = 15678;

    private ActivityResultLauncher<String> requestPermissionLauncher;

    TabFragment(List<String> sections) {
        this.sections = sections;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BasicConfigurator.configure();
        ScrollView scrollView = new ScrollView(inflater.getContext());
        scrollView.setLayoutParams(new ViewGroup.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT
        ));

        LinearLayout root = new LinearLayout(inflater.getContext());
        root.setOrientation(LinearLayout.VERTICAL);

        viewModel = new ViewModelProvider(requireActivity()).get(AddAssetViewModel.class);

        for (String section : sections) {
            View sectionView;
            if (Arrays.asList(SPEC_SHEET_SECTION_TITLE, MANUAL_SECTION_TITLE).contains(section)) {
                sectionView = createDataSheetSection(inflater, section);
            } else if (section.equals(LOCATION_SECTION_TITLE)) {
                sectionView = createLocationSection(inflater, section);
            } else {
                sectionView = createInputTextSection(inflater, section);
            }
            root.addView(sectionView);
        }

        scrollView.addView(root);

        requestPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
            if (isGranted) {
                Toast.makeText(requireContext(), R.string.storage_permission_granted,Toast.LENGTH_SHORT)
                        .show();
            } else {
                Toast.makeText(requireContext(), R.string.failed_to_get_storage_permission,
                        Toast.LENGTH_LONG).show();
            }
        });
        return scrollView;
    }

    private View createInputTextSection(LayoutInflater inflater, String section) {
        View sectionView = inflater.inflate(R.layout.view_input_text_section, null);
        ((TextView) sectionView.findViewById(R.id.section_label)).setText(section);

        LinearLayout linearLayout = sectionView.findViewById(R.id.linear_layout);
        for (String fieldName : viewModel.getInputFieldNamesBySection().get(section)) {
            AssetPropertyDataModel property = viewModel.getInputFieldModels().get(fieldName);
            PropertyBaseInputTextView inputText;
            if (property instanceof DropDownDataModel) {
                inputText = new PropertyAutoCompleteTextView(requireContext(), (DropDownDataModel) property);

                ((DropDownDataModel) property).getMutableLabelsToIri().observe(this.getViewLifecycleOwner(), labelsToIriMap -> {
                    ((PropertyAutoCompleteTextView) inputText).updateAdapterList(((DropDownDataModel) property).getOrderedOptionList());
                });

                ((DropDownDataModel) property).getShowDisallowError().observe(this.getViewLifecycleOwner(), showDisallowError -> {
                    if (showDisallowError) {
                        inputText.setInputLayoutError(getResources().getText(R.string.not_allow_new_instance));
                    } else {
                        inputText.setInputLayoutError(null);
                    }
                });
            } else {
                inputText = new PropertyGeneralInputTextView(requireContext(), property);
            }

            property.getIsMissingField().observe(this.getViewLifecycleOwner(), isMissing -> {
                if (isMissing) {
                    inputText.setInputLayoutError(getResources().getText(R.string.field_is_required));
                } else {
                    inputText.setInputLayoutError(null);
                }
            });

            linearLayout.addView(inputText);
        }
        return sectionView;
    }

    private void setIsMissingObserver(AssetPropertyDataModel property, PropertyBaseInputTextView inputText) {
        property.getIsMissingField().observe(this.getViewLifecycleOwner(), isMissing -> {
            if (isMissing) {
                inputText.setInputLayoutError(getResources().getText(R.string.field_is_required));
            } else {
                inputText.setInputLayoutError(null);
            }
        });
    }

    private View createLocationSection(LayoutInflater inflater, String section) {
        View sectionView = inflater.inflate(R.layout.view_input_text_section, null);
        ((TextView) sectionView.findViewById(R.id.section_label)).setText(section);
        LinearLayout linearLayout = sectionView.findViewById(R.id.linear_layout);

        LocationDropDownDataModel buildingProperty = (LocationDropDownDataModel) viewModel.getInputFieldModels().get(BUILDING);
        PropertyAutoCompleteTextView buildingInputText = new PropertyAutoCompleteTextView(requireContext(), buildingProperty);
        buildingProperty.getMutableInstances().observe(this.getViewLifecycleOwner(), buildings -> ((PropertyAutoCompleteTextView) buildingInputText).updateAdapterList(buildings));
        setIsMissingObserver(buildingProperty, buildingInputText);
        linearLayout.addView(buildingInputText);

        LocationDropDownDataModel facilityProperty = (LocationDropDownDataModel) viewModel.getInputFieldModels().get(FACILITY);
        PropertyAutoCompleteTextView facilityInputText = new PropertyAutoCompleteTextView(requireContext(), facilityProperty);
        facilityProperty.getMutableInstances().observe(this.getViewLifecycleOwner(), facilities -> ((PropertyAutoCompleteTextView) facilityInputText).updateAdapterList(facilities));
        setIsMissingObserver(facilityProperty, facilityInputText);
        linearLayout.addView(facilityInputText);

        LocationDropDownDataModel roomProperty = (LocationDropDownDataModel) viewModel.getInputFieldModels().get(LOCATED_IN);
        PropertyAutoCompleteTextView roomInputText = new PropertyAutoCompleteTextView(requireContext(), roomProperty);
        roomProperty.getMutableInstances().observe(this.getViewLifecycleOwner(), rooms -> ((PropertyAutoCompleteTextView) roomInputText).updateAdapterList(rooms));
        setIsMissingObserver(roomProperty, roomInputText);
        linearLayout.addView(roomInputText);

        LocationDropDownDataModel workspaceProperty = (LocationDropDownDataModel) viewModel.getInputFieldModels().get(SEAT_LOCATION);
        PropertyAutoCompleteTextView workspaceInputText = new PropertyAutoCompleteTextView(requireContext(), workspaceProperty);
        workspaceProperty.getMutableInstances().observe(this.getViewLifecycleOwner(), workspaces -> ((PropertyAutoCompleteTextView) workspaceInputText).updateAdapterList(workspaces));
        setIsMissingObserver(workspaceProperty, workspaceInputText);
        linearLayout.addView(workspaceInputText);

        LocationDropDownDataModel elementProperty = (LocationDropDownDataModel) viewModel.getInputFieldModels().get(STORED_IN);
        PropertyAutoCompleteTextView elementInputText = new PropertyAutoCompleteTextView(requireContext(), elementProperty);
        elementProperty.getMutableInstances().observe(this.getViewLifecycleOwner(), elements -> ((PropertyAutoCompleteTextView) elementInputText).updateAdapterList(elements));
        setIsMissingObserver(elementProperty, elementInputText);
        linearLayout.addView(elementInputText);

        List<PropertyAutoCompleteTextView> locationInputTextViews = Arrays.asList(buildingInputText, facilityInputText, roomInputText, workspaceInputText, elementInputText);
        List<LocationDropDownDataModel> properties = Arrays.asList(buildingProperty, facilityProperty, roomProperty, workspaceProperty, elementProperty);
        setFilterBehaviour(locationInputTextViews, properties);
        return sectionView;
    }

    private void setFilterBehaviour(List<PropertyAutoCompleteTextView> locationInputTextViews, List<LocationDropDownDataModel> properties) {
        for (int i = 0; i < locationInputTextViews.size(); i++) {
            PropertyAutoCompleteTextView view = locationInputTextViews.get(i);
            LocationDropDownDataModel property = properties.get(i);

            int currentViewInd = i;
            AdapterView.OnItemClickListener onItemClickListener = (adapterView, view1, i1, l) -> {
                String selected = adapterView.getItemAtPosition(i1).toString();
                LOGGER.debug(selected);
                property.setFieldValue(selected);

                clearLowerLevelDropdown(currentViewInd + 1, locationInputTextViews);
                loadSubLevelOptions(
                        properties.get(currentViewInd + 1),
                        (Instance) locationInputTextViews.get(currentViewInd).getAdapter().getItem(i1));
            };
            view.setOnItemClickedListener(onItemClickListener);

            TextWatcher textWatcher = new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2) {
                    ;
                }

                @Override
                public void onTextChanged(CharSequence charSequence, int i, int i1, int i2) {
                    ;
                }

                @Override
                public void afterTextChanged(Editable editable) {
                    property.getShowDisallowError().setValue(false);
                    property.getIsMissingField().setValue(false);
                    property.setFieldValue(editable.toString());

                    clearLowerLevelDropdown(currentViewInd + 1, locationInputTextViews);
                    if (property.getMatched(editable.toString()) != null) {
                        loadSubLevelOptions(
                                properties.get(currentViewInd + 1),
                                property.getMatched(editable.toString()));
                    }
                }
            };
            view.setTextWatcher(textWatcher);
        }

    }

    private void clearLowerLevelDropdown(int subLevelViewInd, List<PropertyAutoCompleteTextView> locationInputTextViews) {
        for (int i = subLevelViewInd; i < locationInputTextViews.size(); i++) {
            locationInputTextViews.get(i).clearEditText();
            locationInputTextViews.get(i).updateAdapterList(new ArrayList<>());
        }
    }

    private void loadSubLevelOptions(LocationDropDownDataModel subLevelProperty, Instance currentSelectedInstance) {
        subLevelProperty.getMutableInstances().setValue(currentSelectedInstance.getSortedSubLevelItems());
    }

    private View createDataSheetSection(LayoutInflater inflater, String section) {
        View sectionView = inflater.inflate(R.layout.view_data_sheet_section, null);
        ((TextView) sectionView.findViewById(R.id.section_label)).setText(section);

        LinearLayout linearLayout = sectionView.findViewById(R.id.linear_layout);
        for (String fieldName : viewModel.getInputFieldNamesBySection().get(section)) {
            AssetPropertyDataModel property = viewModel.getInputFieldModels().get(fieldName);
            if (property instanceof DataFileDataModel) {
                DataFileItemView dataSheetItem = new DataFileItemView(requireContext(), (DataFileDataModel) property);
                ((DataFileDataModel) property).getMutableFilePath().observe(getViewLifecycleOwner(), uri -> {
                    if (uri.toString().isEmpty()) {
                        return;
                    }
                    dataSheetItem.setFileNameTextView(FileUtils.getFileNameFromUri(uri, requireContext()));
                });
                dataSheetItem.setOnButtonClickListener(getFilePickerListener((DataFileDataModel) property));
                linearLayout.addView(dataSheetItem);
            } else {
                PropertyBaseInputTextView inputText = new PropertyGeneralInputTextView(requireContext(), property);
                linearLayout.addView(inputText);
            }
        }
        return sectionView;
    }

    private View.OnClickListener getFilePickerListener(DataFileDataModel property) {
        return view -> checkStoragePermission(property);
    }

    private void checkStoragePermission(DataFileDataModel property) {
        if (property.getFieldName().equals(SPEC_SHEET_FILE)) {
            startFilePicker(OPEN_SPEC_SHEET_REQUEST);
        } else if (property.getFieldName().equals(MANUAL_FILE)) {
            startFilePicker(OPEN_MANUAL_REQUEST);
        }

//        if (ContextCompat.checkSelfPermission(requireContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
//                == PackageManager.PERMISSION_GRANTED) {
//            if (property.getFieldName().equals(SPEC_SHEET_FILE_URI)) {
//                startFilePicker(property, OPEN_SPEC_SHEET_REQUEST);
//            } else if (property.getFieldName().equals(MANUAL_FILE_URI)) {
//                startFilePicker(property, OPEN_MANUAL_REQUEST);
//            }
//
//        } else {
//            // Permission not granted, request it
//            requestPermissionLauncher.launch(Manifest.permission.READ_EXTERNAL_STORAGE);
//        }
    }

    private void startFilePicker(int requestCode) {

        Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT);
        intent.addCategory(Intent.CATEGORY_OPENABLE);
        // todo: restrict to pdf
        intent.setType("*/*");

        startActivityForResult(intent, requestCode);
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, @Nullable Intent result) {
        super.onActivityResult(requestCode, resultCode, result);
        if (requestCode == OPEN_SPEC_SHEET_REQUEST) {
            if (resultCode == Activity.RESULT_OK) {
                if (result.getData() != null) {
                    // Get the selected file's URI
                    Uri selectedFileUri = result.getData();
                    LOGGER.info("Selected spec sheet URI: " + selectedFileUri.toString());
                    ((DataFileDataModel) viewModel.getInputFieldModels().get(SPEC_SHEET_FILE)).setFilePath(selectedFileUri);
                }
            }
        } else if (requestCode == OPEN_MANUAL_REQUEST) {
            if (resultCode == Activity.RESULT_OK) {
                if (result.getData() != null) {
                    // Get the selected file's URI
                    Uri selectedFileUri = result.getData();
                    LOGGER.info("Selected spec sheet URI: " + selectedFileUri.toString());
                    ((DataFileDataModel) viewModel.getInputFieldModels().get(MANUAL_FILE)).setFilePath(selectedFileUri);
                }
            }
        }
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        viewModel.requestAllDropDownOptionsFromRepository();
    }
}
