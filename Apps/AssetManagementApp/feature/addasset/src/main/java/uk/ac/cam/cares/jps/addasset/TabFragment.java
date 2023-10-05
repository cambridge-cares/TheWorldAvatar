package uk.ac.cam.cares.jps.addasset;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.BUILDING;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.FACILITY;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.LOCATED_IN;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.LOCATION_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.MANUAL_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SEAT_LOCATION;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SPEC_SHEET_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.STORED_IN;

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
import uk.ac.cam.cares.jps.addasset.model.DropDownDataModel;
import uk.ac.cam.cares.jps.addasset.model.LocationDropDownDataModel;
import uk.ac.cam.cares.jps.addasset.view.DataSheetItemView;
import uk.ac.cam.cares.jps.addasset.view.PropertyAutoCompleteTextView;
import uk.ac.cam.cares.jps.addasset.view.PropertyBaseInputTextView;
import uk.ac.cam.cares.jps.addasset.view.PropertyGeneralInputTextView;
import uk.ac.cam.cares.jps.model.building.Instance;

@AndroidEntryPoint
public class TabFragment extends Fragment {
    AddAssetViewModel viewModel;
    List<String> sections;
    private Logger LOGGER = Logger.getLogger(TabFragment.class);

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

    private View createLocationSection(LayoutInflater inflater, String section) {
        View sectionView = inflater.inflate(R.layout.view_input_text_section, null);
        ((TextView) sectionView.findViewById(R.id.section_label)).setText(section);
        LinearLayout linearLayout = sectionView.findViewById(R.id.linear_layout);

        LocationDropDownDataModel buildingProperty = (LocationDropDownDataModel) viewModel.getInputFieldModels().get(BUILDING);
        PropertyAutoCompleteTextView buildingInputText = new PropertyAutoCompleteTextView(requireContext(), buildingProperty);
        buildingProperty.getMutableInstances().observe(this.getViewLifecycleOwner(), buildings -> ((PropertyAutoCompleteTextView) buildingInputText).updateAdapterList(buildings));
        linearLayout.addView(buildingInputText);

        LocationDropDownDataModel facilityProperty = (LocationDropDownDataModel) viewModel.getInputFieldModels().get(FACILITY);
        PropertyAutoCompleteTextView facilityInputText = new PropertyAutoCompleteTextView(requireContext(), facilityProperty);
        facilityProperty.getMutableInstances().observe(this.getViewLifecycleOwner(), facilities -> ((PropertyAutoCompleteTextView) facilityInputText).updateAdapterList(facilities));
        linearLayout.addView(facilityInputText);

        LocationDropDownDataModel roomProperty = (LocationDropDownDataModel) viewModel.getInputFieldModels().get(LOCATED_IN);
        PropertyAutoCompleteTextView roomInputText = new PropertyAutoCompleteTextView(requireContext(), roomProperty);
        roomProperty.getMutableInstances().observe(this.getViewLifecycleOwner(), rooms -> ((PropertyAutoCompleteTextView) roomInputText).updateAdapterList(rooms));
        linearLayout.addView(roomInputText);

        LocationDropDownDataModel workspaceProperty = (LocationDropDownDataModel) viewModel.getInputFieldModels().get(SEAT_LOCATION);
        PropertyAutoCompleteTextView workspaceInputText = new PropertyAutoCompleteTextView(requireContext(), workspaceProperty);
        workspaceProperty.getMutableInstances().observe(this.getViewLifecycleOwner(), workspaces -> ((PropertyAutoCompleteTextView) workspaceInputText).updateAdapterList(workspaces));
        linearLayout.addView(workspaceInputText);

        LocationDropDownDataModel elementProperty = (LocationDropDownDataModel) viewModel.getInputFieldModels().get(STORED_IN);
        PropertyAutoCompleteTextView elementInputText = new PropertyAutoCompleteTextView(requireContext(), elementProperty);
        elementProperty.getMutableInstances().observe(this.getViewLifecycleOwner(), elements -> ((PropertyAutoCompleteTextView) elementInputText).updateAdapterList(elements));
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
            View dataSheetItem = new DataSheetItemView(requireContext(), viewModel.getInputFieldModels().get(fieldName));
            linearLayout.addView(dataSheetItem);
        }
        return sectionView;
    }


    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        viewModel.requestAllDropDownOptionsFromRepository();
    }
}
