package uk.ac.cam.cares.jps.addasset;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.List;
import java.util.stream.Collectors;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.addasset.model.AddAssetViewModel;
import uk.ac.cam.cares.jps.addasset.model.AssetPropertyDataModel;
import uk.ac.cam.cares.jps.addasset.view.DataSheetItemView;
import uk.ac.cam.cares.jps.addasset.view.PropertyAutoCompleteTextView;
import uk.ac.cam.cares.jps.addasset.view.PropertyGeneralInputTextView;
import uk.ac.cam.cares.jps.data.OtherInfoModel;

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
            if (viewModel.DATA_SHEET_SECTIONS.contains(section)) {
                sectionView = createDataSheetSection(inflater, section);
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
        for (AssetPropertyDataModel property : viewModel.getInputFieldsBySection().get(section)) {
            View inputText;
            if (property.getType().equals(AssetPropertyDataModel.ViewType.DROP_DOWN)) {
                inputText = new PropertyAutoCompleteTextView(requireContext(), property);
                viewModel.getDropDownLiveDataByKey(property.getFieldName()).observe(this.getViewLifecycleOwner(), options -> {
                    ((PropertyAutoCompleteTextView) inputText).updateAdapterList(options);
                });
            } else {
                inputText = new PropertyGeneralInputTextView(requireContext(), property);
            }

            linearLayout.addView(inputText);
        }
        return sectionView;
    }

    private View createDataSheetSection(LayoutInflater inflater, String section) {
        View sectionView = inflater.inflate(R.layout.view_data_sheet_section, null);
        ((TextView) sectionView.findViewById(R.id.section_label)).setText(section);

        LinearLayout linearLayout = sectionView.findViewById(R.id.linear_layout);
        for (AssetPropertyDataModel property : viewModel.getInputFieldsBySection().get(section)) {
            View dataSheetItem = new DataSheetItemView(requireContext(), property);
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
