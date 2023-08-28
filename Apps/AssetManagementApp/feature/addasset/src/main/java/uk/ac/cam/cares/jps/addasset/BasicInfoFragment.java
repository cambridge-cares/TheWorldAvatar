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

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.addasset.model.AddAssetViewModel;
import uk.ac.cam.cares.jps.addasset.model.AssetPropertyDataModel;
import uk.ac.cam.cares.jps.addasset.view.PropertyInputTextView;

@AndroidEntryPoint
public class BasicInfoFragment extends Fragment {
    AddAssetViewModel viewModel;
    List<String> sections;
    private Logger LOGGER = Logger.getLogger(BasicInfoFragment.class);

    BasicInfoFragment(List<String> sections) {
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
            View sectionView = inflater.inflate(R.layout.view_section, null);
            ((TextView) sectionView.findViewById(R.id.section_label)).setText(section);

            LinearLayout sectionRoot = sectionView.findViewById(R.id.linear_layout);
            for (AssetPropertyDataModel property : viewModel.getInputFieldsBySection().get(section)) {
                View inputText = new PropertyInputTextView(requireContext(), property);

                sectionRoot.addView(inputText);
            }
            root.addView(sectionView);
        }

        scrollView.addView(root);
        return scrollView;
    }


    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
    }
}
