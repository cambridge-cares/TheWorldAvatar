package uk.ac.cam.cares.jps.addasset;

import static uk.ac.cam.cares.jps.utils.SerializationUtils.deserializeStringToObject;
import static uk.ac.cam.cares.jps.utils.SerializationUtils.serializeObjectToString;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;
import androidx.viewpager2.widget.ViewPager2;

import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.addasset.databinding.FragmentAddAssetBinding;
import uk.ac.cam.cares.jps.addasset.model.AddAssetViewModel;
import uk.ac.cam.cares.jps.model.AssetInfo;
import uk.ac.cam.cares.jps.ui.UiUtils;

@AndroidEntryPoint
public class AddAssetFragment extends Fragment {
    private FragmentAddAssetBinding binding;
    private Logger LOGGER = Logger.getLogger(AddAssetFragment.class);
    TabAdapter adapter;
    AddAssetViewModel viewModel;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BasicConfigurator.configure();
        binding = FragmentAddAssetBinding.inflate(inflater);
        viewModel = new ViewModelProvider(requireActivity()).get(AddAssetViewModel.class);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        view.findViewById(uk.ac.cam.cares.jps.ui.R.id.back_bt).setOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());

        Uri uri = ((Intent) getArguments().get("android-support-nav:controller:deepLinkIntent")).getData();
        if (uri.getPath().contains("/add_asset")) {
            ((TextView) view.findViewById(uk.ac.cam.cares.jps.ui.R.id.instance_title)).setText(R.string.add_asset);
            viewModel.setEditMode("add");
            loadUIComponent();
        } else if (uri.getPath().contains("/edit_asset")) {
            ((TextView) view.findViewById(uk.ac.cam.cares.jps.ui.R.id.instance_title)).setText(R.string.edit_asset);
            try {
                AssetInfo assetInfo = (AssetInfo) deserializeStringToObject(getArguments().getString("assetinfo"));
                viewModel.initFieldsWithAssetInfo(assetInfo);
                viewModel.setEditMode("edit");
                loadUIComponent();

                binding.doneBt.setOnClickListener(view1 -> {
                    // todo: the check may not pass for edit function. Some mandatory fields are missing from the existing data, and in test phase the drop down list is not completed
                    UiUtils.showNotImplementedDialog(requireContext());
                });
            } catch (IOException | ClassNotFoundException e) {
                throw new RuntimeException(e);
            }

        }


    }

    private void loadUIComponent() {
        ViewPager2 viewPager = binding.viewPager;
        TabLayout tabLayout = binding.tabs;
        adapter = new TabAdapter(requireActivity().getSupportFragmentManager(), getLifecycle());
        viewPager.setAdapter(adapter);
        List<String> tabNames = Arrays.asList("General", "Purchase", "Documents");
        new TabLayoutMediator(tabLayout, viewPager, (tab, position) -> tab.setText(tabNames.get(position))).attach();

        binding.saveBt.setOnClickListener(view -> {
            UiUtils.showNotImplementedDialog(requireContext());
        });

        binding.doneBt.setOnClickListener(view1 -> {
            // todo: remove check for test purpose
//            if (viewModel.checkMissingInput()) {
//                return;
//            }
//
//            if (viewModel.checkDisallowNewInstanceInputField()) {
//                return;
//            }

            // show summary page
            NavHostFragment.findNavController(this).navigate(getRequest());
        });
    }

    private NavDeepLinkRequest getRequest() {
        NavDeepLinkRequest request = null;
        try {
            request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/asset_summary?assetinfo=" + serializeObjectToString(viewModel.getAssetInfo(requireContext()))  + "&operation=" + viewModel.getEditMode()))
                    .build();
            return request;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

}
