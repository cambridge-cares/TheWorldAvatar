package uk.ac.cam.cares.jps.addasset;

import static uk.ac.cam.cares.jps.utils.SerializationUtils.serializeObjectToString;

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
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.addasset.databinding.FragmentAddAssetBinding;
import uk.ac.cam.cares.jps.addasset.model.AddAssetViewModel;

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
        ((TextView) view.findViewById(uk.ac.cam.cares.jps.ui.R.id.instance_title)).setText(R.string.add_asset);

        ViewPager2 viewPager = binding.viewPager;
        TabLayout tabLayout = binding.tabs;
        adapter = new TabAdapter(requireActivity().getSupportFragmentManager(), getLifecycle());
        viewPager.setAdapter(adapter);
        List<String> tabNames = Arrays.asList("General", "Purchase", "Documents");
        new TabLayoutMediator(tabLayout, viewPager, (tab, position) -> tab.setText(tabNames.get(position))).attach();

        binding.doneBt.setOnClickListener(view1 -> {
            if (viewModel.checkMissingInput()) {
                return;
            }

            if (viewModel.checkDisallowNewInstanceInputField()) {
                return;
            }

            // show summary page
            Bundle bundle = new Bundle();
            // todo: check whether AssetInfo is serializable
            bundle.putSerializable("assetinfo", (Serializable) viewModel.getAssetInfo());


            NavDeepLinkRequest request = null;
            try {
                request = NavDeepLinkRequest.Builder
                        .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/new_asset_summary?assetinfo=" + serializeObjectToString(viewModel.getAssetInfo())))
                        .build();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            NavHostFragment.findNavController(this).navigate(request);
        });
    }


}
