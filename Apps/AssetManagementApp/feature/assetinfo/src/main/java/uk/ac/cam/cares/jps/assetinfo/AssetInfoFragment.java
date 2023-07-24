package uk.ac.cam.cares.jps.assetinfo;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import javax.inject.Inject;

import dagger.hilt.android.AndroidEntryPoint;
import dagger.hilt.android.scopes.FragmentScoped;
import uk.ac.cam.cares.jps.assetinfo.databinding.FragmentAssetInfoBinding;
import uk.ac.cam.cares.jps.data.AssetInfo;

@AndroidEntryPoint
public class AssetInfoFragment extends Fragment {

    private FragmentAssetInfoBinding binding;
    private static final Logger LOGGER = LogManager.getLogger(AssetInfoFragment.class);

    AssetInfoViewModel viewModel;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentAssetInfoBinding.inflate(inflater);
        BasicConfigurator.configure();
        viewModel = new ViewModelProvider(this).get(AssetInfoViewModel.class);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        LOGGER.info("Navigated to asset info page");

        viewModel.getAssetInfo().observe(this.getViewLifecycleOwner(), assetInfo -> {
            binding.url.setText(assetInfo.toString());
        });

        viewModel.getAssetInfoByIri(getArguments().getString("uri"));

//        assetUri = getArguments().getString("uri");
//        binding.url.setText(assetUri);
//        LOGGER.info(assetUri);
    }
}
