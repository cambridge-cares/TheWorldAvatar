package uk.ac.cam.cares.jps.assetinfo;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.assetinfo.databinding.FragmentAssetInfoBinding;

public class AssetInfoFragment extends Fragment {

    private FragmentAssetInfoBinding binding;
    private static final Logger LOGGER = LogManager.getLogger(AssetInfoFragment.class);

    private String assetUri;


    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentAssetInfoBinding.inflate(inflater);
        BasicConfigurator.configure();
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        LOGGER.info("Navigated to asset info page");
        assetUri = getArguments().getString("uri");
        binding.url.setText(assetUri);
        LOGGER.info(assetUri);
    }
}
