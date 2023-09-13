package uk.ac.cam.cares.jps.assetinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.HAS_TIME_SERIES;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.IRI;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.REFERENCE_LABEL;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.TYPE;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.assetinfo.databinding.FragmentAssetInfoBinding;

@AndroidEntryPoint
public class AssetInfoFragment extends Fragment {

    private FragmentAssetInfoBinding binding;
    private static final Logger LOGGER = LogManager.getLogger(AssetInfoFragment.class);

    AssetInfoViewModel viewModel;
    AssetInfoAdapter assetInfoAdapter;

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
        LOGGER.info(getArguments().getString("uri"));

        view.findViewById(uk.ac.cam.cares.jps.ui.R.id.back_bt).setOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());
        ((TextView) view.findViewById(uk.ac.cam.cares.jps.ui.R.id.instance_title)).setText(R.string.asset_info);

        viewModel.getAssetInfo().observe(this.getViewLifecycleOwner(), assetInfo -> {
            assetInfoAdapter.updateProperties(assetInfo);
            binding.shimmerViewContainer.stopShimmer();
            binding.shimmerViewContainer.setVisibility(View.GONE);
            binding.assetInfoRv.setVisibility(View.VISIBLE);

            binding.viewGraphBt.setOnClickListener(bt -> new MaterialAlertDialogBuilder(requireActivity()).setTitle(R.string.view_status_graph)
                    .setMessage(R.string.view_status_in_bms_app).setPositiveButton(R.string.yes, (dialogInterface, i) -> {
                        Intent bmsIntent = new Intent();
                        bmsIntent.setAction("uk.ac.cam.cares.jps.bmsqueryapp.action.VIEW_GRAPH");
                        bmsIntent.putExtra("equipmentIRI", assetInfo.getProperties().get(IRI));
                        bmsIntent.putExtra("equipmentLabel", assetInfo.getProperties().getOrDefault(REFERENCE_LABEL, ""));
                        bmsIntent.putExtra("equipmentType", assetInfo.getProperties().get(TYPE));
                        bmsIntent.setType("text/plain");

                        startActivity(bmsIntent);
                    })
                    .setNegativeButton(R.string.no, null)
                    .show());
            if (assetInfo.getProperties().get(HAS_TIME_SERIES).equals("true")) {
                binding.viewGraphBt.setVisibility(View.VISIBLE);
            }
        });
        viewModel.getErrorMessage().observe(getViewLifecycleOwner(), error -> {
            binding.shimmerViewContainer.stopShimmer();
            binding.shimmerViewContainer.setVisibility(View.GONE);
            binding.errorView.setVisibility(View.VISIBLE);
        });
        viewModel.getAssetInfoByIri(getArguments().getString("uri"));

        assetInfoAdapter = new AssetInfoAdapter();
        binding.assetInfoRv.setLayoutManager(new LinearLayoutManager(view.getContext()));
        binding.assetInfoRv.setAdapter(assetInfoAdapter);

        binding.shimmerViewContainer.startShimmer();


    }
}
