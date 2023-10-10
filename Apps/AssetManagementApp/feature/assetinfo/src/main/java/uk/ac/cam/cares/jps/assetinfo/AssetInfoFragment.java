package uk.ac.cam.cares.jps.assetinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.HAS_TIME_SERIES;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.IRI;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.REFERENCE_LABEL;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.TYPE;
import static uk.ac.cam.cares.jps.utils.SerializationUtils.deserializeStringToObject;
import static uk.ac.cam.cares.jps.utils.SerializationUtils.serializeObjectToString;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.io.IOException;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.assetinfo.databinding.FragmentAssetInfoBinding;
import uk.ac.cam.cares.jps.model.AssetInfo;
import uk.ac.cam.cares.jps.ui.UiUtils;

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
        view.findViewById(uk.ac.cam.cares.jps.ui.R.id.back_bt).setOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());

        Uri uri = ((Intent) getArguments().get("android-support-nav:controller:deepLinkIntent")).getData();

        if (uri.getPath().contains("/info_page")) {
            showAssetInfoForGivenUri(view, getArguments().getString("uri"));
        } else if (uri.getPath().contains("/asset_summary")) {
            try {
                AssetInfo assetInfo = (AssetInfo) deserializeStringToObject(getArguments().getString("assetinfo"));
                String operation = getArguments().getString("operation");

                int appBarTitle = 0;
                if (operation.equals("add")) {
                    appBarTitle = R.string.add_asset;
                } else if (operation.equals("edit")) {
                    appBarTitle = R.string.edit_asset;
                }

                NavDeepLinkRequest request = null;
                try {
                    request = NavDeepLinkRequest.Builder
                            .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/new_asset_result?assetinfo=" + serializeObjectToString(assetInfo) + "&operation=" + operation))
                            .build();
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }

                showAssetSummary(view, assetInfo, appBarTitle, request);
            } catch (IOException | ClassNotFoundException e) {
                throw new RuntimeException(e);
            }
        }

    }

    private void showAssetInfoForGivenUri(View view, String uri) {
        LOGGER.info("Navigated to asset info page");
        LOGGER.info(getArguments().getString("uri"));

        ((TextView) view.findViewById(uk.ac.cam.cares.jps.ui.R.id.instance_title)).setText(R.string.asset_info);
        view.findViewById(R.id.info_app_bar_buttons).setVisibility(View.VISIBLE);
        ImageButton editButton = view.findViewById(R.id.edit_bt);
        editButton.setEnabled(false);
        editButton.setOnClickListener(view1 -> {
            // todo: the infor for assetinfo here may not be complete due to setting. need to fix it when goes to edit
            AssetInfo assetInfo = viewModel.getAssetInfo().getValue();
            NavDeepLinkRequest request = null;
            try {
                request = NavDeepLinkRequest.Builder
                        .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/edit_asset?assetinfo=" + serializeObjectToString(assetInfo)))
                        .build();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            NavHostFragment.findNavController(this).navigate(request);
        });

        ImageButton deleteButton = view.findViewById(R.id.delete_bt);
        deleteButton.setEnabled(false);
        deleteButton.setOnClickListener(view1 -> {
            // todo: call repository for delete
            UiUtils.showNotImplementedDialog(requireContext());
        });

        viewModel.getAssetInfo().observe(this.getViewLifecycleOwner(), assetInfo -> {
            assetInfoAdapter.updateProperties(assetInfo);
            hideShimmer();
            binding.assetInfoRv.setVisibility(View.VISIBLE);

            editButton.setEnabled(true);
            deleteButton.setEnabled(true);

            binding.viewGraphBt.setOnClickListener(bt -> new MaterialAlertDialogBuilder(requireActivity()).setTitle(R.string.view_status_graph)
                    .setMessage(R.string.view_status_in_bms_app).setPositiveButton(R.string.yes, (dialogInterface, i) -> {
                        Intent bmsIntent = new Intent();
                        bmsIntent.setAction("uk.ac.cam.cares.jps.bmsqueryapp.action.VIEW_GRAPH");
                        bmsIntent.putExtra("equipmentIRI", assetInfo.getProperties().get(IRI));
                        bmsIntent.putExtra("equipmentLabel", assetInfo.getProperties().getOrDefault(REFERENCE_LABEL, ""));
                        bmsIntent.putExtra("equipmentType", assetInfo.getProperties().get(TYPE));
                        bmsIntent.setType("text/plain");

                        try {
                            startActivity(bmsIntent);
                        } catch (Exception e) {
                            new MaterialAlertDialogBuilder(requireActivity())
                                    .setTitle(R.string.unable_to_open_bms_app)
                                    .setMessage(R.string.check_bms_installation)
                                    .setPositiveButton(R.string.ok, (dialogInterface1, i1) -> {})
                                    .show();
                        }
                    })
                    .setNegativeButton(R.string.no, null)
                    .show());
            if (assetInfo.getProperties().get(HAS_TIME_SERIES).equals("true")) {
                binding.viewGraphBt.setVisibility(View.VISIBLE);
            }
        });
        viewModel.getErrorMessage().observe(getViewLifecycleOwner(), error -> {
            hideShimmer();
            binding.errorView.setVisibility(View.VISIBLE);
        });
        viewModel.getAssetInfoByIri(uri);

        assetInfoAdapter = new AssetInfoAdapter();
        binding.assetInfoRv.setLayoutManager(new LinearLayoutManager(view.getContext()));
        binding.assetInfoRv.setAdapter(assetInfoAdapter);

        binding.shimmerViewContainer.startShimmer();
    }

    private void showAssetSummary(View view, AssetInfo assetInfo, int appBarTitle, NavDeepLinkRequest request) {
        // NOTICE: assetinfo is not hosted in ViewModel
        ((TextView) view.findViewById(uk.ac.cam.cares.jps.ui.R.id.instance_title)).setText(appBarTitle);
        assetInfoAdapter = new AssetInfoAdapter(assetInfo, false);
        binding.assetInfoRv.setLayoutManager(new LinearLayoutManager(view.getContext()));
        binding.assetInfoRv.setAdapter(assetInfoAdapter);
        hideShimmer();
        binding.assetInfoRv.setVisibility(View.VISIBLE);

        ImageButton doneButton = view.findViewById(R.id.done_bt);
        doneButton.setVisibility(View.VISIBLE);
        doneButton.setOnClickListener(v -> {
            NavHostFragment.findNavController(this).navigate(request);
        });
    }

    private void hideShimmer() {
        binding.shimmerViewContainer.stopShimmer();
        binding.shimmerViewContainer.setVisibility(View.GONE);
    }
}
