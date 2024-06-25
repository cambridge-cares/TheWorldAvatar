package uk.ac.cam.cares.jps.assetinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.HAS_TIME_SERIES;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.IRI;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.REFERENCE_LABEL;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.TYPE;
import static uk.ac.cam.cares.jps.utils.SerializationUtils.serializeObjectToString;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.ContextMenu;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
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
        binding.topAppBar.setNavigationOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());

        Uri uri = ((Intent) getArguments().get("android-support-nav:controller:deepLinkIntent")).getData();

        if (uri.getPath().contains("/info_page")) {
            showAssetInfoForGivenUri(view, getArguments().getString("uri"));
        } else if (uri.getPath().contains("/asset_summary")) {
            AssetInfo assetInfo = viewModel.assetInfo.getValue();
            String operation = getArguments().getString("operation");

            int appBarTitle = 0;
            if (operation.equals("add")) {
                appBarTitle = R.string.add_asset;
            } else if (operation.equals("edit")) {
                appBarTitle = R.string.edit_asset;
            }

            NavDeepLinkRequest request = null;
            request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/new_asset_result?operation=" + operation))
                    .build();

            showAssetSummary(view, appBarTitle, request);
        }

    }

    @Override
    public void onCreateContextMenu(@NonNull ContextMenu menu, @NonNull View v, @Nullable ContextMenu.ContextMenuInfo menuInfo) {
        super.onCreateContextMenu(menu, v, menuInfo);
    }

    @Override
    public boolean onContextItemSelected(@NonNull MenuItem item) {
        return super.onContextItemSelected(item);
    }

    private void showAssetInfoForGivenUri(View view, String uri) {
        LOGGER.info("Navigated to asset info page");
        LOGGER.info(getArguments().getString("uri"));

        binding.topAppBar.setTitle(R.string.asset_info);
        binding.topAppBar.inflateMenu(R.menu.asset_info_menu);

        viewModel.assetInfo.observe(getViewLifecycleOwner(), assetInfo -> {
            if (assetInfo == null) {
                return;
            }

            if (!binding.topAppBar.hasOnClickListeners()) {
                binding.topAppBar.setOnMenuItemClickListener(menuItem -> {
                    if (menuItem.getItemId() == R.id.edit) {
                        editItemClicked();
                        return true;
                    } else if (menuItem.getItemId() == R.id.add_maintenance) {
                        maintenanceItemClicked();
                        return true;
                    } else if (menuItem.getItemId() == R.id.delete) {
                        UiUtils.showNotImplementedDialog(requireContext());
                        return true;
                    }
                    return false;
                });
            }

            assetInfoAdapter.updateProperties(assetInfo);
            hideShimmer();
            binding.assetInfoRv.setVisibility(View.VISIBLE);

            setupViewGraphButton(assetInfo);
        });
        viewModel.getErrorMessage().observe(getViewLifecycleOwner(), error -> {
            hideShimmer();
            binding.errorView.setVisibility(View.VISIBLE);
            ((TextView) binding.errorView.findViewById(R.id.error_msg_tv)).setText(error);
        });
        viewModel.getAssetInfoByIri(uri);

        assetInfoAdapter = new AssetInfoAdapter();
        binding.assetInfoRv.setLayoutManager(new LinearLayoutManager(view.getContext()));
        binding.assetInfoRv.setAdapter(assetInfoAdapter);

        binding.shimmerViewContainer.startShimmer();
    }

    private void editItemClicked() {
        AssetInfo assetInfo = viewModel.getAssetInfo().getValue();
        NavDeepLinkRequest request = null;
        request = NavDeepLinkRequest.Builder
                .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/edit_asset"))
                .build();
        NavHostFragment.findNavController(this).navigate(request);
    }

    private void maintenanceItemClicked() {
        AssetInfo assetInfo = viewModel.getAssetInfo().getValue();
        NavDeepLinkRequest request = null;
        try {
            request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/add_maintenance?assetinfo=" + serializeObjectToString(assetInfo)))
                    .build();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        NavHostFragment.findNavController(this).navigate(request);
    }

    private void setupViewGraphButton(AssetInfo assetInfo) {
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
    }

    private void showAssetSummary(View view, int appBarTitle, NavDeepLinkRequest request) {
        assetInfoAdapter = new AssetInfoAdapter(viewModel.assetInfo.getValue(), false);
        binding.assetInfoRv.setLayoutManager(new LinearLayoutManager(view.getContext()));
        binding.assetInfoRv.setAdapter(assetInfoAdapter);
        hideShimmer();
        binding.assetInfoRv.setVisibility(View.VISIBLE);

        binding.topAppBar.setTitle(appBarTitle);
        binding.topAppBar.inflateMenu(R.menu.asset_summary_menu);

        binding.topAppBar.setOnMenuItemClickListener( menuItem -> {
            if (menuItem.getItemId() == R.id.done) {
                NavHostFragment.findNavController(this).navigate(request);
                return true;
            }
            return false;
        });
    }

    private void hideShimmer() {
        binding.shimmerViewContainer.stopShimmer();
        binding.shimmerViewContainer.setVisibility(View.GONE);
    }
}
