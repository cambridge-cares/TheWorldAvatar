package uk.ac.cam.cares.jps.addasset;

import static uk.ac.cam.cares.jps.ui.UiUtils.showNotImplementedDialog;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;

import org.apache.log4j.Logger;

import java.io.IOException;

import uk.ac.cam.cares.jps.addasset.databinding.FragmentNewAssetResultBinding;
import uk.ac.cam.cares.jps.addasset.model.NewAssetResultViewModel;
import uk.ac.cam.cares.jps.model.AssetInfo;

public class NewAssetResultFragment extends Fragment {
    private FragmentNewAssetResultBinding binding;
    private Logger LOGGER = Logger.getLogger(NewAssetResultFragment.class);

    NewAssetResultViewModel viewModel;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        viewModel = new ViewModelProvider(requireActivity()).get(NewAssetResultViewModel.class);
        Fragment temp = this;
        requireActivity().getOnBackPressedDispatcher().addCallback(this, new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (viewModel.getIsSuccess().getValue() == null || !viewModel.getIsSuccess().getValue()) {
                    NavHostFragment.findNavController(temp).navigateUp();
                } else {
                    NavHostFragment.findNavController(temp).popBackStack(R.id.add_asset_info, true);
                }
            }
        });
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentNewAssetResultBinding.inflate(inflater);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        ((TextView) view.findViewById(uk.ac.cam.cares.jps.ui.R.id.instance_title)).setText(R.string.add_asset);
        view.findViewById(uk.ac.cam.cares.jps.ui.R.id.back_bt).setOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());
        viewModel.getIsSuccess().observe(getViewLifecycleOwner(), isSuccess -> {
            view.findViewById(R.id.loading_view).setVisibility(View.GONE);
            if (isSuccess) {
                view.findViewById(R.id.success_view).setVisibility(View.VISIBLE);
                view.findViewById(uk.ac.cam.cares.jps.ui.R.id.back_bt).setOnClickListener(v -> NavHostFragment.findNavController(this).popBackStack(R.id.add_asset_info, true));
            } else {
                view.findViewById(R.id.error_view).setVisibility(View.VISIBLE);
            }
        });


        if (getArguments().getString("operation").equals("add")) {
            viewModel.addNewAsset();
        } else if (getArguments().getString("operation").equals("edit")) {
            // todo: not implemented yet
            showNotImplementedDialog(requireContext());
        }

    }
}
