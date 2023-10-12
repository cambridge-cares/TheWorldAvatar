package uk.ac.cam.cares.jps.setting;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.google.android.material.checkbox.MaterialCheckBox;
import com.google.android.material.theme.MaterialComponentsViewInflater;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.setting.databinding.FragmentSettingBinding;

@AndroidEntryPoint
public class SettingFragment extends Fragment {
    private FragmentSettingBinding binding;
    private final Logger LOGGER = Logger.getLogger(SettingFragment.class);
    private SettingViewModel viewModel;
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BasicConfigurator.configure();
        binding = FragmentSettingBinding.inflate(inflater);
        viewModel = new ViewModelProvider(this).get(SettingViewModel.class);
        viewModel.getUserInfo();
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        view.findViewById(uk.ac.cam.cares.jps.ui.R.id.back_bt).setOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());
        ((TextView) view.findViewById(uk.ac.cam.cares.jps.ui.R.id.instance_title)).setText(R.string.setting);

        binding.assetInfoContainer.setLayoutManager(new LinearLayoutManager(view.getContext()));
        binding.assetInfoContainer.setAdapter(new SettingAssetInfoAdapter(requireContext(), viewModel, this.getViewLifecycleOwner()));

        ((MaterialCheckBox) binding.emialNotifCb).addOnCheckedStateChangedListener((checkBox, state) -> {
            viewModel.getSettingByKey("mail").setValue(state);
        });
        viewModel.getSettingByKey("mail").observe(this.getViewLifecycleOwner(), integer -> ((MaterialCheckBox) binding.emialNotifCb).setCheckedState(integer));

        ((MaterialCheckBox) binding.inAppNotifCb).addOnCheckedStateChangedListener((checkBox, state) -> {
            viewModel.getSettingByKey("inAppNotification").setValue(state);
        });
        viewModel.getSettingByKey("inAppNotification").observe(this.getViewLifecycleOwner(), integer -> ((MaterialCheckBox) binding.inAppNotifCb).setCheckedState(integer));


        binding.doneBt.setOnClickListener(view1 -> {
            viewModel.submitChanges();
            Toast.makeText(requireContext(), R.string.new_setting_saved, Toast.LENGTH_SHORT).show();
        });

        ActivityResultLauncher<Intent> logoutLauncher = viewModel.getLogoutLauncher(this);
        binding.logoutBt.setOnClickListener(view1 -> {
            viewModel.logout(logoutLauncher);
        });

        viewModel.getEmail().observe(getViewLifecycleOwner(), email -> binding.emailTv.setText(email));
        viewModel.getName().observe(getViewLifecycleOwner(), name -> binding.nameTv.setText(name));
        viewModel.getShouldShowSessionExpired().observe(getViewLifecycleOwner(), shouldShowSessionExpired -> {
            if (shouldShowSessionExpired) {
                viewModel.getSessionExpiredDialog(this).show();
            }
        });
    }
}
