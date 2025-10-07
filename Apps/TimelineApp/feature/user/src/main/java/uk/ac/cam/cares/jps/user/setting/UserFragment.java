package uk.ac.cam.cares.jps.user.setting;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;

import org.apache.log4j.Logger;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.ui.base.UiUtils;
import uk.ac.cam.cares.jps.ui.impl.viewmodel.UserAccountViewModel;
import uk.ac.cam.cares.jps.user.R;
import uk.ac.cam.cares.jps.user.databinding.FragmentUserPageBinding;

/**
 * User page
 */
@AndroidEntryPoint
public class UserFragment extends Fragment {
    FragmentUserPageBinding binding;
    Logger LOGGER = Logger.getLogger(UserFragment.class);

    private UserAccountViewModel accountViewModel;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        accountViewModel = new ViewModelProvider(this).get(UserAccountViewModel.class);
        accountViewModel.getShouldShowSessionExpired().observe(getViewLifecycleOwner(), hasExpired -> {
            if (hasExpired) {
                accountViewModel.getSessionExpiredDialog(this).show();
            }
        });

        binding = FragmentUserPageBinding.inflate(inflater);
        binding.setLifecycleOwner(this);
        binding.setAccountViewModel(accountViewModel);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.mapTopAppbar.setNavigationOnClickListener(view1 -> NavHostFragment.findNavController(this).navigateUp());
        binding.accountSetting.setOnClickListener(view1 -> NavHostFragment.findNavController(this). navigate(R.id.action_global_account_setting));
        binding.sensorSetting.setOnClickListener(view1 -> NavHostFragment.findNavController(this). navigate(R.id.action_global_sensor_setting));
        binding.helpPage.setOnClickListener(view1 -> NavHostFragment.findNavController(this). navigate(R.id.action_global_help_page));
        binding.timelineSetting.setOnClickListener(view1 -> NavHostFragment.findNavController(this). navigate(R.id.action_global_timeline_setting));

        // todo: unimplemented button
        binding.healthReport.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.privacySetting.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.locationHistory.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));

        accountViewModel.getUserInfo();
    }

}

