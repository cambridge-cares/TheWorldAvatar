package uk.ac.cam.cares.jps.user;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.Navigation;
import androidx.navigation.fragment.NavHostFragment;

import org.apache.log4j.Logger;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.ui.UiUtils;
import uk.ac.cam.cares.jps.user.databinding.FragmentUserPageBinding;
import uk.ac.cam.cares.jps.user.viewmodel.AccountViewModel;

@AndroidEntryPoint
public class UserFragment extends Fragment {
    FragmentUserPageBinding binding;
    Logger LOGGER = Logger.getLogger(UserFragment.class);

    private AccountViewModel accountViewModel;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        accountViewModel = new ViewModelProvider(this).get(AccountViewModel.class);

        binding = FragmentUserPageBinding.inflate(inflater);
        binding.setLifecycleOwner(this);
        binding.setAccountViewModel(accountViewModel);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.mapTopAppbar.setNavigationOnClickListener(view1 -> NavHostFragment.findNavController(this).navigateUp());
        binding.accountSetting.setOnClickListener(view1 -> NavHostFragment.findNavController(this). navigate(R.id.action_user_fragment_to_account_setting));

        // todo: unimplemented button
        binding.healthReport.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.privacySetting.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.locationHistory.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.sensorSetting.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));

        accountViewModel.getUserInfo();
    }

}
