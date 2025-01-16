package uk.ac.cam.cares.jps.user;

import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.ui.UiUtils;
import uk.ac.cam.cares.jps.user.databinding.FragmentAccountSettingBinding;
import uk.ac.cam.cares.jps.user.viewmodel.AccountViewModel;
import uk.ac.cam.cares.jps.user.viewmodel.SensorViewModel;

/**
 * Account setting page
 */
@AndroidEntryPoint
public class AccountSettingFragment extends Fragment {

    private FragmentAccountSettingBinding binding;
    private AccountViewModel accountViewModel;
    private SensorViewModel sensorViewModel;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        sensorViewModel = new ViewModelProvider(requireActivity()).get(SensorViewModel.class);

        accountViewModel = new ViewModelProvider(this).get(AccountViewModel.class);
        accountViewModel.registerForLogoutResult(this);
        accountViewModel.logoutStatus.observe(getViewLifecycleOwner(), logoutStatus -> {
            if (logoutStatus.getFirst()) {
                sensorViewModel.clearManagers(logoutStatus.getSecond());

                NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                        .fromUri(Uri.parse(requireContext().getString(uk.ac.cam.cares.jps.utils.R.string.login_fragment_link)))
                        .build();
                NavHostFragment.findNavController(this).navigate(request);
            } else {
                Toast.makeText(requireContext(), uk.ac.cam.cares.jps.loginmodule.R.string.cancel_logout, Toast.LENGTH_SHORT).show();
            }
        });
        accountViewModel.shouldShowSessionExpired.observe(getViewLifecycleOwner(), hasExpired -> {
            if (hasExpired) {
                accountViewModel.getSessionExpiredDialog(this).show();
            }
        });

        binding = FragmentAccountSettingBinding.inflate(inflater);
        binding.setLifecycleOwner(this);
        binding.setAccountViewModel(accountViewModel);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.mapTopAppbar.setNavigationOnClickListener(view1 -> NavHostFragment.findNavController(this).navigateUp());
        binding.signOut.setOnClickListener(view1 -> accountViewModel.logout());

        // todo: unimplemented button
        binding.editProfile.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.updatePassword.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.deleteAccount.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));

        accountViewModel.getUserInfo();
    }
}
