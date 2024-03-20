package uk.ac.cam.cares.jps.user;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.ui.UiUtils;
import uk.ac.cam.cares.jps.user.databinding.FragmentAccountSettingBinding;
import uk.ac.cam.cares.jps.user.viewmodel.AccountViewModel;
import uk.ac.cam.cares.jps.user.viewmodel.LoginViewModel;

@AndroidEntryPoint
public class AccountSettingFragment extends Fragment {

    private FragmentAccountSettingBinding binding;
    private AccountViewModel accountViewModel;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        accountViewModel = new ViewModelProvider(this).get(AccountViewModel.class);

        binding = FragmentAccountSettingBinding.inflate(inflater);
        binding.setLifecycleOwner(this);
        binding.setAccountViewModel(accountViewModel);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.mapTopAppbar.setNavigationOnClickListener(view1 -> NavHostFragment.findNavController(this).navigateUp());

        // todo: unimplemented button
        binding.editProfile.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.updatePassword.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.signOut.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.deleteAccount.setOnClickListener(view1 -> UiUtils.showNotImplementedDialog(requireContext()));

        accountViewModel.getUserInfo();
    }
}
