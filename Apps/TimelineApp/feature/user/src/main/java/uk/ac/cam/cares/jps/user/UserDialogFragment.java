package uk.ac.cam.cares.jps.user;

import android.app.Dialog;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.DialogFragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import kotlin.Pair;
import uk.ac.cam.cares.jps.user.databinding.FragmentUserDialogBinding;
import uk.ac.cam.cares.jps.user.viewmodel.AccountViewModel;
import uk.ac.cam.cares.jps.user.viewmodel.SensorViewModel;
import uk.ac.cam.cares.jps.ui.UiUtils;

@AndroidEntryPoint
public class UserDialogFragment extends DialogFragment {

    private FragmentUserDialogBinding binding;
    private AccountViewModel accountViewModel;
    private SensorViewModel sensorViewModel;

    public static void show(@NonNull FragmentManager fragmentManager) {
        UserDialogFragment dialog = new UserDialogFragment();
        dialog.show(fragmentManager, "UserDialog");
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentUserDialogBinding.inflate(inflater, container, false);
        accountViewModel = new ViewModelProvider(this).get(AccountViewModel.class);
        sensorViewModel = new ViewModelProvider(requireActivity()).get(SensorViewModel.class);
        accountViewModel.registerForLogoutResult(this);
        binding.setLifecycleOwner(getViewLifecycleOwner());
        binding.setAccountViewModel(accountViewModel);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        if (accountViewModel.name.getValue() == null || accountViewModel.name.getValue().isEmpty()) {
            accountViewModel.getUserInfo();
        }

        accountViewModel.logoutStatus.observe(getViewLifecycleOwner(), status -> {
            if (status != null && Boolean.TRUE.equals(status.getFirst())) {

                sensorViewModel.clearManagers(status.getSecond());

                dismiss();
                navigate(R.string.onboarding_fragment_link);
            } else {
                Toast.makeText(requireContext(), uk.ac.cam.cares.jps.loginmodule.R.string.cancel_logout, Toast.LENGTH_SHORT).show();
            }
        });

        accountViewModel.shouldShowSessionExpired.observe(getViewLifecycleOwner(), expired -> {
            if (Boolean.TRUE.equals(expired)) {
                accountViewModel.getSessionExpiredDialog(this).show();
            }
        });

        binding.accountSetting.setOnClickListener(v -> {
            dismiss();
            navigate(R.string.account_setting_link);
        });

        binding.sensorSetting.setOnClickListener(v -> {
            dismiss();
            navigate(R.string.sensor_fragment_link);
        });

        binding.helpPage.setOnClickListener(v -> {
            dismiss();
            navigate(R.string.help_fragment_link);
        });

        binding.timelineSetting.setOnClickListener(v -> {
            dismiss();
            navigate(R.string.timeline_setting_link);
        });

        binding.privacySetting.setOnClickListener(v -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.healthReport.setOnClickListener(v -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.locationHistory.setOnClickListener(v -> UiUtils.showNotImplementedDialog(requireContext()));
        binding.logOut.setOnClickListener(v -> accountViewModel.logout());
    }

    private void navigate(int uriResId) {
        Uri uri = Uri.parse(requireContext().getString(uriResId));
        NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                .fromUri(uri)
                .build();
        NavHostFragment.findNavController(this).navigate(request);
    }

    @Override
    public void onStart() {
        super.onStart();
        Dialog dialog = getDialog();
        if (dialog != null && dialog.getWindow() != null) {
            Window window = dialog.getWindow();
            window.setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));

            WindowManager.LayoutParams params = window.getAttributes();
            params.width = (int) (getResources().getDisplayMetrics().widthPixels * 0.85);
            params.height = ViewGroup.LayoutParams.WRAP_CONTENT;
            window.setAttributes(params);
        }
    }
}
