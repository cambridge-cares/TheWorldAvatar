package uk.ac.cam.cares.jps.timeline;

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

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.DialogFragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.timelinemap.R;
import uk.ac.cam.cares.jps.timelinemap.databinding.FragmentUserDialogBinding;
import uk.ac.cam.cares.jps.ui.viewmodel.UserAccountViewModel;
import uk.ac.cam.cares.jps.timeline.viewmodel.RecordingViewModel;
import uk.ac.cam.cares.jps.ui.UiUtils;

@AndroidEntryPoint
public class UserDialogFragment extends DialogFragment {

    private FragmentUserDialogBinding binding;
    private UserAccountViewModel userAccountViewModel;
    private RecordingViewModel recordingViewModel;

    public static void show(@NonNull FragmentManager fragmentManager) {
        UserDialogFragment dialog = new UserDialogFragment();
        dialog.show(fragmentManager, "UserDialog");
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentUserDialogBinding.inflate(inflater, container, false);
        userAccountViewModel = new ViewModelProvider(requireActivity()).get(UserAccountViewModel.class);
        recordingViewModel = new ViewModelProvider(requireActivity()).get(RecordingViewModel.class);
        binding.setLifecycleOwner(getViewLifecycleOwner());
        binding.setUserAccountViewModel(userAccountViewModel);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        userAccountViewModel.logoutStatus.observe(getViewLifecycleOwner(), status -> {
            if (status != null && Boolean.TRUE.equals(status.getFirst())) {
                recordingViewModel.clearManagers(status.getSecond());
                dismiss();
                navigate(uk.ac.cam.cares.jps.timelinemap.R.string.onboarding_fragment_link);
            }
        });

        binding.accountSetting.setOnClickListener(v -> {
            dismiss();
            navigate(uk.ac.cam.cares.jps.timelinemap.R.string.account_setting_link);
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

        binding.logOut.setOnClickListener(v -> userAccountViewModel.logout(requireContext()));
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
