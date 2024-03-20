package uk.ac.cam.cares.jps.user;

import static android.app.Activity.RESULT_CANCELED;

import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentTransaction;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.FragmentLoginBinding;
import uk.ac.cam.cares.jps.user.viewmodel.LoginViewModel;

@AndroidEntryPoint
public class LoginFragment extends Fragment {
    private static final Logger LOGGER = LogManager.getLogger(LoginFragment.class);

    private FragmentLoginBinding binding;
    private LoginViewModel viewModel;

    private ActivityResultLauncher<Intent> authorizationLauncher;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BasicConfigurator.configure();
        binding = FragmentLoginBinding.inflate(inflater);
        viewModel = new ViewModelProvider(this).get(LoginViewModel.class);

        requireActivity().getOnBackPressedDispatcher().addCallback(getViewLifecycleOwner(), new OnBackPressedCallback(true) {
            private boolean doubleBackToExitPressedOnce;

            @Override
            public void handleOnBackPressed() {
                if (doubleBackToExitPressedOnce) {
                    requireActivity().finishAffinity();
                    return;
                }

                this.doubleBackToExitPressedOnce = true;
                Toast.makeText(requireContext(), "Please click BACK again to exit", Toast.LENGTH_SHORT).show();

                new Handler(Looper.getMainLooper()).postDelayed(() -> doubleBackToExitPressedOnce=false, 2000);
            }
        });

        authorizationLauncher = registerForActivityResult(
                new ActivityResultContracts.StartActivityForResult(),
                result -> {
                    if (result.getResultCode() == RESULT_CANCELED) {
                        Toast.makeText(requireContext(), R.string.login_canceled, Toast.LENGTH_SHORT).show();
                    } else {
                        showLoading();
                        viewModel.processAuthorizationResponse(result.getData());
                    }
                }
        );

        viewModel.getHasLogin().observe(getViewLifecycleOwner(), hasLogin -> {
            hideLoading();
            if (hasLogin) {
                NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                        .fromUri(Uri.parse(getString(uk.ac.cam.cares.jps.utils.R.string.timeline_fragment_link)))
                        .build();
                NavHostFragment.findNavController(this).navigate(request);
            }
        });

//        viewModel.getToastErrorMessage().observe(getViewLifecycleOwner(), errorMsgId -> Toast.makeText(requireContext(), errorMsgId, Toast.LENGTH_SHORT).show());

        binding.signInOrUpButton.setOnClickListener(bt -> viewModel.doAuth());
        viewModel.getLoginIntent().observe(getViewLifecycleOwner(), intent -> {
            if (intent != null) {
                authorizationLauncher.launch(intent);
            }
        });

        try {
            PackageInfo packageInfo = requireContext().getPackageManager().getPackageInfo(requireContext().getPackageName(), 0);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
                binding.versionTv.setText("" + packageInfo.getLongVersionCode());
            }
        } catch (PackageManager.NameNotFoundException e) {
            throw new RuntimeException(e);
        }


        viewModel.initAuth();
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
    }

    @Override
    public void onStart() {
        super.onStart();
        clearAllActiveFragment();
    }

    private void showLoading() {
        binding.progressBar.setVisibility(View.VISIBLE);
        binding.signInOrUpButton.setVisibility(View.GONE);
    }

    private void hideLoading() {
        binding.progressBar.setVisibility(View.GONE);
        binding.signInOrUpButton.setVisibility(View.VISIBLE);
    }


    private void clearAllActiveFragment() {
        FragmentTransaction transaction = getParentFragmentManager().beginTransaction();
        for (Fragment fragment : getParentFragmentManager().getFragments()) {
            if (fragment instanceof LoginFragment) {
                continue;
            }

            transaction.remove(fragment);
        }
        transaction.commit();
    }


}
