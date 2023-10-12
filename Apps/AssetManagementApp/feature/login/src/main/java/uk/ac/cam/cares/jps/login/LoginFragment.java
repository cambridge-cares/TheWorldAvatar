package uk.ac.cam.cares.jps.login;

import static android.app.Activity.RESULT_CANCELED;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.login.databinding.FragmentLoginBinding;

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
            if (hasLogin) {
                NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                        .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/home"))
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
//        FragmentManager fragmentManager = getParentFragmentManager();
//        while (fragmentManager.getBackStackEntryCount() > 0) {
//            fragmentManager.popBackStack();
//        }
    }

    private void showLoading() {
        binding.progressBar.setVisibility(View.VISIBLE);
        binding.signInOrUpButton.setVisibility(View.GONE);
    }

    private void hideLoading() {
        binding.progressBar.setVisibility(View.GONE);
        binding.signInOrUpButton.setVisibility(View.VISIBLE);
    }
}
