package uk.ac.cam.cares.jps.user;

import static android.app.Activity.RESULT_CANCELED;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
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
import androidx.viewpager2.widget.ViewPager2;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.util.Arrays;
import java.util.List;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.FragmentOnboardingBinding;
import uk.ac.cam.cares.jps.user.viewmodel.AccountViewModel;
import uk.ac.cam.cares.jps.user.viewmodel.LoginViewModel;
import uk.ac.cam.cares.jps.user.viewmodel.SensorViewModel;

@AndroidEntryPoint
public class OnboardingFragment extends Fragment {
    static final Logger LOGGER = LogManager.getLogger(OnboardingFragment.class);

    private FragmentOnboardingBinding binding;
    private LoginViewModel loginViewModel;
    private SensorViewModel sensorViewModel;
    private AccountViewModel accountViewModel;

    private ActivityResultLauncher<Intent> authorizationLauncher;

    private static final String PREF_NAME = "onboarding";
    private static final String SEEN_KEY = "seen";

    private Handler autoSlideHandler;
    private Runnable autoSlideRunnable;
    private boolean userSwiped = false;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BasicConfigurator.configure();
        binding = FragmentOnboardingBinding.inflate(inflater);
        loginViewModel = new ViewModelProvider(this).get(LoginViewModel.class);

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
                new Handler(Looper.getMainLooper()).postDelayed(() -> doubleBackToExitPressedOnce = false, 2000);
            }
        });

        authorizationLauncher = registerForActivityResult(
                new ActivityResultContracts.StartActivityForResult(),
                result -> {
                    if (result.getResultCode() == RESULT_CANCELED) {
                        Toast.makeText(requireContext(), R.string.login_canceled, Toast.LENGTH_SHORT).show();
                    } else {
                        showLoading();
                        loginViewModel.processAuthorizationResponse(result.getData());
                    }
                }
        );

        loginViewModel.getHasLogin().observe(getViewLifecycleOwner(), hasLogin -> {
            hideLoading();
            if (hasLogin) {
                initLoginDependentViewModel();
                sensorViewModel.registerPhoneToUser();
                NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                        .fromUri(Uri.parse(getString(uk.ac.cam.cares.jps.utils.R.string.timeline_fragment_link)))
                        .build();
                NavHostFragment.findNavController(this).navigate(request);
            }
        });

        loginViewModel.getLoginIntent().observe(getViewLifecycleOwner(), intent -> {
            if (intent != null) {
                authorizationLauncher.launch(intent);
            }
        });

        try {
            PackageInfo packageInfo = requireContext().getPackageManager().getPackageInfo(requireContext().getPackageName(), 0);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
                binding.versionTv.setText(packageInfo.versionName);
            }
        } catch (PackageManager.NameNotFoundException e) {
            throw new RuntimeException(e);
        }

        setupOnboardingPager();
        setupButtons();

        loginViewModel.initAuth();
        return binding.getRoot();
    }

    private void setupOnboardingPager() {
        List<Integer> realPages = Arrays.asList(
                R.layout.onboarding_page_1,
                R.layout.onboarding_page_2,
                R.layout.onboarding_page_3
        );

        List<Integer> loopedPages = Arrays.asList(
                realPages.get(2),
                realPages.get(0),
                realPages.get(1),
                realPages.get(2),
                realPages.get(0)
        );

        OnboardingAdapter adapter = new OnboardingAdapter(loopedPages);
        binding.viewPager.setAdapter(adapter);
        binding.viewPager.setCurrentItem(1, false);

        binding.dotsIndicator.setViewPager(binding.viewPager);
        binding.dotsIndicator.createIndicators(realPages.size(), 0);

        binding.viewPager.post(() -> binding.dotsIndicator.animatePageSelected(0));

        binding.viewPager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                if (position > 0 && position < adapter.getItemCount() - 1) {
                    int realPosition = (position - 1 + realPages.size()) % realPages.size();
                    binding.dotsIndicator.animatePageSelected(realPosition);
                }
                userSwiped = true;
                pauseAutoSlide();
                resumeAutoSlideWithDelay();
            }

            @Override
            public void onPageScrollStateChanged(int state) {
                super.onPageScrollStateChanged(state);
                if (state == ViewPager2.SCROLL_STATE_IDLE) {
                    int current = binding.viewPager.getCurrentItem();
                    int total = adapter.getItemCount();

                    if (current == 0) {
                        binding.viewPager.setCurrentItem(total - 2, false);
                    } else if (current == total - 1) {
                        binding.viewPager.setCurrentItem(1, false);
                    }
                }
            }
        });

        autoSlideHandler = new Handler(Looper.getMainLooper());
        autoSlideRunnable = () -> {
            if (!userSwiped) {
                int currentItem = binding.viewPager.getCurrentItem();
                if (currentItem == adapter.getItemCount() - 2) {
                    binding.viewPager.setCurrentItem(1, false);
                } else {
                    binding.viewPager.setCurrentItem(currentItem + 1, true);
                }
            }
            autoSlideHandler.postDelayed(autoSlideRunnable, 3000);
        };
        autoSlideHandler.postDelayed(autoSlideRunnable, 3000);
    }

    private void pauseAutoSlide() {
        autoSlideHandler.removeCallbacks(autoSlideRunnable);
    }

    private void resumeAutoSlideWithDelay() {
        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            userSwiped = false;
            autoSlideHandler.postDelayed(autoSlideRunnable, 3000);
        }, 3000);
    }

    private void setupButtons() {
        binding.signInOrUpButton.setOnClickListener(v -> {
            showLoading();
            loginViewModel.doAuth();
        });
    }

    private void showLoading() {
        binding.progressBar.setVisibility(View.VISIBLE);
        binding.signInOrUpButton.setVisibility(View.GONE);
    }

    private void hideLoading() {
        binding.progressBar.setVisibility(View.GONE);
        binding.signInOrUpButton.setVisibility(View.VISIBLE);
    }

    private void initLoginDependentViewModel() {
        sensorViewModel = new ViewModelProvider(this).get(SensorViewModel.class);
        accountViewModel = new ViewModelProvider(this).get(AccountViewModel.class);
        sensorViewModel.getHasAccountError().observe(getViewLifecycleOwner(), hasAccountError -> {
            if (hasAccountError) {
                accountViewModel.getSessionExpiredDialog(this).show();
            }
        });
    }

    @Override
    public void onStart() {
        super.onStart();
        clearAllActiveFragments();
    }

    @Override
    public void onStop() {
        super.onStop();
        if (autoSlideHandler != null && autoSlideRunnable != null) {
            autoSlideHandler.removeCallbacks(autoSlideRunnable);
        }
    }

    private void clearAllActiveFragments() {
        FragmentTransaction transaction = getParentFragmentManager().beginTransaction();
        for (Fragment fragment : getParentFragmentManager().getFragments()) {
            if (fragment instanceof OnboardingFragment) continue;
            transaction.remove(fragment);
        }
        transaction.commit();
    }
}
