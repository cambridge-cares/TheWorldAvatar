package uk.ac.cam.cares.jps.login;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.navigation.NavOptions;
import androidx.navigation.fragment.NavHostFragment;
import androidx.viewpager2.widget.ViewPager2;

import java.util.Arrays;
import java.util.List;

import me.relex.circleindicator.CircleIndicator3;
import uk.ac.cam.cares.jps.loginmodule.R;

public class OnboardingFragment extends Fragment {

    private static final String PREF_NAME = "onboarding";
    private static final String SEEN_KEY = "seen";

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        // Skip onboarding if already seen
        SharedPreferences prefs = requireContext().getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
        if (prefs.getBoolean(SEEN_KEY, false)) {
            NavHostFragment.findNavController(this).navigate(R.id.login_fragment);
            return null;
        }

        View view = inflater.inflate(R.layout.fragment_onboarding, container, false);

        ViewPager2 viewPager = view.findViewById(R.id.viewPager);
        CircleIndicator3 indicator = view.findViewById(R.id.dots_indicator);
        Button btnSignIn = view.findViewById(R.id.btnSignIn);
        Button btnCreateAccount = view.findViewById(R.id.btnCreateAccount);

        List<Integer> layouts = Arrays.asList(
                R.layout.onboarding_page_1,
                R.layout.onboarding_page_2,
                R.layout.onboarding_page_3
        );

        OnboardingAdapter adapter = new OnboardingAdapter(layouts);
        viewPager.setAdapter(adapter);
        viewPager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);
                indicator.animatePageSelected(position);
            }
        });
        indicator.createIndicators(layouts.size(), 0);


        btnSignIn.setOnClickListener(v -> {
            prefs.edit().putBoolean(SEEN_KEY, true).apply();
            NavOptions options = new NavOptions.Builder()
                    .setPopUpTo(R.id.onboarding_fragment, true)
                    .build();
            NavHostFragment.findNavController(this).navigate(R.id.login_fragment, null, options);
        });

        btnCreateAccount.setOnClickListener(v -> {
            prefs.edit().putBoolean(SEEN_KEY, true).apply();
            NavOptions options = new NavOptions.Builder()
                    .setPopUpTo(R.id.onboarding_fragment, true)
                    .build();
            NavHostFragment.findNavController(this).navigate(R.id.create_account_fragment, null, options);
        });

        return view;
    }
}
