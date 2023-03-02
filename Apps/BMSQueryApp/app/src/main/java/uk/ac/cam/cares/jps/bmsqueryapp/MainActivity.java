package uk.ac.cam.cares.jps.bmsqueryapp;

import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;
import androidx.viewpager2.widget.ViewPager2;

import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;

import uk.ac.cam.cares.jps.bmsqueryapp.databinding.ActivityMainBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.ui.main.TabAdapter;

import uk.ac.cam.cares.jps.bmsqueryapp.util.RequestStatus;

public class MainActivity extends AppCompatActivity {

    private ActivityMainBinding binding;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        ViewPager2 viewPager = binding.viewPager;
        TabLayout tabLayout = binding.tabs;
        TabAdapter adapter = new TabAdapter(getSupportFragmentManager(), getLifecycle());
        viewPager.setAdapter(adapter);

        new TabLayoutMediator(tabLayout, viewPager, (tab, position) -> tab.setText(RequestStatus.statusArray[position])).attach();
    }
}