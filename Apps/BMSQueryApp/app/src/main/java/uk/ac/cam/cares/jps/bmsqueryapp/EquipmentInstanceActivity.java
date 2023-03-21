package uk.ac.cam.cares.jps.bmsqueryapp;

import android.os.Bundle;
import android.os.PersistableBundle;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.viewpager2.widget.ViewPager2;

import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.bmsqueryapp.databinding.ActivityEquipmentInstanceBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.ui.tab.TabAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.util.RequestStatus;

public class EquipmentInstanceActivity extends AppCompatActivity {
    ActivityEquipmentInstanceBinding binding;
    private static final Logger LOGGER = LogManager.getLogger(EquipmentInstanceActivity.class);

    public static final String EQUIPMENT_LABEL = "equipment_label";
    public static final String EQUIPMENT_IRI = "equipment_iri";

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        BasicConfigurator.configure();

        LOGGER.info(getIntent().getStringExtra(EQUIPMENT_LABEL));
        LOGGER.info(getIntent().getStringExtra(EQUIPMENT_IRI));

        binding = ActivityEquipmentInstanceBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        ViewPager2 viewPager = binding.viewPager;
        TabLayout tabLayout = binding.tabs;
        TabAdapter adapter = new TabAdapter(getSupportFragmentManager(), getLifecycle());
        viewPager.setAdapter(adapter);

        new TabLayoutMediator(tabLayout, viewPager, (tab, position) -> tab.setText(RequestStatus.statusArrayTemp[position])).attach();
    }
}
