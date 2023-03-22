package uk.ac.cam.cares.jps.bmsqueryapp;

import android.os.Bundle;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.viewpager2.widget.ViewPager2;

import com.android.volley.Request;
import com.android.volley.toolbox.StringRequest;
import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.bmsqueryapp.databinding.ActivityEquipmentInstanceBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.ui.tab.TabAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.util.Constants;
import okhttp3.HttpUrl.Builder;
import uk.ac.cam.cares.jps.bmsqueryapp.util.SingletonConnection;

public class EquipmentInstanceActivity extends AppCompatActivity {
    ActivityEquipmentInstanceBinding binding;
    private static final Logger LOGGER = LogManager.getLogger(EquipmentInstanceActivity.class);

    public static final String EQUIPMENT_LABEL = "equipmentLabel";
    public static final String EQUIPMENT_IRI = "equipmentIRI";

    TabAdapter adapter;

    private final Builder ANDROID_STATUS_AGENT_URL = Constants.constructUrlBuilder("android-status-agent/set");

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        BasicConfigurator.configure();

        binding = ActivityEquipmentInstanceBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        String equipmentLabel = getIntent().getStringExtra(EQUIPMENT_LABEL);
        String equipmentIri = getIntent().getStringExtra(EQUIPMENT_IRI);
        LOGGER.info(equipmentLabel);
        LOGGER.info(equipmentIri);
        binding.instanceTitle.setText(equipmentLabel);
        binding.returnButton.setOnClickListener(view -> {
            finish();
        });

        ViewPager2 viewPager = binding.viewPager;
        TabLayout tabLayout = binding.tabs;
        adapter = new TabAdapter(getSupportFragmentManager(), getLifecycle());
        viewPager.setAdapter(adapter);
        new TabLayoutMediator(tabLayout, viewPager, (tab, position) -> tab.setText(Constants.statusArrayTemp[position])).attach();

        sendEquipmentIri(equipmentIri);
    }

    private void sendEquipmentIri(String equipmentIri) {
        String requestUri = ANDROID_STATUS_AGENT_URL.addQueryParameter(EQUIPMENT_IRI, equipmentIri).build().toString();
        StringRequest jsonRequest = new StringRequest(Request.Method.POST, requestUri,
                this::showGraph, this::showFailureMessage);

        LOGGER.info("Sending POST request to " + jsonRequest);

        SingletonConnection.getInstance(this.getBaseContext()).addToRequestQueue(jsonRequest);
    }

    private void showGraph(String response) {
        adapter.getDtvfTab().loadDTVF();
    }

    private void showFailureMessage(Exception response) {
        LOGGER.error(response.getMessage());
        Toast.makeText(getBaseContext(), "Unable to get the available equipment, please try again later", Toast.LENGTH_SHORT).show();
    }

    // TODO: need to clear the android status agent?
//    @Override
//    protected void onDestroy() {
//        super.onDestroy();
//
//    }
}
