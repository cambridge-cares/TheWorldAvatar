package uk.ac.cam.cares.jps.bmsqueryapp;

import android.os.Bundle;
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

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.bmsqueryapp.data.attribtue.EditableAttribute;
import uk.ac.cam.cares.jps.bmsqueryapp.databinding.ActivityEquipmentInstanceBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.view.tab.TabAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.Constants;
import okhttp3.HttpUrl.Builder;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.SingletonConnection;

public class EquipmentInstanceActivity extends AppCompatActivity {
    ActivityEquipmentInstanceBinding binding;
    private static final Logger LOGGER = LogManager.getLogger(EquipmentInstanceActivity.class);

    public static final String EQUIPMENT_LABEL = "equipmentLabel";
    public static final String EQUIPMENT_IRI = "equipmentIRI";
    public static final String EQUIPMENT_TYPE = "equipmentType";

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
        String equipmentType = getIntent().getStringExtra(EQUIPMENT_TYPE);

        LOGGER.info(equipmentLabel);
        LOGGER.info(equipmentIri);

        sendEquipmentIri(equipmentIri);

        binding.instanceTitle.setText(equipmentLabel);
        binding.returnButton.setOnClickListener(view -> {
            finish();
        });

        ViewPager2 viewPager = binding.viewPager;
        TabLayout tabLayout = binding.tabs;
        adapter = new TabAdapter(getSupportFragmentManager(), getLifecycle(), getEditableAttributeList(equipmentType));
        viewPager.setAdapter(adapter);
        new TabLayoutMediator(tabLayout, viewPager, (tab, position) -> tab.setText(Constants.statusArrayTemp[position])).attach();

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

    private List<EditableAttribute> getEditableAttributeList(String type) {
        // determine the list of editable attribute based on the equipment type
        if (type.equals("https://www.theworldavatar.com/kg/ontobms/WalkInFumeHood")) {
            return new ArrayList<>();
        } else if (type.equals("https://www.theworldavatar.com/kg/ontodevice/SmartSensor") || type.equals("https://w3id.org/s3n/SmartSensor")) {
            ArrayList<EditableAttribute> attributes = new ArrayList<>();
            attributes.add(new EditableAttribute("https://www.theworldavatar.com/kg/ontodevice/V_Setpoint-01-Temperature", "Temperature", "double", "°C"));
            return attributes;
        }
        return new ArrayList<>();
    }

    // TODO: need to clear the android status agent?
//    @Override
//    protected void onDestroy() {
//        super.onDestroy();
//
//    }
}
