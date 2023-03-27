package uk.ac.cam.cares.jps.bmsqueryapp;

import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.volley.Request;
import com.android.volley.toolbox.StringRequest;


import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import okhttp3.HttpUrl.Builder;
import uk.ac.cam.cares.jps.bmsqueryapp.databinding.ActivityMainBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.ui.equipmentinstancelist.EquipmentAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.ui.equipmentinstancelist.OnEquipmentClickedListener;
import uk.ac.cam.cares.jps.bmsqueryapp.util.BaseArrayAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.util.Constants;
import uk.ac.cam.cares.jps.bmsqueryapp.util.SingletonConnection;

public class MainActivity extends AppCompatActivity {

    private static final Logger LOGGER = LogManager.getLogger(MainActivity.class);
//    private ActivityMainDepBinding binding;
    private ActivityMainBinding binding;
    private final Builder BMS_URL = Constants.constructUrlBuilder("bms-query-agent/retrieve/equipment");

    Map<String, String> equipmentInsts = new HashMap<>();
    EquipmentAdapter equipmentAdapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        BasicConfigurator.configure();

        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        // facility_spinner : fixed list, can be ignored for now
        initFacilitySelection();

        // equip_type_spinner: fixed list, use the selection to fetch the available instance of the type
        initTypeSelection();

        // equip_instance_list: get from BMSQueryAgent, when click on the item should jump to new activity for visualisation and modification
        initEquipmentInstanceList();

    }

    private void initFacilitySelection() {
        // TODO: not implemented yet
        BaseArrayAdapter<String> facilityAdapter = new BaseArrayAdapter<>(this);
        facilityAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        facilityAdapter.add("Facility Name");
        binding.facilitySpinner.setAdapter(facilityAdapter);
        binding.facilitySpinner.setOnItemSelectedListener(new OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> adapterView, View view, int i, long l) {
                String selectedLabel = ((TextView) view).getText().toString();
                LOGGER.info(selectedLabel + " is selected");
                if (i > 0) {
                    // TODO: Not Implemented
                } else {
                    ((TextView) view).setTextColor(Color.GRAY);
                }
            }

            @Override
            public void onNothingSelected(AdapterView<?> adapterView) {
                // Do nothing
            }
        });
    }

    private void initTypeSelection() {
        ArrayAdapter<String> typeAdapter = new BaseArrayAdapter<>(this);;
        typeAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        typeAdapter.add("Equipment Type");
        typeAdapter.addAll(Constants.EQUIPMENT_TYPES.keySet());
        binding.equipTypeSpinner.setAdapter(typeAdapter);
        binding.equipTypeSpinner.setOnItemSelectedListener(new OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> adapterView, View view, int i, long l) {
                String selectedLabel = ((TextView) view).getText().toString();
                LOGGER.info(selectedLabel + " is selected");
               if (selectedLabel.equals("Cooling Fan")) {
                   // TODO: temporary way to start the activity for fan edit
                    List<String> equipmentInstsList = new ArrayList<>();
                    equipmentInstsList.add("Cooling Fan #01");
                    equipmentInsts.clear();
                    equipmentInsts.put("Cooling Fan #01", "https://www.theworldavatar.com/kg/ontodevice/CoolingFan-01");
                    equipmentAdapter.updateEquipments(equipmentInstsList);
                } else if (i > 0) {
                    getListOfEquipInstances(Constants.EQUIPMENT_TYPES.get(selectedLabel));
                } else {
                    ((TextView) view).setTextColor(Color.GRAY);
                }
            }

            @Override
            public void onNothingSelected(AdapterView<?> adapterView) {
                // Do nothing
            }
        });
    }

    private void initEquipmentInstanceList() {
        OnEquipmentClickedListener listener = item -> {
            Intent intent = new Intent(getBaseContext(), EquipmentInstanceActivity.class);
            CharSequence selectedEquipment = item.getTextView().getText();
            intent.putExtra(EquipmentInstanceActivity.EQUIPMENT_LABEL, selectedEquipment);
            intent.putExtra(EquipmentInstanceActivity.EQUIPMENT_IRI, equipmentInsts.get(selectedEquipment.toString()));

            LOGGER.info("selected label: " + selectedEquipment);
            LOGGER.info("selected iri: " + equipmentInsts.get(selectedEquipment.toString()));
            LOGGER.info("start EquipmentInstanceActivity");

            startActivity(intent);
        };

        equipmentAdapter = new EquipmentAdapter(new ArrayList<>(equipmentInsts.keySet()), listener);
        binding.equipInstanceList.setLayoutManager(new LinearLayoutManager(this.getBaseContext()));
        binding.equipInstanceList.setAdapter(equipmentAdapter);
    }

    private void getListOfEquipInstances(String typeIRI) {

        String requestUri = BMS_URL.addQueryParameter("dataIRI", typeIRI).build().toString();

        StringRequest jsonRequest = new StringRequest(Request.Method.GET, requestUri,
                this::processEquipmentResponse, this::showFailureMessage);

        LOGGER.info("Sending GET request to " + jsonRequest);

        SingletonConnection.getInstance(this.getBaseContext()).addToRequestQueue(jsonRequest);

    }

    private void processEquipmentResponse(String responseStr) {
        LOGGER.info("Getting Response: " + responseStr);

        JSONObject response = null;
        try {
            response = new JSONObject(responseStr);
            parseEquipmentList(response);

            List<String> equipmentInstsList = new ArrayList<>(equipmentInsts.keySet());
            Collections.sort(equipmentInstsList);
            equipmentAdapter.updateEquipments(equipmentInstsList);
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }

    private void parseEquipmentList(JSONObject response) {
        try {
            for (int i = 0; i < response.getJSONArray("Equipments").length(); i ++) {
                JSONObject jo = response.getJSONArray("Equipments").getJSONObject(i);
                equipmentInsts.put(jo.getString("label"), jo.getString("dataIRI"));
            }
        } catch (JSONException e) {
            showFailureMessage(e);
        }
    }

    private void showFailureMessage(Exception response) {
        LOGGER.error(response.getMessage());
        Toast.makeText(getBaseContext(), "Unable to get the available equipment, please try again later", Toast.LENGTH_SHORT).show();
    }



}