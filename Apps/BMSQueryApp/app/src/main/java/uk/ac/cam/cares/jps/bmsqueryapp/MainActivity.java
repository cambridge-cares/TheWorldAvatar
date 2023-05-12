package uk.ac.cam.cares.jps.bmsqueryapp;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.Spinner;
import android.widget.SpinnerAdapter;
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
import java.util.Iterator;
import java.util.List;

import uk.ac.cam.cares.jps.bmsqueryapp.databinding.ActivityMainBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.data.buildings.Building;
import uk.ac.cam.cares.jps.bmsqueryapp.data.buildings.Room;
import uk.ac.cam.cares.jps.bmsqueryapp.adapter.list.EquipmentAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.adapter.list.OnEquipmentClickedListener;
import uk.ac.cam.cares.jps.bmsqueryapp.adapter.spinner.BaseArrayAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.Constants;
import uk.ac.cam.cares.jps.bmsqueryapp.adapter.spinner.InstanceAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.SingletonConnection;

public class MainActivity extends AppCompatActivity {

    private static final Logger LOGGER = LogManager.getLogger(MainActivity.class);
    private ActivityMainBinding binding;

    private final String BMS_RETRIEVE_ZONES = "bms-query-agent/retrieve/zones";
    private final String BMS_RETRIEVE_EQUIPMENT = "bms-query-agent/retrieve/equipment";

    ArrayList<Building> buildings = new ArrayList<>();
    ArrayList<Spinner> spinners = new ArrayList<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        BasicConfigurator.configure();

        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        // create spinner levels with arraylist
        spinners.add(binding.buildingSpinner);
        spinners.add(binding.facilitySpinner);
        spinners.add(binding.roomSpinner);
        spinners.add(binding.equipTypeSpinner);


        initZoneSpinner(binding.buildingSpinner, getListenerClearViewsAndLoadSubLevelChoices(binding.buildingSpinner));
        initZoneSpinner(binding.facilitySpinner, getListenerClearViewsAndLoadSubLevelChoices(binding.facilitySpinner));
        initZoneSpinner(binding.roomSpinner, getListenerClearViewsAndQueryEquipment(binding.roomSpinner));
        initTypeSpinner(null);    // attach click listener when getting the list of equipment

        initEquipmentInstanceList();
        getZonesFromAgent();

        binding.refreshButton.setOnClickListener(view -> {
            getZonesFromAgent();
        });

    }

    private void getZonesFromAgent() {
        String requestUri = Constants.constructUrlBuilder(BMS_RETRIEVE_ZONES)
                .build().toString();

        StringRequest jsonRequest = new StringRequest(Request.Method.GET, requestUri,
                this::buildBuildingsList, this::showFailureMessage);

        LOGGER.info("Sending GET request to " + jsonRequest);

        SingletonConnection.getInstance(this.getBaseContext()).addToRequestQueue(jsonRequest);
    }

    private void buildBuildingsList(String responseStr) {
        JSONObject response = null;
        try {
            response = new JSONObject(responseStr).getJSONObject("buildings");
            Iterator<String> iter = response.keys();
            while (iter.hasNext()) {
                String buildingIri = iter.next();
                Building building = new Building(response.getJSONObject(buildingIri), buildingIri);
                buildings.add(building);
            }
        } catch (JSONException e) {
            // todo: test with empty json
            throw new RuntimeException("Unable to parse the JSONObject returned from the BMSQueryAgent");
        }

        LOGGER.info("Finished building. Created " + buildings.size() + "buildings.");
        ((InstanceAdapter) binding.buildingSpinner.getAdapter()).addAll(buildings);
    }

    private void initZoneSpinner(Spinner spinner, OnItemSelectedListener listener) {
        InstanceAdapter adapter = new InstanceAdapter(this, "Please select");
        adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        spinner.setAdapter(adapter);

        spinner.setOnItemSelectedListener(listener);
    }

    private void clearViewsInSubLevel(int currentSpinnerIndex) {
        // remove all sub-levels spinner's content
        for (int j = currentSpinnerIndex + 1; j < spinners.size(); j++) {
            SpinnerAdapter currentAdapter = spinners.get(j).getAdapter();
            if (currentAdapter instanceof InstanceAdapter) {
                ((InstanceAdapter) currentAdapter).clear();
            } else if (currentAdapter instanceof BaseArrayAdapter) {
                ((BaseArrayAdapter<?>) currentAdapter).clear();
            }
        }

        // remove instance lists
        ((EquipmentAdapter) binding.equipInstanceList.getAdapter()).clear();
    }

    private OnItemSelectedListener getListenerClearViewsAndLoadSubLevelChoices(Spinner spinner) {
        return new OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> adapterView, View view, int i, long l) {
                if (i > 0) {
                    int currentSpinnerIndex = spinners.indexOf(spinner);

                    clearViewsInSubLevel(currentSpinnerIndex);

                    // add choices for spinner one level lower
                    InstanceAdapter currentAdapter = (InstanceAdapter) spinners.get(currentSpinnerIndex).getAdapter();
                    InstanceAdapter subLevelAdapter = (InstanceAdapter) spinners.get(currentSpinnerIndex + 1).getAdapter();
                    subLevelAdapter.addAll(currentAdapter.getItem(i).getSortedSubLevelItems());
                }
            }

            @Override
            public void onNothingSelected(AdapterView<?> adapterView) {

            }
        };
    }

    private OnItemSelectedListener getListenerClearViewsAndQueryEquipment(Spinner spinner) {
        return new OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> adapterView, View view, int i, long l) {
                if (i > 0) {
                    int currentSpinnerIndex = spinners.indexOf(spinner);
                    InstanceAdapter currentAdapter = (InstanceAdapter) spinners.get(currentSpinnerIndex).getAdapter();

                    clearViewsInSubLevel(currentSpinnerIndex);

                    getListOfEquipInstances((Room) currentAdapter.getItem(i));

                    // choices are added to type spinner in the http callback
                }
            }

            @Override
            public void onNothingSelected(AdapterView<?> adapterView) {

            }
        };
    }

    private OnItemSelectedListener getListenerLoadNewEquipmentList(Room currentRoom) {
        return new OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> adapterView, View view, int i, long l) {
                String selectedLabel = ((TextView) view).getText().toString();
                LOGGER.info(selectedLabel + " is selected");

                // build list
                ((EquipmentAdapter) binding.equipInstanceList.getAdapter()).updateEquipments(currentRoom.getSortedSubLevelItemsOfGivenType(selectedLabel));
            }

            @Override
            public void onNothingSelected(AdapterView<?> adapterView) {
                // Do nothing
            }
        };
    }

    private void initTypeSpinner(Room currentRoom) {
        if (currentRoom == null) {
            BaseArrayAdapter<String> typeAdapter = new BaseArrayAdapter<>(this, "Please select");
            typeAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
            binding.equipTypeSpinner.setAdapter(typeAdapter);
            return;
        }

        List<String> types = currentRoom.getEquipmentTypes();

        BaseArrayAdapter<String> typeAdapter = (BaseArrayAdapter<String>) binding.equipTypeSpinner.getAdapter();
        typeAdapter.addAll(types);
        binding.equipTypeSpinner.setOnItemSelectedListener(getListenerLoadNewEquipmentList(currentRoom));
    }

    private void getListOfEquipInstances(Room currentRoom) {

        String requestUri = Constants.constructUrlBuilder(BMS_RETRIEVE_EQUIPMENT)
                .addQueryParameter("roomIRI", currentRoom.getIri())
                .build().toString();

        StringRequest jsonRequest = new StringRequest(Request.Method.GET, requestUri,
                responseStr -> {
                    LOGGER.info("Getting Response: " + responseStr);
                    try {
                        currentRoom.buildEquipmentFromJSON(new JSONObject(responseStr).getJSONArray("equipment"));

                        initTypeSpinner(currentRoom);

                    } catch (JSONException e) {
                        throw new RuntimeException(e);
                    }
                }, this::showFailureMessage);

        LOGGER.info("Sending GET request to " + jsonRequest);

        SingletonConnection.getInstance(this.getBaseContext()).addToRequestQueue(jsonRequest);

    }

    private void initEquipmentInstanceList() {
        OnEquipmentClickedListener listener = (item, equipment) -> {
            Intent intent = new Intent(getBaseContext(), EquipmentInstanceActivity.class);
            intent.putExtra(EquipmentInstanceActivity.EQUIPMENT_LABEL, equipment.getLabel());
            intent.putExtra(EquipmentInstanceActivity.EQUIPMENT_IRI, equipment.getIri());
            intent.putExtra(EquipmentInstanceActivity.EQUIPMENT_TYPE, equipment.getType());

            LOGGER.info("selected label: " + equipment.getLabel());
            LOGGER.info("selected iri: " + equipment.getIri());
            LOGGER.info("start EquipmentInstanceActivity");

            startActivity(intent);
        };

        EquipmentAdapter equipmentAdapter = new EquipmentAdapter(new ArrayList<>(), listener);
        binding.equipInstanceList.setLayoutManager(new LinearLayoutManager(this.getBaseContext()));
        binding.equipInstanceList.setAdapter(equipmentAdapter);
    }

    private void showFailureMessage(Exception response) {
        LOGGER.error(response.getMessage());
        Toast.makeText(getBaseContext(), "Unable to get the available equipment, please try again later", Toast.LENGTH_SHORT).show();
    }
}