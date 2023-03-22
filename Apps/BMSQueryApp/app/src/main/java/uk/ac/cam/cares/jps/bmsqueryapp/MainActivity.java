package uk.ac.cam.cares.jps.bmsqueryapp;

import android.content.Intent;
import android.os.Bundle;
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
import java.util.HashMap;
import java.util.Map;

import okhttp3.HttpUrl.Builder;
import uk.ac.cam.cares.jps.bmsqueryapp.databinding.ActivityMainBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.ui.equipmentinstancelist.EquipmentAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.ui.equipmentinstancelist.OnEquipmentClickedListener;
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

        // spinner 1: fixed list, can be ignored

        // spinner 2: fixed list, need to use to fetch the available instance

        // recycleview 1: get from FIA, when click on the item should jump to new activity for visualisation and modification

        initEquipmentRecyclerView();
        getListOfEquipInstances("http://www.theworldavatar.com/BMS/CaresLab#WalkIn-FumeHood");
    }

    private void initEquipmentRecyclerView() {
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

            equipmentAdapter.updateEquipments(new ArrayList<>(equipmentInsts.keySet()));
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