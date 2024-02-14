package uk.ac.cam.cares.jps.bmsqueryapp;

import android.os.Bundle;
import android.webkit.ValueCallback;
import android.webkit.WebResourceError;
import android.webkit.WebResourceRequest;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.viewpager2.widget.ViewPager2;

import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.bmsqueryapp.data.attribute.EditableAttribute;
import uk.ac.cam.cares.jps.bmsqueryapp.data.dict.IRIMapping;
import uk.ac.cam.cares.jps.bmsqueryapp.databinding.ActivityEquipmentInstanceBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.view.tab.EditFragment;
import uk.ac.cam.cares.jps.bmsqueryapp.view.tab.TabAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.Constants;

public class EquipmentInstanceActivity extends AppCompatActivity {
    ActivityEquipmentInstanceBinding binding;
    private static final Logger LOGGER = LogManager.getLogger(EquipmentInstanceActivity.class);

    public static final String EQUIPMENT_LABEL = "equipmentLabel";
    public static final String EQUIPMENT_IRI = "equipmentIRI";
    public static final String EQUIPMENT_TYPE = "equipmentType";

    TabAdapter adapter;
    int tabPosition;

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

        binding.instanceTitle.setText(equipmentLabel);
        binding.returnButton.setOnClickListener(view -> {
            finish();
        });

        WebViewClient webViewClient = new WebViewClient() {
            @Override
            public void onPageFinished(WebView view, String url) {
                super.onPageFinished(view, url);
                LOGGER.info("page finished loading");
//                binding.progressBarWrapper.setVisibility(View.GONE);
            }

            @Override
            public void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error) {
                super.onReceivedError(view, request, error);
                LOGGER.error(error);
            }
        };

        ValueCallback<String> reloadCallback = (ValueCallback<String>) o -> {
            LOGGER.info("New data loaded");
            binding.refreshButton.setEnabled(true);
        };

        ViewPager2 viewPager = binding.viewPager;
        TabLayout tabLayout = binding.tabs;
        adapter = new TabAdapter(getSupportFragmentManager(), getLifecycle());
        adapter.configDtvfTab(equipmentIri, webViewClient, reloadCallback);
        adapter.configEditTab(getEditableAttributeList(equipmentType, equipmentIri));
        viewPager.setAdapter(adapter);
        new TabLayoutMediator(tabLayout, viewPager, (tab, position) -> tab.setText(Constants.statusArrayTemp[position])).attach();
        tabLayout.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                tabPosition = tab.getPosition();
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {;}

            @Override
            public void onTabReselected(TabLayout.Tab tab) {;}
        });

        binding.refreshButton.setOnClickListener(view -> {
            if (tabPosition == 0) {
                Toast.makeText(this.getBaseContext(), "Loading new data", Toast.LENGTH_SHORT).show();
                if (adapter.getDtvfTab() != null) {
                    adapter.getDtvfTab().refreshDTVF();
                    binding.refreshButton.setEnabled(false);
                }
            } else if (tabPosition == 1) {
                if (adapter.getEditTab() != null) {
                    ((EditFragment) adapter.getEditTab()).clearInputs();
                }
            }
        });
    }

    private List<EditableAttribute> getEditableAttributeList(String type, String equipmentIRI) {
        IRIMapping iriMapping = new IRIMapping();
        // determine the list of editable attribute based on the equipment type
        if (type.equals("https://www.theworldavatar.com/kg/ontobms/WalkInFumeHood")) {
            return new ArrayList<>();
        } else if (type.equals("https://www.theworldavatar.com/kg/ontodevice/SmartSensor") || type.equals("https://w3id.org/s3n/SmartSensor")) {
            ArrayList<EditableAttribute> attributes = new ArrayList<>();
            attributes.add(new EditableAttribute("https://www.theworldavatar.com/kg/ontodevice/V_Setpoint-01-Temperature", "Temperature Setpoint", "double", "°C"));
            return attributes;
        } else if (type.equals("https://www.theworldavatar.com/kg/ontobms/ExhaustVAV") || type.equals("https://www.theworldavatar.com/kg/ontobms/CanopyHood")) {
            ArrayList<EditableAttribute> attributes = new ArrayList<>();
            String editableDataIRI = iriMapping.getEditableDataIRIFromEquipmentIRI(equipmentIRI);
            if (editableDataIRI != null) {
                attributes.add(new EditableAttribute(editableDataIRI, "Airflow Setpoint", "double", "m3/h"));
                return attributes;
            }
        }
        return new ArrayList<>();
    }

}
