package uk.ac.cam.cares.jps.bmsqueryapp;

import android.os.Bundle;
import android.webkit.WebResourceError;
import android.webkit.WebResourceRequest;
import android.webkit.WebView;
import android.webkit.WebViewClient;
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
                binding.refreshButton.setEnabled(true);
            }

            @Override
            public void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error) {
                super.onReceivedError(view, request, error);
                LOGGER.error(error);
            }
        };

        ViewPager2 viewPager = binding.viewPager;
        TabLayout tabLayout = binding.tabs;
        adapter = new TabAdapter(getSupportFragmentManager(), getLifecycle());
        adapter.configDtvfTab(equipmentIri, webViewClient);
        adapter.configEditTab(getEditableAttributeList(equipmentType));
        viewPager.setAdapter(adapter);
        new TabLayoutMediator(tabLayout, viewPager, (tab, position) -> tab.setText(Constants.statusArrayTemp[position])).attach();

        binding.refreshButton.setOnClickListener(view -> {
            adapter.getDtvfTab().refreshDTVF();
            binding.refreshButton.setEnabled(false);
        });
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

}
