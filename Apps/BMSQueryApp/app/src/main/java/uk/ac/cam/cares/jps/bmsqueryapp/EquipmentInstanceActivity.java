package uk.ac.cam.cares.jps.bmsqueryapp;

import android.app.Activity;
import android.content.Intent;
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

import uk.ac.cam.cares.jps.bmsqueryapp.authorization.AuthorizationHelper;
import uk.ac.cam.cares.jps.bmsqueryapp.data.attribute.EditableAttribute;
import uk.ac.cam.cares.jps.bmsqueryapp.databinding.ActivityEquipmentInstanceBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.Constants;
import uk.ac.cam.cares.jps.bmsqueryapp.view.tab.TabAdapter;

public class EquipmentInstanceActivity extends AppCompatActivity{
    ActivityEquipmentInstanceBinding binding;
    private static final Logger LOGGER = LogManager.getLogger(EquipmentInstanceActivity.class);
    private AuthorizationHelper authHelper;

    public static final String EQUIPMENT_LABEL = "equipmentLabel";
    public static final String EQUIPMENT_IRI = "equipmentIRI";
    public static final String EQUIPMENT_TYPE = "equipmentType";

    private static final int END_SESSION_REQUEST_CODE = 911;

    TabAdapter adapter;
    int tabPosition;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        BasicConfigurator.configure();

        binding = ActivityEquipmentInstanceBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        authHelper = AuthorizationHelper.getInstance(this);

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
        adapter.configEditTab(getEditableAttributeList(equipmentType));
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
                    adapter.getEditTab().clearInputs();
                }
            }
        });

        getSupportFragmentManager().setFragmentResultListener("startLogin", this,
                (requestKey, result) -> {
                    startActivityForResult(authHelper.getLogOutIntent(), END_SESSION_REQUEST_CODE);
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

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == END_SESSION_REQUEST_CODE && resultCode == Activity.RESULT_OK) {
            authHelper.clearSharedPref();
            Intent loginIntent = new Intent(this, LoginActivity.class);
            loginIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(loginIntent);
            finish();
        } else {
            Toast.makeText(this, R.string.cancel_logout, Toast.LENGTH_SHORT).show();
        }
    }

}
