package uk.ac.cam.cares.jps.assetmanagementapp;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

public class AssetInfoActivity extends AppCompatActivity {

    private static final Logger LOGGER = LogManager.getLogger(AssetInfoActivity.class);
    public static final String ASSET_URI = "assetUri";
    private String assetUri;
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        BasicConfigurator.configure();

        assetUri = getIntent().getStringExtra(ASSET_URI);

        LOGGER.info(assetUri);
    }
}
