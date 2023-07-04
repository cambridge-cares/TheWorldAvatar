package uk.ac.cam.cares.jps.assetmanagementapp;

import android.content.Intent;
import android.os.Bundle;
import android.os.PersistableBundle;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

import org.apache.log4j.BasicConfigurator;

import uk.ac.cam.cares.jps.assetmanagementapp.databinding.ActivityMainBinding;

public class MainActivity extends AppCompatActivity {
    private ActivityMainBinding binding;
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        BasicConfigurator.configure();

        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        binding.settingBt.setOnClickListener(v -> {
            Intent intent = new Intent(getBaseContext(), SettingActivity.class);
            startActivity(intent);
        });

        binding.scanCard.setOnClickListener(v -> {
            Intent intent = new Intent(getBaseContext(), ScanActivity.class);
            startActivity(intent);
        });
    }
}
