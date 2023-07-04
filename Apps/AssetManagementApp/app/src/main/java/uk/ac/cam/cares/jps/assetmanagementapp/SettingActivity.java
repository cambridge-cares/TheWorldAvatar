package uk.ac.cam.cares.jps.assetmanagementapp;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

import org.apache.log4j.BasicConfigurator;

import uk.ac.cam.cares.jps.assetmanagementapp.databinding.ActivitySettingBinding;

public class SettingActivity extends AppCompatActivity {
    private ActivitySettingBinding binding;
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        BasicConfigurator.configure();

        binding = ActivitySettingBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());


    }
}
