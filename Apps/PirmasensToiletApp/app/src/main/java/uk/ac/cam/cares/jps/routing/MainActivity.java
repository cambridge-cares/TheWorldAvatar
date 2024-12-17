package uk.ac.cam.cares.jps.routing;

import androidx.appcompat.app.AppCompatActivity;
import android.os.Bundle;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.routing.databinding.ActivityMainBinding;

@AndroidEntryPoint
public class MainActivity extends AppCompatActivity {
    private static ActivityMainBinding binding;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
    }
}