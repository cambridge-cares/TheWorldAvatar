package uk.ac.cam.cares.jps.timeline;

import androidx.activity.EdgeToEdge;
import androidx.appcompat.app.AppCompatActivity;
import android.os.Bundle;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.timeline.databinding.ActivityMainBinding;

@AndroidEntryPoint
public class MainActivity extends AppCompatActivity {
    private static ActivityMainBinding binding;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        EdgeToEdge.enable(this);
        super.onCreate(savedInstanceState);

        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
    }
}