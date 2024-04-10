package com.example.cityapp;

import androidx.appcompat.app.AppCompatActivity;

import android.content.Intent;
import android.os.Bundle;
import android.widget.Button;

public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        Button locations_btn = findViewById(R.id.amenity);

        locations_btn.setOnClickListener(view -> {
            Intent intent = new Intent(MainActivity.this, LocationsActivity.class);
            startActivity(intent);
        });
    }

}

