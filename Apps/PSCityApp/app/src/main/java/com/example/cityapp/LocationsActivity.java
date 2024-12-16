package com.example.cityapp;

import androidx.appcompat.app.AppCompatActivity;

import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;

public class LocationsActivity extends AppCompatActivity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_locations);

        Button parkingButton = findViewById(R.id.parking);
        Button toiletButton = findViewById(R.id.toilet);
        Button attractionButton = findViewById(R.id.attraction);
        Button recyclingButton = findViewById(R.id.recycling);
        Button waterButton = findViewById(R.id.water);
        Button shoppingButton = findViewById(R.id.shopping);
        Button entertainmentButton = findViewById(R.id.entertainment);

        View.OnClickListener onClickListener = view -> Toast.makeText(view.getContext(), "This function has not been implemented yet!", Toast.LENGTH_SHORT).show();
        parkingButton.setOnClickListener(onClickListener);
        toiletButton.setOnClickListener(onClickListener);
        attractionButton.setOnClickListener(onClickListener);
        recyclingButton.setOnClickListener(onClickListener);
        waterButton.setOnClickListener(onClickListener);
        shoppingButton.setOnClickListener(onClickListener);
        entertainmentButton.setOnClickListener(onClickListener);
    }
}