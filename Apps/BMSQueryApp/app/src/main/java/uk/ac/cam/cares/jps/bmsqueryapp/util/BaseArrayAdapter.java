package uk.ac.cam.cares.jps.bmsqueryapp.util;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class BaseArrayAdapter<T> extends ArrayAdapter<T> {
    public BaseArrayAdapter(Context context) {
        super(context, android.R.layout.simple_spinner_item);
    }

    @Override
    public boolean isEnabled(int position) {
        return position != 0;
    }

    @Override
    public View getDropDownView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        TextView view = (TextView) super.getDropDownView(position, convertView, parent);
        if (position == 0) {
            view.setTextColor(Color.GRAY);
        } else {
            view.setTextColor(Color.BLACK);
        }
        return view;
    }
}
