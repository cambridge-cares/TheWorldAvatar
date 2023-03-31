package uk.ac.cam.cares.jps.bmsqueryapp.adapter.spinner;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class BaseArrayAdapter<T> extends ArrayAdapter<T> {
    T placeholder;
    public BaseArrayAdapter(Context context) {
        super(context, android.R.layout.simple_spinner_item);
    }
    public BaseArrayAdapter(Context context, T placeholder) {
        super(context, android.R.layout.simple_spinner_item);
        this.add(placeholder);
        this.placeholder = placeholder;
    }

    @Override
    public boolean isEnabled(int position) {
        return position != 0;
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        TextView view = (TextView) super.getView(position, convertView, parent);
        if (position == 0) {
            view.setTextColor(Color.GRAY);
        } else {
            view.setTextColor(Color.BLACK);
        }
        return view;
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

    @Override
    public void clear() {
        super.clear();
        this.add(placeholder);
    }
}
