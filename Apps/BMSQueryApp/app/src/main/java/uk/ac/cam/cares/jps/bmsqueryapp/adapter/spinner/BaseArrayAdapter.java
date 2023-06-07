package uk.ac.cam.cares.jps.bmsqueryapp.adapter.spinner;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import uk.ac.cam.cares.jps.bmsqueryapp.R;

public class BaseArrayAdapter<T> extends ArrayAdapter<T> {
    T placeholder;

    public BaseArrayAdapter(Context context, T placeholder) {
        super(context, R.layout.spinner_item);
        setDropDownViewResource(R.layout.spinner_dropdown_item);

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
        }
        return view;
    }

    @Override
    public View getDropDownView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        TextView view = (TextView) super.getDropDownView(position, convertView, parent);
        if (position == 0) {
            view.setTextColor(Color.GRAY);
        }
        return view;
    }

    @Override
    public void clear() {
        super.clear();
        add(placeholder);
    }
}
