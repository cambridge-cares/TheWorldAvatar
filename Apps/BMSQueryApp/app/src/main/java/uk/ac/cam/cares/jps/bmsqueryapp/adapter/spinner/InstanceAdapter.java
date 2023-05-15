package uk.ac.cam.cares.jps.bmsqueryapp.adapter.spinner;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import uk.ac.cam.cares.jps.bmsqueryapp.data.buildings.Instance;

public class InstanceAdapter extends BaseArrayAdapter<Instance>{
    public InstanceAdapter(Context context, String placeholderStr) {
        super(context);

        Instance placeholder = new Instance("Please select...");
        placeholder.setLabel(placeholderStr);
        this.placeholder = placeholder;

        add(this.placeholder);
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        TextView tv = (TextView) super.getView(position, convertView, parent);
        tv.setText(getItem(position).getLabel());
        return tv;
    }

    @Override
    public View getDropDownView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        TextView tv = (TextView) super.getDropDownView(position, convertView, parent);
        tv.setText(getItem(position).getLabel());
        return tv;
    }

}
