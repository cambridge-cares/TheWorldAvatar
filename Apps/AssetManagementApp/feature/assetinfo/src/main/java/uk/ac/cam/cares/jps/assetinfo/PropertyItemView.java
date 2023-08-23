package uk.ac.cam.cares.jps.assetinfo;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;


public class PropertyItemView extends ConstraintLayout {
    TextView labelTv;
    TextView valueTv;

    public PropertyItemView(Context context) {
        super(context);
        View view = inflate(getContext(), R.layout.property_item_view, this);
        labelTv = view.findViewById(R.id.label);
        valueTv = view.findViewById(R.id.content);
    }

    public PropertyItemView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        View view = inflate(getContext(), R.layout.property_item_view, this);
        labelTv = view.findViewById(R.id.label);
        valueTv = view.findViewById(R.id.content);
    }

    public void initView(String label, String value) {
        labelTv.setText(label);
        valueTv.setText(value);
    }
}
