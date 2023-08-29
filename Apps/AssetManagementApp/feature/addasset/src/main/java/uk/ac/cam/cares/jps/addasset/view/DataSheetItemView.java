package uk.ac.cam.cares.jps.addasset.view;

import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.RelativeLayout;


import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.addasset.R;
import uk.ac.cam.cares.jps.addasset.model.AssetPropertyDataModel;

public class DataSheetItemView extends RelativeLayout {
    private final Logger LOGGER = Logger.getLogger(DataSheetItemView.class);

    public DataSheetItemView(Context context) {
        super(context);
        inflate(getContext(), R.layout.view_data_sheet_item, this);
    }

    public DataSheetItemView(Context context, AttributeSet attrs) {
        super(context, attrs);
        inflate(getContext(), R.layout.view_data_sheet_item, this);
    }

    public DataSheetItemView(Context context, AssetPropertyDataModel property) {
        super(context);
        View view = inflate(getContext(), R.layout.view_data_sheet_item, this);
        EditText editText = view.findViewById(R.id.edit_text);
        editText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2) {;}

            @Override
            public void onTextChanged(CharSequence charSequence, int i, int i1, int i2) {;}

            @Override
            public void afterTextChanged(Editable editable) {
                property.setFieldValue(editText.getText().toString());
            }
        });

        Button button = view.findViewById(R.id.upload_bt);
        button.setOnClickListener(view1 -> {
            LOGGER.info("upload document bt clicked");
        });
    }

}
