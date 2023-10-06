package uk.ac.cam.cares.jps.addasset.view;

import android.Manifest;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;


import androidx.activity.result.ActivityResultCaller;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.core.content.ContextCompat;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.addasset.R;
import uk.ac.cam.cares.jps.addasset.model.DataFileDataModel;

public class DataFileItemView extends RelativeLayout {
    private final Logger LOGGER = Logger.getLogger(DataFileItemView.class);
    private Context context;
    private View view;


    public DataFileItemView(Context context) {
        super(context);
        inflate(getContext(), R.layout.view_data_sheet_item, this);
    }

    public DataFileItemView(Context context, AttributeSet attrs) {
        super(context, attrs);
        inflate(getContext(), R.layout.view_data_sheet_item, this);
    }



    public DataFileItemView(Context context, DataFileDataModel property) {
        super(context);

//        this.activityResultCaller = activityResultCaller;
        this.context = context;

        view = inflate(getContext(), R.layout.view_data_sheet_item, this);
    }

    public void setOnButtonClickListener(OnClickListener onClickListener) {
        view.findViewById(R.id.upload_bt).setOnClickListener(onClickListener);
    }

    public void setFileNameTextView(String fileName) {
        ((TextView) view.findViewById(R.id.file_name_tv)).setText(fileName + "is selected.");
        view.findViewById(R.id.file_name_tv).setVisibility(VISIBLE);
    }
}
