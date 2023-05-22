package uk.ac.cam.cares.jps.bmsqueryapp.view.tab;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.JavascriptInterface;
import android.webkit.WebResourceError;
import android.webkit.WebResourceRequest;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.ImageButton;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.bmsqueryapp.databinding.FragmentVisualizationBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.Constants;

public class VisualizationFragment extends Fragment {

    /**
     * A data class used to inject application data to the visualisation webpage
     */
    static class JsObject {
        public String equipmentIri;
        public JsObject(String equipmentIri) {
            this.equipmentIri = equipmentIri;
        }

        /**
         * This function is called in the visualisation webpage to get the user selected equipmentIri
         * @return equipmentIri
         */
        @JavascriptInterface
        public String getEquipmentIri() {
            return equipmentIri;
        }
    }


    private static final Logger LOGGER = LogManager.getLogger(VisualizationFragment.class);

    public int requestedStatus;
    private FragmentVisualizationBinding binding;

    private String equipmentIri;
    private WebViewClient webViewClient;

    public VisualizationFragment(String equipmentIri, WebViewClient webViewClient) {
        super();
        this.equipmentIri = equipmentIri;
        this.webViewClient = webViewClient;
    }

    public void setPosition(int position) {
        requestedStatus = position;
    }

    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = FragmentVisualizationBinding.inflate(inflater, container, false);
        BasicConfigurator.configure();
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.dtvfViz.setWebViewClient(webViewClient);
        loadDTVF();
    }

    public void loadDTVF() {
        WebView.setWebContentsDebuggingEnabled(true);

        WebView dtvfViz = binding.dtvfViz;

        JsObject jsObject = new JsObject(equipmentIri);

        dtvfViz.getSettings().setJavaScriptEnabled(true);
        binding.dtvfViz.addJavascriptInterface(jsObject, "jsObject");
        binding.dtvfViz.loadUrl("file:///android_asset/visualisation/index.html");
        LOGGER.info("view created");

        dtvfViz.setVisibility(View.VISIBLE);

        WebSettings webSettings = binding.dtvfViz.getSettings();
        if (webSettings.getTextZoom() > 150) {
            webSettings.setTextZoom(150);
        }
    }

    public void refreshDTVF() {
        binding.dtvfViz.loadUrl( "javascript:window.location.reload( true )" );
    }


}
