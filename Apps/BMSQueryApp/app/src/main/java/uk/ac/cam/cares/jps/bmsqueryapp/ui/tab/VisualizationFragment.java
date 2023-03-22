package uk.ac.cam.cares.jps.bmsqueryapp.ui.tab;

import android.app.ProgressDialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebResourceError;
import android.webkit.WebResourceRequest;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.ProgressBar;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.bmsqueryapp.databinding.FragmentVisualizationBinding;

public class VisualizationFragment extends Fragment {
    private static final Logger LOGGER = LogManager.getLogger(VisualizationFragment.class);

    private String DTVFURI = "http://192.168.1.6:80";
    private String TESTURI = "http://www.google.com";

    public int requestedStatus;
    private FragmentVisualizationBinding binding;
    private ProgressBar progressBar;

    public VisualizationFragment() {
        super();
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

    }

    public void loadDTVF() {
        WebView dtvfViz = binding.dtvfViz;
        dtvfViz.setWebViewClient(new WebViewClient() {

            @Override
            public void onPageFinished(WebView view, String url) {
                super.onPageFinished(view, url);
                LOGGER.info("page finished loading");
            }

            @Override
            public void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error) {
                super.onReceivedError(view, request, error);
                LOGGER.error(error);
            }
        });

        dtvfViz.setWebViewClient(new WebViewClient());
        dtvfViz.getSettings().setJavaScriptEnabled(true);
        binding.dtvfViz.loadUrl(DTVFURI);
        LOGGER.info("view created");

        binding.progressBarWrapper.setVisibility(View.GONE);
        dtvfViz.setVisibility(View.VISIBLE);
    }


}
