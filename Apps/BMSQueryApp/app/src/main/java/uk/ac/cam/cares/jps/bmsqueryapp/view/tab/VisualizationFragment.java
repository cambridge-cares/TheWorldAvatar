package uk.ac.cam.cares.jps.bmsqueryapp.view.tab;

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
import uk.ac.cam.cares.jps.bmsqueryapp.utils.Constants;

public class VisualizationFragment extends Fragment {
    private static final Logger LOGGER = LogManager.getLogger(VisualizationFragment.class);

    private String DTVFURI = Constants.HOST_PROD + ":1234";
    private String TESTURI = "http://www.google.com";

    public int requestedStatus;
    private FragmentVisualizationBinding binding;

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
//                binding.progressBarWrapper.setVisibility(View.GONE);
            }

            @Override
            public void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error) {
                super.onReceivedError(view, request, error);
                LOGGER.error(error);
            }
        });

        dtvfViz.getSettings().setJavaScriptEnabled(true);
        binding.dtvfViz.loadUrl(DTVFURI);
        LOGGER.info("view created");

        dtvfViz.setVisibility(View.VISIBLE);
    }


}
