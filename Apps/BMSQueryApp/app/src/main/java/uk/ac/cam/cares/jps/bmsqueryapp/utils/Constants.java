package uk.ac.cam.cares.jps.bmsqueryapp.utils;

import okhttp3.HttpUrl;

public class Constants {
    public static final String HOST = "192.168.1.26";
    private static final String HOST_TEST = "10.0.2.2";
    public static String[] statusArrayTemp = {"Graph", "Edit"};

    public static HttpUrl.Builder constructUrlBuilder(String path) {
        HttpUrl.Builder builder = new HttpUrl.Builder().scheme("http").host(HOST_TEST).port(3838);
        return builder.addPathSegments(path);
    }


}
