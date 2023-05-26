package uk.ac.cam.cares.jps.bmsqueryapp.utils;

import okhttp3.HttpUrl;

public class Constants {
    public static final String HOST_DEV = "192.168.1.38";
    public static final String HOST_LOCALHOST = "10.0.2.2";

    public static final String HOST_LAB_WIFI = "192.168.51.103";
    public static final String HOST_OFFICE = "10.25.188.58";
    public static final String HOST_PROD = "137.132.22.165";
    public static final String HOST = HOST_PROD;

    public static String[] statusArrayTemp = {"Graph", "Edit"};

    public static HttpUrl.Builder constructUrlBuilder(String path) {
        return constructUrlBuilder(HOST, 3838, path);
    }

    public static HttpUrl.Builder constructUrlBuilder(String host, int port, String path) {
        HttpUrl.Builder builder = new HttpUrl.Builder().scheme("http").host(host).port(port);
        return builder.addPathSegments(path);
    }


}
