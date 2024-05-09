plugins {
    id("com.android.application")
}

android {
    namespace = "uk.ac.cam.cares.sensorlogger"
    compileSdk = 34

    defaultConfig {
        applicationId = "uk.ac.cam.cares.sensorlogger"
        minSdk = 24
        targetSdk = 34
        versionCode = 1
        versionName = "1.0"

        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            proguardFiles(getDefaultProguardFile("proguard-android-optimize.txt"), "proguard-rules.pro")
        }
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_1_8
        targetCompatibility = JavaVersion.VERSION_1_8
    }
}

dependencies {

    implementation("androidx.appcompat:appcompat:1.6.1")
    implementation("com.google.android.material:material:1.11.0")
    implementation("androidx.constraintlayout:constraintlayout:2.1.4")
    implementation("com.mapbox.maps:android:10.15.0")
    implementation("com.google.android.gms:play-services-location:21.1.0")
    implementation ("com.google.android.material:material:1.11.0")
    implementation("com.android.volley:volley:1.2.1")
    implementation ("androidx.lifecycle:lifecycle-viewmodel:2.7.0")
    implementation ("androidx.lifecycle:lifecycle-livedata:2.7.0")
    testImplementation("junit:junit:4.13.2")
    androidTestImplementation("androidx.test.ext:junit:1.1.5")
    androidTestImplementation("androidx.test.espresso:espresso-core:3.5.1")
}