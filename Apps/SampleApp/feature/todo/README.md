# Todo Module

A module demonstrates in module navigation with action, data binding and the complete workflow of retrieving data from internet and displaying in the app.

## 1. Workflow
The workflow of data transfer has been discussed in [SampleApp/README.md](https://github.com/cambridge-cares/TheWorldAvatar/blob/1786-android-documentation/Apps/SampleApp/README.md#22-data-transfer). Please refer to it for more details.

## 2. In Module Navigation
In module navigation has been discussed in [SampleApp/README.md](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Apps/SampleApp/README.md#213-action). Please refer to it for more details.

## 3. Data Binding

The [Data Binding Library](https://developer.android.com/topic/libraries/data-binding) is a support library that allows you to bind UI components in your layouts to data sources in your app using a declarative format rather than programmatically.

Without data binding, changing a view's value is usually done in the following way:
```java
TextView textView = findViewById(R.id.sample_text);
textView.setText(viewModel.getUserName());
```

With data binding, the source of the view data is defined in the layout file. Changes in the source value will be directly reflected on the view removing the need to call java function explicitly.
```xml
<TextView android:text="@{viewmodel.userName}" />
```

Data binding is suitable for setting simple data to view. If the data required processing before setting to the view, developer may consider to set it programmatically in Java code instead of through data binding to increase readability.

### 3.1 Setup Build Environment
Enable the `dataBinding` build option in the `build.gradle` file. 

```groovy
android {
    ...
    buildFeatures {
        dataBinding true
    }
}
```

This is included in the shared `ui.gradle` file and added in individual modules with 
```groovy
apply from: "${project.rootDir}/ui.gradle"
```

### 3.2 Prepare Layout File
Wrap non-binding layout tag with `<layout>` and indicate the data with `<data>`
```xml
<!--todo_fragment.xml-->
<?xml version="1.0" encoding="utf-8"?>
<layout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:app="http://schemas.android.com/apk/res-auto">

    <data>
        <variable
            name="todoViewModel"
            type="uk.ac.cam.cares.jps.todo.TodoViewModel" />
    </data>
    <androidx.constraintlayout.widget.ConstraintLayout>
        ...
    </androidx.constraintlayout.widget.ConstraintLayout>
</layout>
```

Bind data to view:
```xml
<TextView
    android:layout_width="wrap_content"
    android:layout_height="wrap_content"
    android:text='@{"Id: " + todoViewModel.todo.getValue().id}'
    android:textAppearance="@style/TextAppearance.Material3.BodyLarge" />
```

### 3.3 Set Data to Layout
Set the variable in Java code to the layout
```java
// TodoFragment.java

// inflate the layout
binding = TodoFragmentBinding.inflate(inflater);

// create the instance of the data variable
todoViewModel = new ViewModelProvider(this).get(TodoViewModel.class);

// set the data to layout
binding.setTodoViewModel(todoViewModel);
```