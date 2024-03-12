# CARES AR Lab

## Introduction

This project is to showcase the CARES lab and apply knowledge graph in AR. It is built with [Unity](https://unity.com/) and [MRTK3 library](https://learn.microsoft.com/en-us/windows/mixed-reality/mrtk-unity/mrtk3-overview/) for [Hololens2](https://www.microsoft.com/en-us/hololens) devices. The project consists of 3 scenes, home scene, floor plan scene and model scene.

### Home Scene

This is the first scene user will see when open up the application. User can navigate to the home scene and the floor plan scene with the buttons.

### Floor Plan Scene

In the floor plan scene, user can click on each room to check the real time room temperature and humidity. The user can also click on the models in the open lab area to view the reasize models and their real time status in the model scene. There are three types of model available, fumehood, PIPS robot and chemical cabinet. Each type is labeled with different colors and the color representation can be found from the legend.

### Model Scene

In this scene, the real size model selected in the floor plan scene will be shown. Model can be drag and placed in the world and applied different operations on them based on the type.

#### Fumehood

User can check the fumehood's status on the dashboard and the model's sash position reflects the real time sash opening in the lab.

#### PIPS Robot

Similar to the fumehood, user can check the status of the canopy hood on top of the robot and interact with the model. User can also change the setpoint of the canopy hood with the pop up keyboard. The state change can be viewed on the dashboard.

#### Chemical Cabinet

User can experience the process of unlocking the cabinet, opening the door and check the chemical bottle's status. 

## Project Setup

### Prerequisites

- [Unity Hub](https://unity.com/download) with Unity 2021.3 LTS editor installed
    - Tick platforms `Universal Windows Platform Build Support` and `Windows Build Support (IL2CPP)` when installing the editor
    - Unity Editor configuration steps can be found at [here](https://learn.microsoft.com/en-us/windows/mixed-reality/develop/unity/choosing-unity-version)
- [Visual Studio](https://visualstudio.microsoft.com/vs/older-downloads/) with the [required workloads](https://learn.microsoft.com/en-us/windows/mixed-reality/develop/install-the-tools?tabs=unity)
- Hololens2 device or Hololens2 Emulator
- Windows 10 or 11
- [Microsoft Mixed Reality Feature Tool](https://learn.microsoft.com/en-us/windows/mixed-reality/develop/unity/welcome-to-mr-feature-tool#2-selecting-your-unity-project)

A detailed installation list (without Microsoft Mixed Reality Feature Tool) can be found at [here](https://learn.microsoft.com/en-us/windows/mixed-reality/develop/install-the-tools?tabs=unity).

### Project Setup

1. Clone this project to local

2. Copy files from dropbox [CARESLab-AR folder](https://www.dropbox.com/scl/fo/9z7lwem3gu7g4217s8sh0/h?rlkey=26ldly70xd2p8y069f3d6s4nq&dl=0) 
    - `Assets\Model\*.blend` model to `\CARESLab\Assets\Model`
    - `Assets\Prefabs\HandCoach\HandRig` to `\CARESLab\Assets\Prefabs\HandCoach\HandRig`

        These files are for HandCoach feature in this project. A detailed description of this feature can be found at [HandCoach](#HandCoach) section.

3. Install MRTK3 features with Microsoft Mixed Reality Feature Tool

    Acquire the following features from Microsoft Mixed Reality Feature Tool:

    - MRTK3
        - MRTK Graphics Tools (0.6.2)
        - MRTK Core Definitions (3.0.0)
        - MRTK Extended Assets (3.0.0)
        - MRTK Input (3.0.0)
        - MRTK Spatial Manipulation (3.0.0)
        - MRTK Standard Assets (3.0.0)
        - MRTK Tools (3.0.0)
        - MRTK UX Components (3.1.0)
        - MRTK UX Core Scripts (3.1.0)
    - Platform SUpport
        - Mixed Reality OpenXR Plugin (1.9.0)
        - Mixed Reality Scene Understanding (0.6.0)
    - Spatial Audio 
        - Microsoft Spatializer (2.0.47)
    
    Ideally, the features should be able to be restored with the 'Restore Features' button in the tool.  However, from experiments, only part of the features is recovered, so it is recommended to acquire these features.

    A complete step by step guide on Microsoft Mixed Reality Feature Tool can be found at [here](https://learn.microsoft.com/en-us/windows/mixed-reality/develop/unity/welcome-to-mr-feature-tool).

4. Open Unity hub, add project from disk and open the project. Unity will download and extract the dependency packages and compile codes.

5. Configuration in Unity

    a. Go to `Edit > Project Settings > MRTK3` to assign MRTK default profile.

    b. In Project Settings `XR Plug-in Management`, tick 

    - `OpenXR > Windows Mixed Reality feature group` for `Windows, Mac, Linux settings` 

    - `OpenXR > Microsoft HoloLens feature group` for `Universal Windows Platform settings`

    c. In `XR Plug-in Management > OpenXR` add `Microsoft Hand Interaction Profile` for both `Windows, Mac, Linux settings` and `Universal Windows Platform settings`

    d. In `XR Plug-in Management > Project Validation`, click `Fix All` for both `Windows, Mac, Linux settings` and `Universal Windows Platform settings`.

    e. Check `Player > Resolution and Presentation > Run In Background` is ticked.

A complete guide of setting up **new** MRTK3 project can be found at [here](https://learn.microsoft.com/en-us/windows/mixed-reality/mrtk-unity/mrtk3-overview/getting-started/setting-up/setup-new-project).

6. If TMP Importer is prompt, choose to import TMP Essentials.

7. Replace `<STACK-ADDR>` and `<ONTOLOGY-ADDR>` in [Assets/StreamingAssets/endpoints.properties](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/AR/CARESLab/Assets/StreamingAssets/endpoints.properties).

### Run in Unity

Run in Unity is the most easy and convenient way to test during development. Check [here](https://learn.microsoft.com/en-us/windows/mixed-reality/mrtk-unity/mrtk3-input/packages/input/input-simulation#how-to-use-mrtk3-input-simulation-mrtk3-input-simulator-default-controls) for keys to control.

Other resources:
- [Debug C# code in Unity](https://docs.unity3d.com/Manual/ManagedCodeDebugging.html)

### Test and Deploy on Device or Emulator

Test and deploy on device or emulator consists of two steps:
1. Build project in Unity
2. Build and deploy with Visual Studio

A complete guide on deployment can be found [here](https://learn.microsoft.com/en-us/windows/mixed-reality/develop/unity/build-and-deploy-to-hololens).

If errors occurred when deploying with Master or Release mode in Visual Studio, developer can choose to deploy with Debug mode, which gives a faster compile speed but little optimizations.

Other resources:
- [Using the HoloLens Emulator](https://learn.microsoft.com/en-us/windows/mixed-reality/develop/advanced-concepts/using-the-hololens-emulator)
    
    If deploying to the emulator, the Visual Studio should `run as administrator`. During the deployment process, the Visual Studio will boot the emulator.

- [Debug Unity IL2CPP UWP build in Hololens](https://learn.microsoft.com/en-us/windows/mixed-reality/develop/unity/managed-debugging-with-unity-il2cpp)

    This enables the use of breakpoints when running the program in Hololens.


### Troubleshoot

- Unity Package Manager error

    Error message: Failed to resolve packages: Tarball package [com.microsoft.mixedreality.openxr] cannot be found at path <PACKAGE-PATH>. No packages loaded.

    Occurred when MRTK3 features not added properly. Please check whether all the packages stated in [Project Setup](#project-setup) have been added.

- Prefab file missing error

    Error message: Problem detected while importing the Prefab file: 'Assets/Prefabs/<PREFAB-NAME>'. The file might be corrupt or have missing nested Prefabs.

    Occurred when `*.blend` model files are not added before opening the project in Unity. This will also remove the `*.asset` files in `Assets/Model/`. Please add the `*.blend` model files **and** pull the project again.

## MRTK2 Features
This project is built with MRTK3 library which is generally available in October 2023. Therefore, some of MRTK2 features haven't been fully migrated to MRTK3. This section lists the MRTK2 features which have been modified to integrate with the MRTK3 library.

### HandCoach

[HandCoach](https://learn.microsoft.com/en-us/windows/mixed-reality/mrtk-unity/mrtk2/features/ux-building-blocks/hand-coach?view=mrtkunity-2022-05) is implemented as a "teaching" component that helps guide the user what operations to perform. 

The feature is included in MRTK2 foundation package. To acquire the original files and set up **from scratch**, one can follow the steps below. These steps have already been completed and saved to this repo. Developer doesn't need to redo them. The only operation that is needed is to import the `*.fbx` hand model files as described in [Project Setup](#project-setup) section.

1. Create a new 3D project with Unity Hub
2. Add `Mixed Reality Toolkit > Mixed Reality Toolkit Foundation (2.8.3)` to the 3D project with Microsoft Mixed Reality Feature Tool.
3. Search for `HandCoach` in the MRTK2 3D project, and copy the following folders to your MRTK3 Unity project
    
    a. Packages/com.microsoft.mixedreality.toolkit.foundation/SDK/Editor/Inspectors/UX/HandCoach

    b. Packages/com.microsoft.mixedreality.toolkit.foundation/SDK/Features/UX/Animations/HandCoach

    c. Packages/com.microsoft.mixedreality.toolkit.foundation/SDK/Features/UX/Materials/HandCoach

    d. Packages/com.microsoft.mixedreality.toolkit.foundation/SDK/Features/UX/Meshes/HandCoach

    e. Packages/com.microsoft.mixedreality.toolkit.foundation/SDK/Features/UX/Prefabs/HandCoach

    f. Packages/com.microsoft.mixedreality.toolkit.foundation/SDK/Features/UX/Scripts/HandCoach
4. Manually fix the links between the HandCoach components.
5. Modify `private bool IsHandTracked()` `Scripts/HandCoach/HandInteractionHint.cs` using MRTK3 `HandsAggregatorSubsystem`