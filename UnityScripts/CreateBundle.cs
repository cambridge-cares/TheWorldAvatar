using UnityEngine;
using System.Collections;
using UnityEditor;
using System;

public class CreateBundle : MonoBehaviour
{
    [MenuItem("CreateBundle/Build Asset Bundles")]
    static void BuildABs()
    {
        BuildPipeline.BuildAssetBundles("Assets/ABs", BuildAssetBundleOptions.None, BuildTarget.StandaloneWindows64);
    }
}
