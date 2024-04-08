// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

using UnityEngine;

/// <summary>
/// This script calls device default browser to load the dashboard uri of the selected model.
/// </summary>
/// <remarks>
/// SelectedModel is a scritable object and set in floor plan scene.
/// </remarks>
public class LaunchUri : MonoBehaviour
{
    [SerializeField]
    private SelectedModel selectedModel;

    /// <summary>
    /// Launch a UWP slate app. In most cases, your experience can continue running while the
    /// launched app renders on top.
    /// </summary>
    public void Launch()
    {
        if (selectedModel.DashboardUri.Length == 0)
        {
            Debug.Log("No dashboard uri");
            return;
        }

        Debug.Log($"LaunchUri: Launching {selectedModel.DashboardUri}");

#if UNITY_WSA
        UnityEngine.WSA.Launcher.LaunchUri(selectedModel.DashboardUri, false);
#else
        Application.OpenURL(selectedModel.DashboardUri);
#endif
    }
}
