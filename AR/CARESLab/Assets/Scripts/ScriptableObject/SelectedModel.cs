using UnityEngine;

/// <summary>
/// Scriptable object to share information about the selected model across multiple scenes
/// </summary>
[CreateAssetMenu(menuName = "SelectedModel")]
public class SelectedModel : ScriptableObject
{
    /// <summary>
    /// Model iri
    /// </summary>
    public string Iri;

    /// <summary>
    /// Model name
    /// </summary>
    public string ModelName;

    /// <summary>
    /// Dashboard Uri
    /// </summary>
    public string DashboardUri;

    /// <summary>
    /// Document Uri
    /// </summary>
    public string PdfUri;

    /// <summary>
    /// The current transformation of the selected model
    /// </summary>
    public Transform SelectedModelTransform;

    /// <summary>
    /// The current position of the selected model
    /// </summary>
    public Vector3 Position;

    /// <summary>
    /// The current rotation of the selected model
    /// </summary>
    public Quaternion Rotation;

    /// <summary>
    /// The current scale of the selected model
    /// </summary>
    public Vector3 Scale;

    /// <summary>
    /// The model of the selected object
    /// </summary>
    public GameObject Model;
}
