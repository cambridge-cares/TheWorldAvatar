from enum import Enum


class PlotAttrKey(Enum):
    LAND_USE_TYPE = "LandUseType"
    GROSS_PLOT_RATIO = "GrossPlotRatio"
    PLOT_AREA = "PlotArea"
    GROSS_FLOOR_AREA = "GrossFloorArea"


LAND_USE_TYPES = [
    "https://www.theworldavatar.com/kg/landplot/" + v
    for v in [
        "LandUseType_a8e423e3-c628-4b08-9f63-fcf5b244873a",
        "LandUseType_f93107ea-be85-4e40-a4ac-fd464ef26798",
        "LandUseType_618ab6ea-3d41-4841-95ef-369f000e5075",
        "LandUseType_9a4a4a58-58a4-476b-a249-aeffeb9c2445",
        "LandUseType_a7fd8f8c-4cb3-4b08-b539-a64daeadd29e",
        "LandUseType_a9c70322-46c6-41f7-9bc9-5dc425501535",
        "LandUseType_ffd32fe8-919a-4771-9bd1-2f13cabcf501",
        "LandUseType_51f02bc4-799e-4150-bf56-e55042503de7",
        "LandUseType_9f316da8-e0f8-41c8-bf0a-9efc4a9fd989",
        "LandUseType_84288400-567b-4f84-965c-cbd95e3dca26",
        "LandUseType_f45d365c-1d59-4fda-b240-afb0066f2d61",
        "LandUseType_964b8eae-7226-4e4e-af55-498f3e798fc3",
        "LandUseType_979948e3-4828-4b4a-b317-c2fb67e0409c",
        "LandUseType_cf32f28b-801c-4c1f-87d4-16b7ab51b041",
        "LandUseType_14534f34-3cbc-40c8-9e52-30018d18c486",
        "LandUseType_bc0e9391-f5de-4a2c-87be-a9435af6651f",
        "LandUseType_b6530b45-7d9b-4d8b-b9a0-a5ccc45e8b10",
        "LandUseType_b6530b45-7d9b-4d8b-b9a0-a5ccc45e8b10",
        "LandUseType_579c75a2-04e4-49f5-bc97-a9fab94a4284",
        "LandUseType_0be686be-7ba6-410c-b7e5-ed7cb06e4590",
        "LandUseType_de2f9725-4360-4b0d-b237-fb71b7f09201",
        "LandUseType_80755ccd-8068-4446-b40c-ee064b0fae56",
        "LandUseType_1c2360fa-72fb-46d4-806a-02f6bf1a1a58",
        "LandUseType_6cbda899-27e3-41e9-9ad1-9d4061a5818d",
        "LandUseType_a720a895-80e0-4180-bd3d-95d42ed1a987",
        "LandUseType_b93f8e89-26e3-400c-94ca-fa6677b7f373",
        "LandUseType_60f77832-310c-424d-9eef-bad59ffcf098",
        "LandUseType_1115b7ad-184a-4014-9b87-ebcc3a0eee41",
        "LandUseType_fa4db60a-16c6-4bbe-aff9-3209939142b6",
        "LandUseType_dd78f0f5-ec0a-4a86-95b6-72008f913e51",
        "LandUseType_7d6e19cb-0b9e-43f6-b1df-7a41001a7a34",
        "LandUseType_235abcb2-a42f-4b21-b13e-9e1ba3183477",
        "LandUseType_235abcb2-a42f-4b21-b13e-9e1ba3183477",
    ]
]
