import trimesh


def sample_box_gen():
    return trimesh.creation.box(bounds=[[-4, -6, -10], [6, 4, 10]])


def sample_cone_gen():
    return trimesh.creation.cone(radius=5, height=20)


sample_box_bbox = [
    1, -1, 0,
    5, 0, 0,
    0, 5, 0,
    0, 0, 10
]

sample_cone_bbox = [
    0, 0, 10,
    5, 0, 0,
    0, 5, 0,
    0, 0, 10
]

combined_bbox = [
    0.5, -0.5, 5,
    5.5, 0, 0,
    0, 5.5, 0,
    0, 0, 15
]