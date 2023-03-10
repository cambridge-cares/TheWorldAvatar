import trimesh


def sample_box_gen():
    return trimesh.creation.box(bounds=[[0, 0, 0], [10, 20, 30]])


def sample_cone_gen():
    return trimesh.creation.cone(radius=5, height=20)


sample_box_bbox = [
    5, 10, 15,
    5, 0, 0,
    0, 10, 0,
    0, 0, 15
]

sample_cone_bbox = [
    0, 0, 10,
    5, 0, 0,
    0, 5, 0,
    0, 0, 10
]