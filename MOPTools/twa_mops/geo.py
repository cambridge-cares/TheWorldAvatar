# This file contains utility classes for geometric calculations
from typing import List, Optional
from itertools import combinations
import numpy as np
from scipy.spatial.transform import Rotation as R
from pydantic import BaseModel


class RotationMatrix():
    matrix: np.ndarray

    def __init__(self, matrix: np.ndarray):
        self.matrix = matrix

    def apply(self, vector: np.ndarray):
        return self.matrix @ vector

    def as_matrix(self):
        return self.matrix

    @classmethod
    def identity(cls):
        return cls(matrix=np.eye(3))


class Point(BaseModel):
    x: float
    y: float
    z: float
    label: Optional[str] = None

    def __repr__(self):
        return f"Point({self.x}, {self.y}, {self.z})"

    @property
    def as_array(self):
        return np.array([self.x, self.y, self.z])

    def get_distance_to(self, other: 'Point'):
        return np.linalg.norm(self.as_array - other.as_array)

    def get_mid_point_to(self, other: 'Point'):
        return Point(x=(self.x+other.x)/2, y=(self.y+other.y)/2, z=(self.z+other.z)/2)

    def get_translation_vector_to(self, other: 'Point'):
        return Vector(x=other.x - self.x, y=other.y - self.y, z=other.z - self.z)

    def overlaps_with(self, other: 'Point'):
        return np.allclose(self.as_array, other.as_array)

    def farthest_point(self, points: List['Point']):
        max_dist = 0
        max_point = None
        for pt in points:
            dist = self.get_distance_to(pt)
            if dist > max_dist:
                max_dist = dist
                max_point = pt
        return max_point

    @classmethod
    def from_array(cls, arr, label=None):
        return cls(x=arr[0], y=arr[1], z=arr[2], label=label)

    @classmethod
    def rotate(cls, point: 'Point', rotation: R):
        _ = rotation.apply(point.as_array)
        return cls(x=_[0], y=_[1], z=_[2], label=point.label)

    @classmethod
    def scale(cls, point: 'Point', factor: float):
        return cls(x=point.x * factor, y=point.y * factor, z=point.z * factor, label=point.label)

    @classmethod
    def translate(cls, point: 'Point', vector: 'Vector'):
        return cls(x=point.x + vector.x, y=point.y + vector.y, z=point.z + vector.z, label=point.label)

    @classmethod
    def average(cls, points: List['Point']):
        return cls(x=np.mean([pt.x for pt in points]), y=np.mean([pt.y for pt in points]), z=np.mean([pt.z for pt in points]))

    @classmethod
    def farthest_pair(cls, points: List['Point']):
        max_dist = 0
        max_points = (None, None)
        for pt1, pt2 in combinations(points, 2):
            dist = pt1.get_distance_to(pt2)
            if dist > max_dist:
                max_dist = dist
                max_points = (pt1, pt2)
        return max_points


class Vector(BaseModel):
    x: float
    y: float
    z: float

    def __repr__(self):
        return f"Vector({self.x}, {self.y}, {self.z})"

    @property
    def as_array(self):
        return np.array([self.x, self.y, self.z])

    @property
    def magnitude(self):
        return np.linalg.norm(self.as_array)

    def get_unit_vector(self):
        mag = self.magnitude
        return Vector(x = self.x / mag, y = self.y / mag, z = self.z / mag)

    def get_flip_vector(self):
        return Vector(x = -self.x, y = -self.y, z = -self.z)

    def get_dot_product(self, other: 'Vector'):
        return np.dot(self.as_array, other.as_array)

    def get_cross_product(self, other: 'Vector'):
        return np.cross(self.as_array, other.as_array)

    def get_rad_angle_to(self, other: 'Vector'):
        return np.arccos(np.clip(self.get_dot_product(other) / (self.magnitude * other.magnitude), -1.0, 1.0))

    def get_deg_angle_to(self, other: 'Vector'):
        return np.degrees(self.get_rad_angle_to(other))

    def get_rotation_matrix_to_parallel(
        self,
        other: 'Vector',
        flip_if_180: bool = False,
        base_axis_if_180: Optional['Vector'] = None
    ) -> RotationMatrix:
        # TODO not elegant but works for now
        v1 = self.get_unit_vector()
        v2 = other.get_unit_vector()
        axis = self.get_cross_product(v2)
        if np.allclose(np.linalg.norm(axis), 0): # NOTE this is to avoid numerical errors
            if v1.get_dot_product(v2) < 0:
                if flip_if_180:
                    if base_axis_if_180 is not None:
                        return RotationMatrix(matrix=R.from_rotvec(np.pi * base_axis_if_180.get_unit_vector().as_array).as_matrix())
                    else:
                        # TODO maybe not neccessary to flip all three directions... revisit this...
                        return RotationMatrix(matrix=R.identity().as_matrix() * -1)
                else:
                    return RotationMatrix(matrix=R.identity().as_matrix())
            else:
                return RotationMatrix(matrix=R.identity().as_matrix())
        axis = axis / np.linalg.norm(axis)
        angle = np.arccos(np.clip(v1.get_dot_product(v2), -1.0, 1.0))
        rotation = R.from_rotvec(angle * axis)
        return RotationMatrix(matrix=rotation.as_matrix())

    @classmethod
    def from_array(cls, arr):
        return cls(x=arr[0], y=arr[1], z=arr[2])

    @classmethod
    def from_two_points(cls, start: Point, end: Point):
        return cls(x = end.x - start.x, y = end.y - start.y, z = end.z - start.z)

    @classmethod
    def rotate_to_parallel(cls, vector: 'Vector', other: 'Vector'):
        rotation_matrix = vector.get_rotation_matrix_to_parallel(other)
        r_v = rotation_matrix.apply(vector.as_array)
        return cls(x=r_v[0], y=r_v[1], z=r_v[2])


class Line(BaseModel):
    point: Point
    direction: Vector

    def __repr__(self):
        return f"Line(Point={self.point}, Direction={self.direction})"

    @classmethod
    def from_two_points(cls, start: Point, end: Point):
        return cls(point=start, direction=Vector.from_two_points(start, end))

    def is_point_on_line(self, other: Point, threshold: float = 1e-6):
        cross_product = self.direction.get_cross_product(Vector.from_two_points(self.point, other))
        return np.allclose(cross_product, [0, 0, 0])

    def normal_vector_from_point_to_line(self, other: Point):
        if self.is_point_on_line(other):
            # this means the point is on the line
            # so we return the normal vector of the direction of the line
            # and the normal vector needs to pass through the other point
            # as this is a line, we can choose any arbitrary vector that is not parallel to the line
            v = self.direction.get_unit_vector().as_array
            if not np.allclose(v, [1, 0, 0]):
                # if v is not parallel to the x-axis
                arbitrary_vector = np.array([1, 0, 0])
            else:  # if v is parallel to the x-axis, choose a different arbitrary vector
                arbitrary_vector = np.array([0, 1, 0])
            # cross product for normal vector
            normal_vector = np.cross(v, arbitrary_vector)
            normal_vector = normal_vector / np.linalg.norm(normal_vector)
            return Vector(x=normal_vector[0] + other.x, y=normal_vector[1] + other.y, z=normal_vector[2] + other.z)
        else:
            v = Vector.from_two_points(other, self.point)
            v_proj = (np.dot(v.as_array, self.direction.as_array) / np.dot(self.direction.as_array, self.direction.as_array)) * self.direction.as_array
            return Vector(x=v.x - v_proj[0], y=v.y - v_proj[1], z=v.z - v_proj[2])


class Plane(BaseModel):
    point: Point
    normal: Vector

    def __repr__(self):
        return f"Plane(Point={self.point}, Normal={self.normal})"

    @classmethod
    def from_point_and_normal(cls, point: Point, normal: Vector):
        return cls(point = point, normal = normal.get_unit_vector())

    @classmethod
    def from_two_vectors(cls, vector1: Vector, vector2: Vector, point_on_plane: Point = Point(x=0, y=0, z=0)):
        normal_vector = vector1.get_cross_product(vector2)
        norm = normal_vector / np.linalg.norm(normal_vector)
        return cls(point=point_on_plane, normal=Vector.from_array(norm))

    @classmethod
    def from_three_points(cls, pt1: Point, pt2: Point, pt3: Point):
        v1 = Vector(pt2.x - pt1.x, pt2.y - pt1.y, pt2.z - pt1.z)
        v2 = Vector(pt3.x - pt1.x, pt3.y - pt1.y, pt3.z - pt1.z)
        return cls.from_two_vectors(v1, v2, pt1)

    @classmethod
    def fit_from_points(cls, points: List[Point]):
        n = len(points)
        if n < 3:
            raise ValueError(f'At least 3 points are required to fit a plane. Provided points: {points}')
        A = np.array([[pt.x, pt.y, pt.z] for pt in points])
        centroid = np.mean(A, axis=0)
        A -= centroid
        _, _, V = np.linalg.svd(A)
        normal = V[-1]
        return cls(point=Point.from_array(centroid), normal=Vector.from_array(normal))

    def normal_vector_from_point_to_plane(self, other: Point):
        w = Vector.from_two_points(start=other, end=self.point)
        proj = w.get_dot_product(self.normal) / self.normal.magnitude
        if proj == 0:
            # this means the point is on the plane
            # so we return the normal vector
            return Vector.from_array(self.normal.as_array)
        return Vector(x=proj * self.normal.x, y=proj * self.normal.y, z=proj * self.normal.z)

    def project_point(self, other_point: Point):
        pt = other_point.as_array
        plane_normal = self.normal.get_unit_vector().as_array
        point_to_plane = pt - self.point.as_array
        distance_to_plane = np.dot(point_to_plane, plane_normal)
        projected_point = pt - distance_to_plane * plane_normal
        return Point(x=projected_point[0], y=projected_point[1], z=projected_point[2])

    def project_line(self, line: Line):
        projected_start_point = self.project_point(line.point)
        projected_end_along_direction = self.project_point(Point(x=line.point.x + line.direction.x, y=line.point.y + line.direction.y, z=line.point.z + line.direction.z))
        return Line.from_two_points(projected_start_point, projected_end_along_direction)

    def get_projected_vector(self, vector: Vector):
        unit_normal = self.normal.get_unit_vector()
        return Vector.from_array(vector.as_array - vector.get_dot_product(unit_normal) * unit_normal.as_array)
