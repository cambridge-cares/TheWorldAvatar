# This file contains utility classes for geometric calculations
from typing import List, Tuple, Optional
from itertools import combinations, product
import numpy as np
from scipy.spatial.transform import Rotation as R
from scipy.spatial import Delaunay
from pydantic import BaseModel


class RotationMatrix():
    matrix: np.ndarray

    def __init__(self, matrix: np.ndarray):
        self.matrix = matrix

    def apply(self, vector: np.ndarray):
        return self.matrix @ vector

    def combine(self, other: 'RotationMatrix'):
        return RotationMatrix(matrix=self.matrix @ other.matrix)

    def as_matrix(self):
        return self.matrix

    def as_quaternion(self):
        return R.from_matrix(self.matrix).as_quat()

    def as_quaternion_str(self):
        q = self.as_quaternion()
        return f'{q[0]:f}#{q[1]:f}#{q[2]:f}#{q[3]:f}'

    @classmethod
    def identity(cls):
        return cls(matrix=np.eye(3))


class Quaternion(BaseModel):
    x: float
    y: float
    z: float
    w: float

    @property
    def as_array(self):
        return np.array([self.x, self.y, self.z, self.w])

    def as_str(self):
        return f'{self.x:f}#{self.y:f}#{self.z:f}#{self.w:f}'

    def as_rotation_matrix(self):
        return RotationMatrix(matrix=R.from_quat(self.as_array).as_matrix())

    def rotate_points(self, points: List['Point']):
        return [Point.rotate(pt, self.as_rotation_matrix()) for pt in points]

    @classmethod
    def from_rotation_matrix(cls, rotation_matrix: RotationMatrix):
        q = R.from_matrix(rotation_matrix.matrix).as_quat()
        return cls(x=q[0], y=q[1], z=q[2], w=q[3])

    @classmethod
    def from_string(cls, q_str: str):
        q = q_str.split('#')
        return cls(x=float(q[0]), y=float(q[1]), z=float(q[2]), w=float(q[3]))


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

    def get_distance_to_vector(self, vector: 'Vector'):
        projection = vector.projection_of_point(self)
        return self.get_distance_to(projection)

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

    def rank_distance_to_points(self, points: List['Point']):
        return sorted(points, key=lambda x: self.get_distance_to(x))

    def get_points_within_threshold_distance(self, points: List['Point'], threshold_distance: float):
        return [pt for pt in points if self.get_distance_to(pt) <= threshold_distance]

    @classmethod
    def mid_point(cls, pt1: 'Point', pt2: 'Point'):
        return cls(x=(pt1.x + pt2.x) / 2, y=(pt1.y + pt2.y) / 2, z=(pt1.z + pt2.z) / 2)

    @classmethod
    def centroid(cls, points: List['Point']):
        return cls(x=np.mean([pt.x for pt in points]), y=np.mean([pt.y for pt in points]), z=np.mean([pt.z for pt in points]))

    @classmethod
    def from_array(cls, arr, label=None):
        return cls(x=arr[0], y=arr[1], z=arr[2], label=label)

    @classmethod
    def rotate(cls, point: 'Point', rotation: RotationMatrix):
        _ = rotation.apply(point.as_array)
        return cls(x=_[0], y=_[1], z=_[2], label=point.label)

    @classmethod
    def scale(cls, point: 'Point', factor: float):
        return cls(x=point.x * factor, y=point.y * factor, z=point.z * factor, label=point.label)

    @classmethod
    def translate(cls, point: 'Point', vector: 'Vector'):
        return cls(x=point.x + vector.x, y=point.y + vector.y, z=point.z + vector.z, label=point.label)

    @classmethod
    def translate_points_to_target_centroid(cls, points: List['Point'], target_centroid: 'Point') -> Tuple[List['Point'], 'Vector']:
        current_centroid = cls.centroid(points)
        translation = current_centroid.get_translation_vector_to(target_centroid)
        return [cls.translate(pt, translation) for pt in points], translation

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

    @classmethod
    def closest_pair(cls, points: List['Point']):
        min_dist = np.inf
        min_points = (None, None)
        for pt1, pt2 in combinations(points, 2):
            dist = pt1.get_distance_to(pt2)
            if dist < min_dist:
                min_dist = dist
                min_points = (pt1, pt2)
        return min_points

    @classmethod
    def closest_pair_across_lists(cls, point_list1: List['Point'], point_list2: List['Point']):
        min_dist = np.inf
        min_points = (None, None)
        for pt1, pt2 in product(point_list1, point_list2):
            dist = pt1.get_distance_to(pt2)
            if dist < min_dist:
                min_dist = dist
                min_points = (pt1, pt2)
        return min_points

    @classmethod
    def fit_circle_2d(cls, points: List['Point']):
        if len(points) < 3:
            raise ValueError(f'At least 3 points are required to fit circumcenter. Provided points: {points}')
        elif len(points) > 5:
            raise NotImplementedError('Fitting circumcenter for more than 5 points is not implemented yet')
        else:
            # least squares solution to find the circumcenter
            # first fit a plane
            plane = Plane.fit_from_points(points)
            # project the points to the plane
            projected_points = [plane.project_point(pt) for pt in points]
            # find a local 2-D coordinate system using the first two points
            origin = projected_points[0]
            u = Vector.from_two_points(start=origin, end=projected_points[1])
            u = u.get_unit_vector()
            norm = plane.normal.get_unit_vector()
            v = norm.get_cross_product(u)
            v /= np.linalg.norm(v)
            # project the points to the local 2-D coordinate system
            local_points = [Point(x=np.dot(pt.as_array - origin.as_array, u.as_array), y=np.dot(pt.as_array - origin.as_array, v), z=0) for pt in projected_points]
            local_np_arrs = np.array([pt.as_array for pt in local_points])
            local_x = local_np_arrs[:, 0]
            local_y = local_np_arrs[:, 1]
            a = np.column_stack((local_x, local_y, np.ones(len(local_x))))
            b = local_x**2 + local_y**2
            a_coef, b_coef, c_coef = np.linalg.lstsq(a, b, rcond=None)[0]
            x_center = a_coef / 2
            y_center = b_coef / 2
            c = origin.as_array + x_center * u.as_array + y_center * v
            r = np.sqrt(x_center**2 + y_center**2 + c_coef)
            return cls.from_array(c), r

class Vector(BaseModel):
    x: float
    y: float
    z: float

    def __repr__(self):
        return f"Vector({self.x}, {self.y}, {self.z})"

    def as_str(self):
        return f"{self.x:f}#{self.y:f}#{self.z:f}"

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

    def projection_of_point(self, point: Point):
        vector_unit = self.get_unit_vector()
        scalar_projection = np.dot(point.as_array, vector_unit.as_array)
        projection = scalar_projection * vector_unit.as_array
        return Point(x=projection[0], y=projection[1], z=projection[2])

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
    def from_string(cls, string):
        arr = string.split('#')
        return cls(x=float(arr[0]), y=float(arr[1]), z=float(arr[2]))

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

    def project_point(self, other_point: Point):
        if self.is_point_on_line(other_point):
            return Point(x=other_point.x, y=other_point.y, z=other_point.z)
        v = Vector.from_two_points(self.point, other_point)
        v_proj = (np.dot(v.as_array, self.direction.as_array) / np.dot(self.direction.as_array, self.direction.as_array)) * self.direction.as_array
        return Point(x=self.point.x + v_proj[0], y=self.point.y + v_proj[1], z=self.point.z + v_proj[2])

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

    @property
    def local_2d_coordinate_system_vectors(self):
        if self.normal.x != 0 or self.normal.y != 0:
            v1 = Vector(x=-self.normal.y, y=self.normal.x, z=0).get_unit_vector()
        else:
            v1 = Vector(x=1, y=0, z=0)
        v2 = Vector.from_array(self.normal.get_cross_product(v1))
        return v1, v2

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
        v1 = Vector(x = pt2.x - pt1.x, y = pt2.y - pt1.y, z = pt2.z - pt1.z)
        v2 = Vector(x = pt3.x - pt1.x, y = pt3.y - pt1.y, z = pt3.z - pt1.z)
        return cls.from_two_vectors(v1, v2, pt1)

    @classmethod
    def fit_from_points(cls, points: List[Point]):
        n = len(points)
        if n < 3:
            raise ValueError(f'At least 3 points are required to fit a plane. Provided points: {points}')
        elif n == 3:
            return cls.from_three_points(pt1=points[0], pt2=points[1], pt3=points[2])
        else:
            A = np.array([[pt.x, pt.y, pt.z] for pt in points])
            centroid = np.mean(A, axis=0)
            A -= centroid
            _, _, V = np.linalg.svd(A)
            normal = V[-1]
            return cls(point=Point.from_array(centroid), normal=Vector.from_array(normal))

    def normal_vector_from_point_to_plane(self, other: Point) -> Vector:
        w = Vector.from_two_points(start=other, end=self.point)
        proj = w.get_dot_product(self.normal) / self.normal.magnitude
        if proj == 0:
            # this means the point is on the plane
            # so we return the normal vector
            return Vector.from_array(self.normal.as_array)
        return Vector(x=proj * self.normal.x, y=proj * self.normal.y, z=proj * self.normal.z)

    def project_point(self, other_point: Point) -> Point:
        pt = other_point.as_array
        plane_normal = self.normal.get_unit_vector().as_array
        point_to_plane = pt - self.point.as_array
        distance_to_plane = np.dot(point_to_plane, plane_normal)
        projected_point = pt - distance_to_plane * plane_normal
        return Point(x=projected_point[0], y=projected_point[1], z=projected_point[2])

    def project_line(self, line: Line) -> Line:
        projected_start_point = self.project_point(line.point)
        projected_end_along_direction = self.project_point(Point(x=line.point.x + line.direction.x, y=line.point.y + line.direction.y, z=line.point.z + line.direction.z))
        return Line.from_two_points(projected_start_point, projected_end_along_direction)

    def get_projected_vector(self, vector: Vector) -> Vector:
        unit_normal = self.normal.get_unit_vector()
        return Vector.from_array(vector.as_array - vector.get_dot_product(unit_normal) * unit_normal.as_array)

    def find_perpendicular_bisector_on_plane(self, point1: Point, point2: Point) -> Line:
        projected_point_1 = self.project_point(point1)
        projected_point_2 = self.project_point(point2)
        mid_point = Point.mid_point(projected_point_1, projected_point_2)
        proj_line = Line.from_two_points(projected_point_1, projected_point_2)
        v = self.normal.get_cross_product(proj_line.direction)
        if np.linalg.norm(v) == 0:
            raise ValueError(f'The two points {point1} and {point2} define a line that is perpendicular to the plane {self}. No unique perpendicular bisector exists on the plane.')
        else:
            v = v / np.linalg.norm(v)
        return Line(point=mid_point, direction=Vector.from_array(v))

    def find_intersection_of_lines_projected(self, line1: Line, line2: Line) -> Point:
        proj_l1 = self.project_line(line1)
        proj_l2 = self.project_line(line2)
        a = np.array([proj_l1.direction.as_array, -proj_l2.direction.as_array]).T
        b = proj_l2.point.as_array - proj_l1.point.as_array
        if np.linalg.matrix_rank(a) < 2:
            # the two lines are parallel
            return None
        t = np.linalg.lstsq(a, b, rcond=None)[0][0]
        intersection = proj_l1.point.as_array + t * proj_l1.direction.as_array
        return Point.from_array(intersection)

    def project_points_to_local_2d_coordinate(self, points: List['Point']):
        projected_points = [self.project_point(pt) for pt in points]
        v1, v2 = self.local_2d_coordinate_system_vectors
        # project the points to the local 2-D coordinate system
        local_points = []
        for pt in projected_points:
            x = np.dot(pt.as_array - self.point.as_array, v1.as_array)
            y = np.dot(pt.as_array - self.point.as_array, v2.as_array)
            local_points.append(Point(x=x, y=y, z=0))
        return local_points

    def local_2d_delaunay_triangulation(self, points: List['Point']):
        local_points = self.project_points_to_local_2d_coordinate(points)
        delaunay = Delaunay(np.array([pt.as_array for pt in local_points]))
        return delaunay
