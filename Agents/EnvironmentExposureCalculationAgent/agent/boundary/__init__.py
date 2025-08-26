from flask import abort

from agent.boundary.buffer import create_buffer_around_points

def create_boundary(query_id: str, boundary: str, data:dict):
    if boundary == 'buffer':
        boundary_radius = float(data['boundary_radius'])
        create_buffer_around_points(query_id, boundary_radius)
    else:
        abort(400, description="Unsupported buffer method")

