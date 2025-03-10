from .model.point_template import PointTemplate
def get_regular_sampled_points(lng_start: float, lng_end:float, lng_step: float,
                               lat_start: float, lat_end:float, lat_step:float) -> str :
    return PointTemplate(lng_start=lng_start, lng_end=lng_end, lng_step=lng_step,
                          lat_start=lat_start, lat_end=lat_end, lat_step=lat_step).get_sql()
    
def get_provided_points(lng: float, lat: float) -> str:
    return PointTemplate(lng_start=lng, lng_end=lng, lng_step=0.1,
                          lat_start=lat, lat_end=lat, lat_step=0.1).get_sql()