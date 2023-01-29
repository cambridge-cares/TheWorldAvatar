# The purpose of this module is to print available HTTP requests (i.e. routes)
# at the application root

from flask import Blueprint


# Blueprint Configuration
home_bp = Blueprint(
    'home_bp', __name__
)

# Show an instructional message at the app root
@home_bp.route('/', methods=['GET'])
def default():
    msg  = "The <b>lsoacalculationagent_cop</b> offers the following function via the specified API endpoints:<BR>"
    msg += "<BR>"
    msg += "Request to calculate the COP based on temperature and return the COP in JSON format (GET request):<BR>"
    msg += "(request expects all individual query parameter to be provided in a single nested JSON object with key 'query', see details below):<BR>"
    msg += "&nbsp;&nbsp; <a href=\"/api/lsoacalculationagent_cop/calculation/cop\"> /api/lsoacalculationagent_cop/calculation/cop </a> "
    msg += "<BR>"
    msg += "<BR>"
    msg += "<b>Example query:</b><BR>"
    msg += "<div style=\"text-indent: 25px;\">    <code>query = {<BR> "
    msg += "<div style=\"text-indent: 50px;\">       'query':{'temperature': [5] ,<BR>"
    msg += "<div style=\"text-indent: 100px;\">                 'unit':'http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius',<BR>"
    msg += "<div style=\"text-indent: 100px;\">                 'lsoa_sequence': ['http://statistics.data.gov.uk/id/statistical-geography/E01012991'] <BR>"
    msg += "<div style=\"text-indent: 100px;\">               }<BR>"
    msg += "<div style=\"text-indent: 50px;\">       }</code><BR>"
    msg += "<div style=\"text-indent: 50px;\">    <BR>"
    msg += "<b>Example return:</b><BR>"
    msg += "<div style=\"text-indent: 50px;\">    <code>return = {'COP': [2.5] ,<BR>"
    msg += "<div style=\"text-indent: 100px;\">               'lsoa_sequence': ['http://statistics.data.gov.uk/id/statistical-geography/E01012991'] <BR>"
    msg += "<div style=\"text-indent: 50px;\">       }</code><BR>"
    msg += "<div style=\"text-indent: 50px;\">    <BR>"
    msg += "<b>Arguments:</b><BR>"
    msg += "<div style=\"text-indent: 25px;\">    <li>temperature: should be an array (np.ndarray) I suppose you are not interested in calculating single value...<BR>"
    msg += "<div style=\"text-indent: 25px;\">    <li>unit: unit with respect to temperature, must be wrapped as a url as per OntoMeasurement. Units below are available:<BR>"
    msg += "<div style=\"text-indent: 50px;\">                     <code>DegreeCelsius <span>&#x2103;</span>: &nbsp;&nbsp; <span style=\"text-decoration: underline;\"> http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius</span>  <BR>"
    msg += "<div style=\"text-indent: 50px;\">                      DegreeFahrenheit <span>&#x2109;</span>: &nbsp;&nbsp; <span style=\"text-decoration: underline;\"> http://www.ontology-of-units-of-measure.org/resource/om-2/degreeFahrenheit</span>  <BR>"
    msg += "<div style=\"text-indent: 50px;\">                      Kelvin K: &nbsp;&nbsp; <span style=\"text-decoration: underline;\"> http://www.ontology-of-units-of-measure.org/resource/om-2/kelvin</span> </code><BR>"
    msg += "<div style=\"text-indent: 25px;\">    <li>lsoa_sequence: should be an array (np.ndarray), this is the respective lsoa code with the temperature you provided, which must be wrapped as a url as per OntoGasGrid <BR>"
    msg += "<div style=\"text-indent: 25px;\">    if the lsoa_sequence is not provided the agent will still run and the result will still be given, with a warning in the logger"
    msg += "<BR></li></div> "
    msg += "<BR>"
    return msg