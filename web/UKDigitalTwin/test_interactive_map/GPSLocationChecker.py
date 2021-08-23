####################################################
# Original author: Tom Savage (trs3@cam.ac.uk)     #
# Modified by: Wanni Xie (wx243@cam.ac.uk)         #
# Last Update Date: 04 August 2021                 #
####################################################

def check_GPS_char(c):
  #Checks to provided character is acceptable in GPS format
  acceptable = "-.0123456789"
  if c in acceptable:
    return True
  return False

def GPS_special_chars(coordinate):
  #Checks that if there's a '-' it's only in the first position.
  #Checks that there is only one '.' decimal point.
  acceptable = ".0123456789"
  tick = 0
  point = 0
  for i in coordinate[1]:
    if (tick > 0) and not (i in acceptable):
      coordinate[1] = coordinate[1][:tick] + coordinate[1][tick+1:]
      tick -= 1
      print("Warning: " + coordinate[0] + " contains (automatically removed) misplaced '-' in longitude.")
    if i == '.':
      if point > 0:
        coordinate[1] = coordinate[1][:tick] + coordinate[1][tick+1:]
        tick -= 1
        print("Warning: " + coordinate[0] + " contains (automatically removed) additional '.' in longitude.")
      point += 1
    tick += 1
  tick = 0
  point = 0
  for i in coordinate[2]:
    if (tick > 0) and not (i in acceptable):
      coordinate[2] = coordinate[2][:tick] + coordinate[2][tick+1:]
      tick -= 1
      print("Warning: " + coordinate[0] + " contains (automatically removed) misplaced '-' in latitude.")
    if i == '.':
      if point > 0:
        coordinate[2] = coordinate[2][:tick] + coordinate[2][tick+1:]
        tick -= 1
        print("Warning: " + coordinate[0] + " contains (automatically removed) additional '.' in latitude.")
      point += 1
    tick += 1
  return coordinate

def check_GPS(ret):
  #Checks the GPS coordinates are of the correct format.
  #the 'ret' input array should have the first three columns as:
  #0: The name.
  #1: The longitude.
  #2: The latitude.
  #3+: (Optional) Doesn't matter, it just uses the first three columns, if you have more that's fine. 
  #Note that the 'automatic replacement' is not in the knowledge graph, just on the output side.
  for i in ret:
    #Check the longitude and latitude don't have unexpected characters and have at least some permitted characters. 
    for j in i[1]:
      if not check_GPS_char(j):
        print("Warning: " + i[0] + " contains (automatically removed) longitude (GPS) incompatible character: " + j)
        i[1] = i[1].replace(j, '')
    if len(i[1]) == 0:
      print("Warning: " + i[0] + " contains no valid longitude, 0.0000000 substituted")
      i[1] = "0.0000000"
    for j in i[2]:
      if not check_GPS_char(j):
        print("Warning: " + i[0] + " contains (automatically removed) latitude (GPS) incompatible character: " + j)
        i[2] = i[2].replace(j, '')
    if len(i[2]) == 0:
      print("Warning: " + i[0] + " contains no valid latitude, 0.0000000 substituted")
      i[2] = "0.0000000"
    #Check the longitude and latitude have special characters ('-' and '.') used correctly.
    i = GPS_special_chars(i)
    #Check the longitude and latitude are now valid numbers.
    try:
        float(i[1])
    except ValueError:
        print("Warning: " + i[0] + " contains longitude that is not a number, 0.0000000 substituted")
        i[1] = "0.0000000"
    try:
        float(i[2])
    except ValueError:
        print("Warning: " + i[0] + " contains latitude that is not a number, 0.0000000 substituted")
        i[2] = "0.0000000"
    #Check the longitude and latitude are in the valid range (-180 to 180 for longitude, -90 to 90 for latitude).
    if (float(i[1]) < -180.0) or (float(i[1]) > 180.0):
      print("Warning: " + i[0] + " contains longitude that is out of the valid -180 to 180 range, 0.0000000 substituted")
      i[1] = "0.0000000"
    if (float(i[2]) < -90.0) or (float(i[2]) > 90.0):
      print("Warning: " + i[0] + " contains latitude that is out of the valid -90 to 90 range, 0.0000000 substituted")
      i[2] = "0.0000000"
  print("GPS check completed. If there was a 'Warning' in the output above then the 'automatic replacement / substitution' mentioned does not effect the knowledge graph, just the created JSON file. ")
  return ret
