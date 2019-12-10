import 'dart:io';
import 'dart:math';

void main() {
  final file = new File('input');
  
  var contents = file.readAsStringSync();
  var lines = contents.split("\n");
  var map = lines.map((line) => line.split('')).toList();
  
  var height = map.length;
  var width = map[0].length;

  var maxVisible = 0;
  var coords;

  for(var oi = 0; oi < width; oi++) {
    for(var oj = 0; oj < height; oj++) {
      if(map[oj][oi] == ".") continue;
      var asteroids = Set();
      var asteroidsBefore = Set();

      for(var ii = 0; ii < width; ii++) {
        for(var ij = 0; ij < height; ij++) {
          if(ii == oi && ij == oj) {
            asteroidsBefore = asteroids;
            asteroids = Set();
            continue;
          }

          if(map[ij][ii] == '.') continue;

          var x = oi - ii;
          var y = oj - ij;
          var angle = x / y;
          asteroids.add(angle);
        }
      }

      var visible = asteroidsBefore.length + asteroids.length;

      if(visible > maxVisible) {
        maxVisible = visible;
        coords = [oi, oj];
      }
    }
  }

  print(maxVisible);
  print(coords);
  
  var polarCoords = new Map();
  var x = coords[0];
  var y = coords[1];

  for(var i = 0; i < width; i++) {
    for(var j = 0; j < height; j++) {
      if(x == i && y == j) continue;
      if(map[j][i] == ".") continue;
      
      var rel_x = i - x;
      var rel_y = (j - y);
      var angle = atan2(rel_x,rel_y);
      if(angle.isNaN) angle = 0;
      var dist = pow(pow(rel_x, 2) + pow(rel_y, 2), 1/2);
      var list = List();
      if(!polarCoords.containsKey(angle)) {
        list = List();
      } else {
        list = polarCoords[angle];
      }
      list.add([dist, i, j]);
      list.sort((x,y) => x[0].compareTo(y[0]));
      polarCoords[angle] = list;
    }
  }
  print(polarCoords.keys.length);

  const epsilon = 0.0000001;
  var currentAngle = pi + epsilon;

  var numDestroyed = 0;
  var lastDestroyed;
  while(numDestroyed < 200) {
    var allAngles = polarCoords.keys.toList();
    allAngles.sort();
    allAngles = allAngles.reversed.toList();
    var nextAngle = allAngles.firstWhere((ang) => ang < currentAngle, orElse: () => null);
    if(nextAngle == null) {
      currentAngle = pi + epsilon;
      nextAngle = allAngles.firstWhere((ang) => ang < currentAngle);
    }

    var asteroids = polarCoords[nextAngle];
    var asteroid = asteroids.removeAt(0);
    if(asteroids.length == 0) {
      polarCoords.remove(nextAngle);
    } else {
      polarCoords[nextAngle] = asteroids;
    }

    lastDestroyed = [asteroid[1], asteroid[2]];
    //print([nextAngle, asteroid]);
    currentAngle = nextAngle;
    numDestroyed++;
  }

  print(lastDestroyed[0] * 100 + lastDestroyed[1]);
}