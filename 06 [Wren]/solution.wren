import "io" for File

class Planet {
  parent { _parent }
  name { _name }

  construct new(name) {
    _name = name
    _orbiting = []
  }

  parent=(planet) {
    _parent = planet
  }

  addOrbiting(planet) {
    planet.parent = this
    _orbiting.add(planet)
  }

  orbitsValue(base) {
    var orbiting_sum = 0
    for (planet in _orbiting) {
      orbiting_sum = orbiting_sum + planet.orbitsValue(base + 1)
    }
    return base + orbiting_sum
  }
}

class Galaxy {
  construct new() {
    _planets = {}
  }

  getOrCreate(name) {
    if(_planets.containsKey(name)) {
      return _planets[name]
    } else {
      var planet = Planet.new(name)
      _planets[name] = planet
      return planet
    }
  }
}

var input = File.read("input")
var lines = input.split("\n")
var galaxy = Galaxy.new()
for (line in lines) {
  var parts = line.split(")")
  var leftPlanet = galaxy.getOrCreate(parts[0])
  var rightPlanet = galaxy.getOrCreate(parts[1])
  leftPlanet.addOrbiting(rightPlanet)
}

var com = galaxy.getOrCreate("COM")
System.print(com.orbitsValue(0))

var routeToCom = Fn.new {|planet|
  var list = []
  var parent = planet.parent
  while(parent != com) {
    list.add(parent)
    planet = parent
    parent = planet.parent
  }
  list.add(com)
  return list
}

var you = galaxy.getOrCreate("YOU")
var yourRoute = routeToCom.call(you)
var santa = galaxy.getOrCreate("SAN")
var santaRoute = routeToCom.call(santa)

var haveSameTail = true
while(haveSameTail) {
  yourRoute = yourRoute[0..-2]
  santaRoute = santaRoute[0..-2]
  haveSameTail = (yourRoute[-1] == santaRoute[-1])
}
System.print(yourRoute.count + santaRoute.count)