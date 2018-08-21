var fs = require('fs'),
    parse = require('xml-parser'),
    util = require('util');

const readFile = util.promisify(fs.readFile)

files = ['../styleguide-app/assets/images/icon-calendar.svg'];
file = files[0];

fs.readFile(__dirname + '/' + file, function(err, data) {
  if (err) console.error(err);
  var result = parse(data.toString());
  var name = camelCase(file.replace(/.*\//,''));
  elm = `${name} : Html msg
${name} = ${toElm(result.root)}`;
  console.log(elm);
});

function camelCase(str) {
  let string = str.toLowerCase().replace(/[^A-Za-z0-9]/g, ' ').split(' ')
                  .reduce((result, word) => result + capitalize(word.toLowerCase()))
  return string.charAt(0).toLowerCase() + string.slice(1)
}

function capitalize(str) {
  return str.charAt(0).toUpperCase() + str.toLowerCase().slice(1);
}

function toElm(node) {
  if (node.name == "style")
    return `Svg.${node.name} [${attributes(node.attributes)}] [ Svg.text "${node.content}" ]`;
  else
    return `Svg.${node.name} [${attributes(node.attributes)}] [${node.children.map(toElm).join(',\n')}]`;
}

const regular = ["accentHeight", "accelerate", "accumulate", "additive",
"alphabetic", "allowReorder", "amplitude", "arabicForm", "ascent",
"attributeName", "attributeType", "autoReverse", "azimuth", "baseFrequency",
"baseProfile", "bbox", "begin", "bias", "by", "calcMode", "capHeight", "class",
"clipPathUnits", "contentScriptType", "contentStyleType", "cx", "cy", "d",
"decelerate", "descent", "diffuseConstant", "divisor", "dur", "dx", "dy",
"edgeMode", "elevation", "end", "exponent", "externalResourcesRequired",
"filterRes", "filterUnits", "format", "from", "fx", "fy", "g1", "g2",
"glyphName", "glyphRef", "gradientTransform", "gradientUnits", "hanging",
"height", "horizAdvX", "horizOriginX", "horizOriginY", "id", "ideographic",
"in_", "in2", "intercept", "k", "k1", "k2", "k3", "k4", "kernelMatrix",
"kernelUnitLength", "keyPoints", "keySplines", "keyTimes", "lang",
"lengthAdjust", "limitingConeAngle", "local", "markerHeight", "markerUnits",
"markerWidth", "maskContentUnits", "maskUnits", "mathematical", "max", "media",
"method", "min", "mode", "name", "numOctaves", "offset", "operator", "order",
"orient", "orientation", "origin", "overlinePosition", "overlineThickness",
"panose1", "path", "pathLength", "patternContentUnits", "patternTransform",
"patternUnits", "pointOrder", "points", "pointsAtX", "pointsAtY", "pointsAtZ",
"preserveAlpha", "preserveAspectRatio", "primitiveUnits", "r", "radius",
"refX", "refY", "renderingIntent", "repeatCount", "repeatDur",
"requiredExtensions", "requiredFeatures", "restart", "result", "rotate", "rx",
"ry", "scale", "seed", "slope", "spacing", "specularConstant",
"specularExponent", "speed", "spreadMethod", "startOffset", "stdDeviation",
"stemh", "stemv", "stitchTiles", "strikethroughPosition",
"strikethroughThickness", "string", "style", "surfaceScale", "systemLanguage",
"tableValues", "target", "targetX", "targetY", "textLength", "title", "to",
"transform", "type_", "u1", "u2", "underlinePosition", "underlineThickness",
"unicode", "unicodeRange", "unitsPerEm", "vAlphabetic", "vHanging",
"vIdeographic", "vMathematical", "values", "version", "vertAdvY",
"vertOriginX", "vertOriginY", "viewBox", "viewTarget", "width", "widths", "x",
"xHeight", "x1", "x2", "xChannelSelector", "xlinkActuate", "xlinkArcrole",
"xlinkHref", "xlinkRole", "xlinkShow", "xlinkTitle", "xlinkType", "xmlBase",
"xmlLang", "xmlSpace", "y", "y1", "y2", "yChannelSelector", "z", "zoomAndPan"]

const presentation = ["alignmentBaseline", "baselineShift", "clipPath",
"clipRule", "clip", "colorInterpolationFilters", "colorInterpolation",
"colorProfile", "colorRendering", "color", "cursor", "direction", "display",
"dominantBaseline", "enableBackground", "fillOpacity", "fillRule", "fill",
"filter", "floodColor", "floodOpacity", "fontFamily", "fontSizeAdjust",
"fontSize", "fontStretch", "fontStyle", "fontVariant", "fontWeight",
"glyphOrientationHorizontal", "glyphOrientationVertical", "imageRendering",
"kerning", "letterSpacing", "lightingColor", "markerEnd", "markerMid",
"markerStart", "mask", "opacity", "overflow", "pointerEvents",
"shapeRendering", "stopColor", "stopOpacity", "strokeDasharray",
"strokeDashoffset", "strokeLinecap", "strokeLinejoin", "strokeMiterlimit",
"strokeOpacity", "strokeWidth", "stroke", "textAnchor", "textDecoration",
"textRendering", "unicodeBidi", "visibility", "wordSpacing", "writingMode"]

function attributes(node_attributes) {
  var attrs = Object.keys(node_attributes)
  // console.log(attrs);
  return attrs.map(function(name) {
    var value = node_attributes[name];
    if (regular.includes(name) || presentation.includes(name))
      return `A.${name} "${value}"`;
    // switch (name) {
    //   case "x":
    //     return `Attributes.x "${value}"`;
    //   case "y":
    //     return `Attributes.y "${value}"`;
    //   case "viewBox":
    //     return `Attributes.viewBox "${value}"`;
    //   case "style":
    //     return `Attributes.style "${value}"`;
    //   case "xml:space":
    //     return `Attributes.xmlSpace "${value}"`;
    // }
  }).filter(v => v).join(", ")
}

/*
Root:
Svg.svg [x, y, viewBox, style, xmlSpace] [ recurse! ]

Nodes:
Svg.rect [] []
Svg.path [] []
*/
