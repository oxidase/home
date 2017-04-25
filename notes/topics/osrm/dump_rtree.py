import sys
import os
import struct
import math
import geojson
import urllib.parse
import webbrowser
import operator

coordinate_precision, leaf_size = 1e6, 4096
file_name = '../test/data/monaco.osrm'
if len(sys.argv) >= 2: file_name = sys.argv[1]

leafs = []
with open(file_name + '.fileIndex', 'rb') as f:
    for pos in range(0, os.path.getsize(file_name + '.fileIndex'), leaf_size):
        leaf_node = f.read(leaf_size)
        leaf_header = struct.unpack('Iiiii', leaf_node[0:20]) ## StaticRTree::LeafNode
        leaf_data = [struct.unpack('iiII', leaf_node[i:i + 16])[2:4]
                     for i in range(20, len(leaf_node), 32) if len(leaf_node[i:i + 32]) == 32] ## osrm::extractor::EdgeBasedNode
        leafs.append((leaf_header[0], tuple(x/coordinate_precision for x in leaf_header[1:5]), leaf_data))

## hexdump -s 8 -n 128 -e '2/4 " %d" 2/4 " %d" "\n"'  germany-latest.osrm.nodes
with open(file_name + '.nodes', 'rb') as f:
    fingerprint = struct.unpack('4sBBBB', f.read(8))
    nodes_number = struct.unpack('Q', f.read(8))[0]
    node_coords = [struct.unpack('ii', f.read(8)) for i in range(nodes_number)] # osrm::extractor::QueryNode
    ## ignore packed node IDs

print ('Nodes file fingerprint {} v{}.{}.{} x{:X}, nodes number is {}'.format(fingerprint[0].decode('utf-8'), *fingerprint[1:], nodes_number))
print ('Rtree leaves file has {} entries'.format(len(leafs)))

mercator2wgs84 = lambda y: 180. / math.pi * 2. * math.atan(math.exp(y * math.pi / 180.)) - 90.
area = lambda w,e,s,n: (e-w) * (n-s)
wgs84 = lambda b: (b[0], b[1], mercator2wgs84(b[2]), mercator2wgs84(b[3]))
bb2poly = lambda w,e,s,n: ((w,s), (e,s), (e,n), (w,n), (w,s))
node = lambda u: (node_coords[u][0] / coordinate_precision, node_coords[u][1] / coordinate_precision)


encodeURIComponent = lambda s: urllib.parse.quote(s.encode("utf-8"), safe='~()*!.\'')

## total leafs coverage
total_area = area(min(map(lambda l: l[1][0], leafs)), max(map(lambda l: l[1][1], leafs)),
                  min(map(lambda l: l[1][2], leafs)), max(map(lambda l: l[1][3], leafs)))
area_ratio = sum(area(*wgs84(l[1])) for l in leafs) / total_area
avg_nodes = sum(x[0] for x in leafs) / len(leafs)
print ('Area ratio {}, average nodes per leaf {}'.format(area_ratio, avg_nodes))

## all bounding boxes
rectangles = geojson.MultiPolygon([(bb2poly(*wgs84(l[1])), ) for l in leafs])
feature = geojson.Feature(geometry=rectangles,
                          properties={'file': file_name, 'area_ratio': area_ratio,
                                      'stroke': '#555555', 'stroke-width':2, 'fill': '#555555', 'fill-opacity':0.4})
webbrowser.open('http://geojson.io/#data=data:application/json,' + encodeURIComponent(str(feature)))

## max lon/lat rectangle width
stats = lambda x: max(enumerate(x), key=operator.itemgetter(1)) + (sum(x) / len(x), )
idx_lon, max_lon, avg_lon = stats([l[1][1]-l[1][0] for l in leafs])
idx_lat, max_lat, avg_lat = stats([l[1][3]-l[1][2] for l in leafs])

print ('Longitude difference: avg=', avg_lon, round(avg_lon * coordinate_precision), ', max=', max_lon, round(max_lon * coordinate_precision))
print ('Latitude difference: avg=', avg_lat, round(avg_lat * coordinate_precision), ', max=', max_lat, round(max_lat * coordinate_precision))

## max bounding box
print ([(bb2poly(*wgs84(leafs[idx_lon][1])), ), (bb2poly(*wgs84(leafs[idx_lat][1])), )])
rectangles = geojson.MultiPolygon([(bb2poly(*wgs84(leafs[idx_lon][1])),), (bb2poly(*wgs84(leafs[idx_lat][1])), )])
rectangles = geojson.Feature(geometry=rectangles,
                       properties={'file': file_name, 'max_lon': round(max_lon * coordinate_precision),
                                   'max_lat': round(max_lat * coordinate_precision),
                                   'stroke': '#555555', 'stroke-width':2, 'fill': '#555555', 'fill-opacity':0.1})
coord_lon = [(node(c[0]), node(c[1])) for i, c in enumerate(leafs[idx_lon][2]) if i < leafs[idx_lon][0]]
coord_lat = [(node(c[0]), node(c[1])) for i, c in enumerate(leafs[idx_lat][2]) if i < leafs[idx_lat][0]]
lon_segments = geojson.Feature(geometry=geojson.MultiLineString(coord_lon),
                               properties={'stroke': '#0000ff', 'stroke-width':5})
lat_segments = geojson.Feature(geometry=geojson.MultiLineString(coord_lat),
                               properties={'stroke': '#008000', 'stroke-width':5})
webbrowser.open('http://geojson.io/#data=data:application/json,' + encodeURIComponent(str(geojson.FeatureCollection([rectangles, lon_segments, lat_segments]))))
