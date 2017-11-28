import gdb.printing

# https://sourceware.org/gdb/onlinedocs/gdb/Pretty-Printing.html
# https://sourceware.org/gdb/onlinedocs/gdb/Writing-a-Pretty_002dPrinter.html

COORDINATE_PRECISION = 1e6
coord2float = lambda x: int(x) / COORDINATE_PRECISION
lonlat = lambda x: (coord2float(x['lon']['__value']), coord2float(x['lat']['__value']))

def call(this, method, *args):
    """Call this.method(args)"""
    command = '(*({})({})).{}({})'.format(this.type.target().pointer(), this.address, method, ','.join((str(x) for x in args)))
    return gdb.parse_and_eval(command)

def iterate(v):
    s, e = v['_M_impl']['_M_start'], v['_M_impl']['_M_finish']
    while s != e:
        yield s.dereference()
        s +=1

import re
import sys
print(sys.version)
import math

import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import datetime

plt.ion()

class Plotter(gdb.Command):
    """plot smth."""

    def __init__ (self):
        gdb.Command.__init__ (self, "plot", gdb.COMMAND_USER, gdb.COMPLETE_EXPRESSION)
        self.plotters = {
            'int': self.plot_test,
            'osrm::extractor::guidance::Intersection': self.plot_intersection,
            'const osrm::extractor::guidance::IntersectionView': self.plot_intersection,
            'osrm::extractor::guidance::IntersectionView': self.plot_intersection,
            'osrm::extractor::guidance::IntersectionShape': self.plot_intersection,
            'osrm::extractor::guidance::IntersectionNormalizer::NormalizationResult': self.plot_intersection,
            'osrm::extractor::intersection::IntersectionEdgeGeometries': self.plot_intersection_edges,
            'osrm::extractor::guidance::TurnAnalysis::ShapeResult' : self.plot_intersection_shapes,
            'std::vector<osrm::util::Coordinate, std::allocator<osrm::util::Coordinate> >': self.plot_coordinates,
            'osrm::extractor::NodeBasedGraphFactory': self.plot_nbg}

    def invoke (self, arg, from_tty):
        if arg.startswith('/c'):
            plt.ioff()
            plt.close()
            arg = arg[2:].strip()
            if len(arg) == 0: return
        elif arg.startswith('/i'):
            plt.ioff()
            plt.show()
            return
        elif arg.startswith('/s'):
            filename = datetime.datetime.now().strftime('plot_%Y%m%d-%H%M%S.png')
            print ('saved to' , filename)
            plt.savefig(filename)
            return
        try:
            plt.ion()
            fig = plt.figure(1, figsize=(10, 10))
            ax = fig.add_subplot(111)
            ax.get_xaxis().get_major_formatter().set_useOffset(False)
            ax.get_yaxis().get_major_formatter().set_useOffset(False)

            args = arg.split(' ')
            val = gdb.parse_and_eval(args[0])
            type = val.type.target().unqualified() if val.type.code == gdb.TYPE_CODE_REF else val.type.unqualified()
            print (type)
            if (self.plotters[str(type)](ax, val, *args[1:])):
                plt.pause(0.0001)
                plt.show()
            else:
                plt.ioff()
                plt.close()
        except KeyError as e:
            print ('no GeoJSON printer for: ' + str(e))
        except gdb.error as e:
            print (e)

    @staticmethod
    def plot_test(axis, value, color='k'):
        print (np.random.randn(10))
        axis.plot(np.random.randn(10))

    @staticmethod
    def plot_intersection_shapes(axis, shape_result, *args):
        Plotter.plot_intersection(axis, shape_result['intersection_shape'], args)
        Plotter.plot_intersection(axis, shape_result['annotated_normalized_shape']['normalized_shape'], args)

    @staticmethod
    def plot_intersection(axis, intersection, *args):
        args = ' '.join(args)
        color = re.search('[bgrcmykw]', ' '.join(args))
        color = color.group(0) if color else 'k'
        origin = re.search('([0-9\.]+),([0-9\.]+)', args)
        origin = [float(x) for x in origin.groups()] if origin else None

        for index, road in enumerate(iterate(intersection)):
            fields = set([x.name for x in road.type.fields()])
            style = color + ('-' if 'entry_allowed' not in fields or road['entry_allowed'] else '--')
            if True:
                instruction = re.sub('[A-Za-z_]+ = ', '', str(road['instruction'])) if 'instruction' in fields else ''
                bearing = math.pi * ((360. + 90. - float(road['bearing'])) % 360) / 180.
                eid = road['eid']
                angle = float(road['angle']) if 'angle' in fields else float('nan')
                segment_length = float(road['segment_length'])
            else:
                instruction = re.sub('[A-Za-z_]+ = ', '', str(road['turn']))
                bearing = math.pi * ((360. + 90. - float(road['turn']['bearing'])) % 360) / 180.
                eid = road['turn']['eid']
                angle = float(road['turn']['angle'])
                segment_length = 1

            if origin is not None:
                x0, y0 = origin
                x1, y1 = x0 + segment_length * math.cos(bearing) / 111319.5, y0 + segment_length * math.sin(bearing) / 111319.5
            else:
                x0, y0, x1, y1 = 0, 0, segment_length * math.cos(bearing), segment_length * math.sin(bearing)
            axis.plot([x0, x1], [y0, y1], style)
            axis.text(x1, y1, '{}: {}{:.1f}m\n'.format(eid, '{:.1f}°, '.format(angle) if math.isfinite(angle) else '', segment_length) + instruction,
                      {'ha': 'center', 'va': 'top' if bearing > math.pi else 'baseline'},
                      rotation=0 * bearing / math.pi)
        axis.set_aspect('equal', 'datalim')
        return True

    @staticmethod
    def plot_intersection_edges(axis, intersection, *args):
        args = ' '.join(args)
        color = re.search('[bgrcmykw]', ' '.join(args))
        color = color.group(0) if color else 'k'
        origin = re.search('([0-9\.]+),([0-9\.]+)', args)
        origin = [float(x) for x in origin.groups()] if origin else None

        for index, road in enumerate(iterate(intersection)):
            fields = set([x.name for x in road.type.fields()])
            style = color + ('-' if 'entry_allowed' not in fields or road['entry_allowed'] else '--')
            instruction = re.sub('[A-Za-z_]+ = ', '', str(road['instruction'])) if 'instruction' in fields else ''
            initial_bearing = math.pi * ((360. + 90. - float(road['initial_bearing'])) % 360) / 180.
            perceived_bearing = math.pi * ((360. + 90. - float(road['perceived_bearing'])) % 360) / 180.
            eid = road['edge']
            angle = float(road['angle']) if 'angle' in fields else float('nan')
            length = float(road['length'])

            if origin is not None:
                x0, y0 = origin
                x1, y1 = x0 + length * math.cos(perceived_bearing) / 111319.5, y0 + length * math.sin(perceived_bearing) / 111319.5
            else:
                x0, y0, x1, y1 = 0, 0, length * math.cos(perceived_bearing), length * math.sin(perceived_bearing)
            axis.plot([x0, x1], [y0, y1], style)
            axis.text(x1, y1, '{}: {}{:.1f}m\n'.format(eid, '{:.1f}°, '.format(angle) if math.isfinite(angle) else '', length) + instruction,
                      {'ha': 'center', 'va': 'top' if perceived_bearing > math.pi else 'baseline'},
                      rotation=0 * perceived_bearing / math.pi)
        axis.set_aspect('equal', 'datalim')
        return True

    @staticmethod
    def plot_coordinates(axis, coordinates, *args):
        coordinates = [lonlat(coord) for coord in iterate(coordinates)]
        style = args[0] if len(args) > 0 else 'k'
        axis.plot([x[0] for x in coordinates], [x[1] for x in coordinates], style)
        return True


    @staticmethod
    def plot_nbg(axis, factory, *args):

        graph = call(factory, 'GetGraph').address.reinterpret_cast(gdb.lookup_type('osrm::util::NodeBasedDynamicGraph').pointer()).dereference()
        osmids = call(factory, 'GetOsmNodes').address.reinterpret_cast(gdb.lookup_type('osrm::extractor::PackedOSMIDs').pointer()).dereference()
        coordinates = call(factory, 'GetCoordinates').address.reinterpret_cast(gdb.lookup_type('std::vector<osrm::util::Coordinate, std::allocator<osrm::util::Coordinate> >').pointer()).dereference()

        print (osmids)
        print (coordinates)

        print (graph, graph.type)
        print (call(graph, 'GetNumberOfNodes'))
        print ('ok')
        #for source in range(call(graph, 'GetNumberOfNodes')):
        #print (call(graph['node_array'], 'at', 0))
            #print (call(coordinates, 'at', source))

        # args = ' '.join(args)
        # color = re.search('[bgrcmykw]', ' '.join(args))
        # color = color.group(0) if color else 'k'
        # origin = re.search('([0-9\.]+),([0-9\.]+)', args)
        # origin = [float(x) for x in origin.groups()] if origin else None

        # for index, road in enumerate(iterate(intersection)):
        #     fields = set([x.name for x in road.type.fields()])
        #     style = color + ('-' if 'entry_allowed' not in fields or road['entry_allowed'] else '--')
        #     if True:
        #         instruction = re.sub('[A-Za-z_]+ = ', '', str(road['instruction'])) if 'instruction' in fields else ''
        #         bearing = math.pi * ((360. + 90. - float(road['bearing'])) % 360) / 180.
        #         eid = road['eid']
        #         angle = float(road['angle']) if 'angle' in fields else float('nan')
        #         segment_length = float(road['segment_length'])
        #     else:
        #         instruction = re.sub('[A-Za-z_]+ = ', '', str(road['turn']))
        #         bearing = math.pi * ((360. + 90. - float(road['turn']['bearing'])) % 360) / 180.
        #         eid = road['turn']['eid']
        #         angle = float(road['turn']['angle'])
        #         segment_length = 1

        #     if origin is not None:
        #         x0, y0 = origin
        #         x1, y1 = x0 + segment_length * math.cos(bearing) / 111319.5, y0 + segment_length * math.sin(bearing) / 111319.5
        #     else:
        #         x0, y0, x1, y1 = 0, 0, segment_length * math.cos(bearing), segment_length * math.sin(bearing)
        #     axis.plot([x0, x1], [y0, y1], style)
        #     axis.text(x1, y1, '{}: {}{:.1f}m\n'.format(eid, '{:.1f}°, '.format(angle) if math.isfinite(angle) else '', segment_length) + instruction,
        #               {'ha': 'center', 'va': 'top' if bearing > math.pi else 'baseline'},
        #               rotation=0 * bearing / math.pi)
        # axis.set_aspect('equal', 'datalim')
        return False

Plotter()
