import gdb.printing

COORDINATE_PRECISION = 1e6

class CoordinatePrinter:
    """Print a CoordinatePrinter object."""
    def __init__(self, val):
        self.val = val

    def to_string(self):
        lon, lat = int(self.val['lon']['__value']), int(self.val['lat']['__value'])
        return '{%f, %f}' % (float(lon) / COORDINATE_PRECISION, float(lat) / COORDINATE_PRECISION)

class TurnInstructionPrinter:
    """Print a TurnInstruction object."""

    modifiers = {0:'UTurn', 1:'SharpRight', 2:'Right', 3:'SlightRight',
                 4:'Straight', 5:'SlightLeft', 6:'Left', 7:'SharpLeft'}
    types = {0:'Invalid', 1:'NewName', 2:'Continue', 3:'Turn', 4:'Merge', 5:'OnRamp',
             6:'OffRamp', 7:'Fork', 8:'EndOfRoad', 9:'Notification', 10:'EnterRoundabout',
             11:'EnterAndExitRoundabout', 12:'EnterRotary', 13:'EnterAndExitRotary',
             14:'EnterRoundaboutIntersection', 15:'EnterAndExitRoundaboutIntersection',
             16:'UseLane', 17:'NoTurn', 18:'Suppressed', 19:'EnterRoundaboutAtExit',
             20:'ExitRoundabout', 21:'EnterRotaryAtExit', 22:'ExitRotary',
             23:'EnterRoundaboutIntersectionAtExit', 24:'ExitRoundaboutIntersection',
             25:'StayOnRoundabout', 26:'Sliproad'}

    def __init__(self, val):
        self.val = val

    def to_string(self):
        t, m = int(self.val['type']), int(self.val['direction_modifier'])
        m = '%s (%d)' % (self.modifiers[m], m) if m in self.modifiers else str(m)
        t = '%s (%d)' % (self.types[t], t) if t in self.types else str(t)
        return ('{type = ' + t + ', direction_modifier = ' + m + '}')

def build_pretty_printer():
    pp = gdb.printing.RegexpCollectionPrettyPrinter('OSRM')
    pp.add_printer('TurnInstruction', '::TurnInstruction$', TurnInstructionPrinter)
    pp.add_printer('Coordinate', '::Coordinate$', CoordinatePrinter)
    return pp

# gdb.pretty_printers = []
gdb.printing.register_pretty_printer(gdb.current_objfile(), build_pretty_printer())
