## wget -N http://gcc.gnu.org/svn/gcc/trunk/libstdc++-v3/python/libstdcxx/v6/printers.py -P .gdbinit.d/python/libstdcxx/v6

python
import os, sys
sys.path.append('/usr/share/gcc-6/python')
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers (None)

sys.path.insert(0, os.path.expanduser('~/.gdbinit.d/python'))
from boost.printers import register_printer_gen
register_printer_gen(None)
end

source ~/.gdbinit.d/osrm-backend.py
source ~/.gdbinit.d/osrm-stage.py


define hook-quit
  set confirm off
end

# set follow-fork-mode child
set pagination off
set history filename ~/.gdb_history
set history remove-duplicates unlimited
set history save

define sanbreaks
  rbreak ^__ubsan_handle_.*
  rbreak ^__asan_report_.*
end

# no PCRE regular expression recursion support https://regex101.com/r/kHuljM/1
# skip -rfu ^std::([a-zA-z0-9_]+)<([^>]|(?R)?)>::~?\1 *\\(
skip -rfu ^std::([a-zA-z0-9_]+)<.+>::\\1\\(
skip -rfu ^std::move
skip -rfu ^std::

define re
  b main
  r -t1 -p ../profiles/car.lua map.osm
  set scheduler-locking step
  clear main
  cont
end
define rr
  b main
  r -t1 -aMLD map.osrm
  set scheduler-locking step
  clear main
  cont
end
