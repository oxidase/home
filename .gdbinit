## wget -N http://gcc.gnu.org/svn/gcc/trunk/libstdc++-v3/python/libstdcxx/v6/printers.py -P .gdbinit.d/python/libstdcxx/v6

# python
# import sys, os
# sys.path.insert(0, os.path.expanduser('~/.gdbinit.d/python'))
# import osrm.printers
# end
source ~/foss/osrm-backend/scripts/gdb_printers.py

define hook-quit
  set confirm off
end

set follow-fork-mode child
set pagination off
set history filename ~/.gdb_history
set history save

define sanbreaks
  rbreak ^__ubsan_handle_.*
  rbreak ^__asan_report_.*
end
