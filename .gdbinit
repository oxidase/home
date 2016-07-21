
## wget -N http://gcc.gnu.org/svn/gcc/trunk/libstdc++-v3/python/libstdcxx/v6/printers.py -P .gdbinit.d/python/libstdcxx/v6
#python
#import sys, os
#sys.path.insert(0, os.path.expanduser('~/.gdbinit.d/python'))
#from libstdcxx.v6.printers import register_libstdcxx_printers
#register_libstdcxx_printers (None)
# from qt5.printers import register_qt5_printers
# #register_qt5_printers (None)
# from imp import reload
# import tox.printers
# reload(tox.printers)
# #from tox.printers import register_tox_printers
# tox.printers.register_tox_printers()
#end

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
