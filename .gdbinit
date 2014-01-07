
## wget -N http://gcc.gnu.org/svn/gcc/trunk/libstdc++-v3/python/libstdcxx/v6/printers.py -P .gdbinit.d/python/libstdcxx/v6
python
import sys, os
sys.path.insert(0, os.path.expanduser('~/.gdbinit.d/python'))
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers (None)
from qt5.printers import register_qt5_printers
register_qt5_printers (None)
end

define hook-quit
  set confirm off
end

set history filename ~/.gdb_history
set history save

define go
  b main
  run
  b simple.cpp:30
  cont
  p s
  p ba
  p l
  p sl
  p vv
end
