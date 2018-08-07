## wget -N http://gcc.gnu.org/svn/gcc/trunk/libstdc++-v3/python/libstdcxx/v6/printers.py -P .gdbinit.d/python/libstdcxx/v6

python
import os, sys, glob

libstdcxx = glob.glob('/usr/share/gcc-*')
if len(libstdcxx) > 0 and os.path.isdir(libstdcxx[0] + '/python'):
  libstdcxx_path = libstdcxx[0] + '/python'
  sys.path.append(libstdcxx_path)
  print('Added {} to python system paths'.format(libstdcxx_path))

sys.path.insert(0, os.path.expanduser('~/.gdbinit.d/python'))
from boost.printers import register_printer_gen
register_printer_gen(None)

from eigen.printers import register_eigen_printers
register_eigen_printers (None)
end


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


python
import os
local_config = os.path.join(os.environ['HOME'], '.gdbinit.d', os.environ['HOSTNAME'])
if os.path.exists(local_config):
  gdb.execute('source {}'.format(local_config))
end
