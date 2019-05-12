## wget -N http://gcc.gnu.org/svn/gcc/trunk/libstdc++-v3/python/libstdcxx/v6/printers.py -P .gdbinit.d/python/libstdcxx/v6

python
import os, sys, glob

libstdcxx = glob.glob(os.path.expanduser('~/.gdbinit.d/python/libstdcxx')) +glob.glob('/usr/share/gcc-*/python/libstdcxx')
if len(libstdcxx) > 0 and os.path.isdir(libstdcxx[0]):
  libstdcxx_path = os.path.split(libstdcxx[0])[0]
  sys.path.append(libstdcxx_path)
  print('Added {} to python system paths'.format(libstdcxx_path))
  from libstdcxx.v6.printers import register_libstdcxx_printers
  register_libstdcxx_printers(None)

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
import socket, os
local_config = os.path.join(os.environ['HOME'], '.gdbinit.d', socket.gethostname())
if os.path.exists(local_config):
  gdb.execute('source {}'.format(local_config))
end


skip -gfi /usr/include/c++/*/*/*/*
skip -gfi /usr/include/c++/*/*/*
skip -gfi /usr/include/c++/*/*
skip -gfi /usr/include/c++/*

skip -gfi /usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../../include/c++/5.4.0/bits/*
skip -gfi /usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../../include/c++/5.4.0/*

#set follow-fork-mode child
#set detach-on-fork off

set follow-fork-mode child
set detach-on-fork off
set follow-exec-mode same
