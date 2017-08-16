import sys
import os
import traceback
import IPython

from collections import defaultdict

"""
Functions to reload the module corresponding to a file and all the
modules that depend on it.

>>> update_deps() # Sync the dependency graph with the runtime
>>> deep_reload(src_to_modules('/abs/path/to/file.py'))
"""

# {module_name: set([dependency_names])}
DEPS = defaultdict(set)
# transopose(DEPS)
RDEPS = defaultdict(set)

def module_src(m):
  return m.__file__.replace('.pyc', '.py').replace('.pyo', '.py')

def src_to_modules(path):
  """
  From a source path get all the modules that correspond to that
  path. This also follows symlinks.
  """
  modules = []

  # Reload the modules that refer directly to the path and gather all
  # variables of all namespaces.
  for k,m in sys.modules.iteritems():
    if not hasattr(m, '__file__'):
      continue

    mpath = module_src(m).replace('/__init__.py','')
    if path.startswith(mpath):
      modules.append(m)
      continue

    try:
      if path.startswith(os.readlink(mpath)):
        modules.append(m)
        continue
    except OSError:
      pass

  return modules

def update_deps():
  """
  From the loaded modules read their files and see what modules they
  depend on populating DEPS and RDEPS.
  """
  global DEPS, RDEPS
  for m in sys.modules.values():
    if not hasattr(m, '__package__') or  m.__package__ in DEPS:
      continue

    fname = module_src(m)
    mname = m.__package__
    try:
      for l in open(fname):
        if l.startswith("import ") or l.startswith("from "):
          iline = [i.strip() for i in l.split(" ") if len(i) > 1]
          if iline[0] == 'import':
            for x in iline[1:]:
              DEPS[mname].add(x)
              RDEPS[x].add(mname)
          elif iline[0] == 'from':
            DEPS[mname].add(iline[1])
            RDEPS[iline[1]].add(x)

    except:
      continue

def deep_reload(module):
  """
  Reload module or list of modules and all modules depending on
  them. Uses DEPS and RDEPS so run updeate_deps() before this
  function.
  """

  seen_modules = set()
  stack = list(module) if hasattr(module, '__iter__') else [module]
  while len(stack):
    m = stack.pop()

    # There should be no circular deps but we got the tree from source
    # files.
    if m in seen_modules:
      continue

    reload(sys.modules[m])
    stack += list(RDEPS[m])
    seen_modules = seen_modules.union(RDEPS[m])

def showtraceback(self):
    traceback_lines = traceback.format_exception(*sys.exc_info())
    del traceback_lines[1]
    message = ''.join(traceback_lines)
    sys.stderr.write(message)

IPython.core.interactiveshell.InteractiveShell.showtraceback = showtraceback
IPython.core.interactiveshell.InteractiveShell._instance.prompt_manager.in_template = u'>>> '
IPython.core.interactiveshell.InteractiveShell._instance.prompt_manager.out_template = u''
IPython.core.interactiveshell.InteractiveShell._instance.prompt_manager.justify = False
