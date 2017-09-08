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

def update_dep(fname, mname):
  for l in open(fname).readlines():
    if l.startswith("import ") or l.startswith("from "):
      iline = [i.strip().strip(',') for i in l.split(' as ')[0].split(" ") if len(i) > 1]
      if iline[0] == 'import':
        for x in iline[1:]:
          DEPS[mname].add(x)
          RDEPS[x].add(mname)
      elif len(iline) > 1 and iline[0] == 'from':
        DEPS[mname].add(iline[1])
        RDEPS[iline[1]].add(mname)




def update_deps():
  """
  From the loaded modules read their files and see what modules they
  depend on populating DEPS and RDEPS.
  """

  for m in sys.modules.values():
    if not hasattr(m, '__file__') or not hasattr(m, '__name__') or  m.__name__ in DEPS:
      continue

    try:
      update_dep(module_src(m), m.__name__)
    except:
      continue


def deep_reload(module):
  """
  Reload module or list of modules and all modules depending on
  them. Uses DEPS and RDEPS so run updeate_deps() before this
  function.
  """
  seen_modules = set()
  stack = [m.__name__ for m in module] if hasattr(module, '__iter__') else [module.__name__]
  tmp = []
  while len(stack) > 0:
    m = stack.pop()
    p = sys.modules.get(m, None)
    if p:
      tmp.append((m, p))
      stack += list(RDEPS[m])

  reloaded = []
  for (m, p) in reversed(tmp):
    if m in reloaded:
      continue

    reloaded.append(m)

  ret = []
  for m in reversed(reloaded):
    print "Reloading:", m
    p = sys.modules[m]
    ret.append(p)
    reload(p)

  return ret

def reload_file(path):
  # Update the dep graphs with new imports.
  update_deps()

  # Update the dependency graphs graphs with possible new dependencies.
  ms = src_to_modules(path)
  for m in ms:
    update_dep(path, m)

  # Do the reload
  ms = deep_reload(ms)

  # All top level symbols that were just loaded from `path` should be
  # taken from the module.
  for km,vm in globals().iteritems():
    for md in reversed([i.__dict__ for i in ms]):
      # Reload an object itself
      if km in md:
        globals()[km] = md[km]
        break

def showtraceback(self):
  traceback_lines = traceback.format_exception(*sys.exc_info())
  del traceback_lines[1]
  message = ''.join(traceback_lines)
  sys.stderr.write(message)

IPython.core.interactiveshell.InteractiveShell.showtraceback = showtraceback
IPython.core.interactiveshell.InteractiveShell._instance.prompt_manager.in_template = u'>>> '
IPython.core.interactiveshell.InteractiveShell._instance.prompt_manager.out_template = u''
IPython.core.interactiveshell.InteractiveShell._instance.prompt_manager.justify = False
