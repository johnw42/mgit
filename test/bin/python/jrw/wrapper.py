import logging
import os
import os.path
import signal
import subprocess
import sys

_wrapped_program = None


_log = logging.getLogger('jrw.wrapper')


def FindWrappedProgram():
  """Find the program that this program wraps.

  Returns:
    The absolute path of a program in $PATH named sys.argv[0].
  """
  global _wrapped_program
  if _wrapped_program is None:
    basename = os.path.basename(sys.argv[0])
    path_dirs = os.getenv('PATH').split(os.path.pathsep)
    next_program = None
    found_self = False
    for path_dir in path_dirs:
      file_path = os.path.join(path_dir, basename)
      if not os.access(file_path, os.X_OK):
        continue
      if os.path.samefile(file_path, sys.argv[0]):
        next_program = None
        found_self = True
      elif next_program is None and os.access(file_path, os.X_OK):
        next_program = file_path
        if found_self:
          break
    if next_program is None:
      raise Exception('Can\'t find %s in PATH.')
    _wrapped_program = next_program
  _log.debug('wrapped program: %r', _wrapped_program)
  return _wrapped_program


def ExecWrappedProgram(args=None):
  """Exec the program returned by FindWrappedProgram.

  Arguments:
    args: If specified, the argument list for the wrapper program.
        Defaults to sys.argv[1:].
  """
  if args is None:
    args = sys.argv[1:]
  wrapped_program = FindWrappedProgram()
  os.execvp(wrapped_program, [wrapped_program] + args)


def PopenWrappedProgram(args=None, **kwargs):
  """Runs the program returned by FindWrappedProgram as a subprocess.

  Arguments:
    args: The arguments to the wrapper program.  Defaults to
        sys.argv[1:].
    kwargs: Additional keyword arguments to pass to subprocess.Popen.
        If not specified, stdin defaults to subprocess.PIPE.

  Returns:
    A specialized instance of subprocess.Popen that inherits from
    ForwardingSignals.

  Example:

    # Call the wrapped program with no arguments, forward signals,
    # and pipe stdin to the wrapped program.
    with PopenWrappedProgram([]) as proc:
      for line in sys.stdin:
        proc.stdin.write(line)
      proc.stdin.close()
      proc.wait()
  """
  if args is None:
    args = sys.argv[1:]
  kwargs.setdefault('stdin', subprocess.PIPE)
  return _WrappedPopen([FindWrappedProgram()] + args, **kwargs)


class ForwardingSignals(object):
  """A context manager that forwards signals to a process."""

  def __init__(self, pid_or_proc):
    if isinstance(pid_or_proc, int):
      self.__pid = pid_or_proc
    else:
      self.__pid = pid_or_proc.pid

  def __enter__(self):
    self.__old_handlers = []
    for sig in [signal.SIGINT, signal.SIGQUIT]:
      old_handler = signal.signal(sig, self.__Handler)
      self.__old_handlers.append((sig, old_handler))

  def __exit__(self, *unused):
    for sig, handler in self.__old_handlers:
      signal.signal(sig, handler)
    del self.__old_handlers

  def __Handler(self, sig, unused):
    os.kill(self.__pid, sig)


class _WrappedPopen(subprocess.Popen, ForwardingSignals):

  def __init__(self, *args, **kwargs):
    subprocess.Popen.__init__(self, *args, **kwargs)
    ForwardingSignals.__init__(self, self.pid)

  def __enter__(self):
    super(_WrappedPopen, self).__enter__()
    return self
