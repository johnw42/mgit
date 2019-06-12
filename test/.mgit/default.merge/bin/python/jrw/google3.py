import os
import os.path
import subprocess


def _AssertEqual(actual, expected):
  if actual != expected:
    raise AssertionError('Expected %r, got %r' % (expected, actual))


def Google3Split(path=None):
  if path is None:
    path = os.getcwd()
  if path.endswith('/google3'):
    return path, ''
  to_find = '/google3/'
  try:
    index = path.index(to_find) + len(to_find) - 1
  except ValueError:
    raise ValueError('path does not contain %r' % to_find)
  return path[:index], path[index:]

_AssertEqual(
    Google3Split('/usr/local/google/home/jrw/git5/cloudview/google3'),
    ('/usr/local/google/home/jrw/git5/cloudview/google3', ''))

assert Google3Split('/foo/bar/google3/a/b/c') == ('/foo/bar/google3', '/a/b/c')


def Google3Dir(path=None):
  return Google3Split(path)[0]


def RelativePathToGoogle3(path=None):
  return '../' * Google3Split(path)[1].count('/')

_AssertEqual(
    RelativePathToGoogle3('/foo/bar/google3/a/b/c'),
    '../../../')

def BlazeBinary(spec):
  """Get the path of a Blaze binary.

  The binary is built if it does not already exist.  To reduce
  latency, this call does not check for out-of-date binaries.

  Targets are interpreted relative to the current directory.

  Args:
    spec: (str) A Blaze spec in the package:target syntax.
  """
  dir_name, sep, target = spec.partition(':')
  if not sep: raise ValueError('invalid spec: %r' % spec)
  assert dir_name.startswith('//')
  bin_dir = Google3Dir() + '/blaze-bin'
  bin_file = bin_dir + dir_name[1:] + '/' + target
  if not os.path.isfile(bin_file):
    subprocess.check_call(['blaze', 'build', spec])
  assert os.path.isfile(bin_file)
  return bin_file
