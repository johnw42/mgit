"""Tools for working with the i3 window manager."""

import json
import subprocess
import re


def _i3_json(t):
  """Runs i3-msg and parse the JSON data.

  Args:
    t: (str) The message type.

  Returns:
    The parsed JSON data.
  """
  return json.loads(subprocess.check_output(['i3-msg', '-t', t]))


def _SortById(node_list):
  """Sorts a list of nodes by their `id` attribute."""
  node_list.sort(key=lambda x: x.id)


class Node(object):
  """Class representing a node in i3's container tree.

  Attributes:
    id: (int) The identifier used by i3.
    name: (str) The name reported by i3.
    nodes: (list of Node) The non-floating subnodes of this node, sorted by id.
    floating_nodes: (list of Node) The floating subnodes of this node, sorted by id.
    json_data: The raw JSON data describing this node.
  """

  @staticmethod
  def ForJsonData(json_data, is_floating=False):
    """Constructs an instance of a subclass of Node based on JSON data.

    Args:
      json_data: (dict) The parsed JSON data.
      is_floating: True if this is a floating node.

    Returns:
      An instance of a subclass of Node.
    """
    if json_data['window'] is not None:
      return Window(json_data, is_floating)
    else:
      return Node(json_data)

  def __init__(self, json_data):
    """Constructs a Node based on JSON data.

    Args:
      json_data: (dict) The parsed JSON data.
    """
    self.id = json_data['id']
    self.name = json_data['name']
    self.nodes = [Node.ForJsonData(node, False) for node in json_data['nodes']]
    _SortById(self.nodes)
    self.floating_nodes = [Node.ForJsonData(node, True) for node in json_data['floating_nodes']]
    _SortById(self.floating_nodes)
    self.json_data = json_data

  def GetWindows(self):
    """Finds the subnodes of this node that are windows.

    Returns:
      A list of Window objects.
    """
    result = []
    for node in self.nodes + self.floating_nodes:
      result += node.GetWindows()
    return result

  def GetFocusedWindow(self):
    for w in self.GetWindows():
      if w.is_focused:
        return w
    return None

  def GetWindowWithMark(self, mark):
    for w in self.GetWindows():
      if w.mark == mark:
        return w
    return None

  def __str__(self):
    return str(self.id)


class Window(Node):
  """An object that represents a Window in i3's container tree.

  Attributes:
    is_floating: True if this is a floating window.
    is_focused: True if this window is focused.
    is_urgent: True if this window's urgency hint is set.
    window_id: (int) The X11 ID of this window.
    _class_hint: private
  """

  def __init__(self, json_data, is_floating):
    Node.__init__(self, json_data)
    self.is_floating = is_floating
    self.is_focused = json_data['focused']
    self.is_urgent = json_data['urgent']
    self.window_id = json_data['window']
    self.mark = json_data.get('mark')
    self._class_hint = None

  def Mark(self, mark):
    self.Exec('mark', mark)

  def GetWindows(self):
    return [self]

  def XGetClassHint(self):
    """Gets the X11 WM_CLASS hint for this window.

    Returns:
      A pair of strings.
    """
    if not self._class_hint:
      output = subprocess.check_output(['xprop', '-id', str(self.window_id), 'WM_CLASS'])
      m = re.match(r'WM_CLASS\(STRING\) = "([^"]*)", "([^"]*)"', output)
      self._class_hint = m.groups()
    return self._class_hint

  def Focus(self):
    """Focuses this window."""
    self.Exec('focus')

  def Exec(self, *args, **kwargs):
    Exec(*args, con_id=self.id, **kwargs)


def _Quote(arg):
  """Quotes the argument in i3 syntax.

  Args:
    arg: (str) The value to quote.

  Returns:
    The quoted argument.

  Raises:
    ValueError: If the argument contains both ' and ".
  """
  arg = str(arg)
  if '"' not in arg:
    return '"' + arg + '"'
  elif '\'' not in arg:
    return '\'' + arg + '\''
  else:
    raise ValueError('Cannot quote argument: %r' % arg)


def Exec(*args, **kwargs):
  """Executes an i3 command.

  Positional arguments are passed directly to i3-msg as separate
  arguments.  Keyword arguments, if present, are used to construct
  the scope for the command.  The valid keyword arguments are:

  class: Compares the window class (the second part of WM_CLASS)
  instance: Compares the window instance (the first part of WM_CLASS)
  window_role: Compares the window role (WM_WINDOW_ROLE).
  id: Compares the X11 window ID, which you can get via xwininfo for
      example.
  title: Compares the X11 window title (_NET_WM_NAME or WM_NAME as
      fallback).
  urgent: Compares the urgent state of the window. Can be "latest" or
      "oldest". Matches the latest or oldest urgent window,
      respectively. (The following aliases are also available: newest,
      last, recent, first)
  con_mark: Compares the mark set for this container, see
      [vim_like_marks].
  con_id: Compares the i3-internal container ID, which you can get via
      the IPC interface. Handy for scripting.
  """
  command = ['i3-msg']
  if kwargs:
    scope_args = []
    for key, value in kwargs.items():
      if ' ' in key:
        raise ValueError('Invalid key: %r' % key)
      scope_args.append('%s=%s' % (key, _Quote(value)))
    command.append('[' + ' '.join(scope_args) + ']')
  command += list(args)
  result = json.loads(subprocess.check_output(command))
  if result[0]['success'] is not True:
    raise EnvironmentError('i3-msg returned an error: %r' % result)


def GetTree():
  """Gets a copy of i3's container tree.

  Returns:
    A Node object.
  """
  return Node.ForJsonData(_i3_json('get_tree'))


def GetMarks():
  """Gets a list of existing marks.

  Returns:
    A list of strings.
  """
  return _i3_json('get_marks')


def CycleWindows(windows, mark_name='cycle_windows_mark'):
  """Focuses the next window in a sequence.

  This function is meant to be executed repeatedly with the same list
  of windows.  Doing so will focus each window in turn.

  The currently-focused window is treated specially by this function.
  If this function is called while the focused window is not in
  `windows`, the effect in susequent calls will be the same as if that
  window was the first element of `windows`.  To achive this result,
  we set an i3 mark.  The name of the mark can be set using the
  `mark_name` parameter.  This behavior is disabled if mark_name is
  set to '' or None.

  Args:
    windows: An iterable of Window objects.
    mark_name: (str or None) A mark to use temporarily.
  """

  # Set `to_focus` to the next window that should receive focus, and
  # set `any_focused` to True iff any window in windows is already focused.
  to_focus = None
  focus_next = True
  any_focused = False
  focused_window = None
  for window in windows:
    if window.is_focused:
      focus_next = True
      to_focus = None
      any_focused = True
      focused_window = window
    elif focus_next:
      to_focus = window
      focus_next = False

  if to_focus:
    if mark_name and not any_focused:
      Exec('mark', mark_name)
    to_focus.Focus()
    return True
  elif mark_name:
    Exec('focus', con_mark=mark_name)
    Exec('unmark', mark_name)

  return False


def DumpWindowTree():
  """Prints the entire window tree in a human-readable format."""
  print json.dumps(_i3_json('get_tree'), sort_keys=True, indent=4)
