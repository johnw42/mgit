def StripPrefix(prefix, paths):
  result = []
  for path in paths:
    before, sep, after = path.partition(prefix)
    if sep and not before:
      result.append(after)
  return result

def PruneSubdirectories(dir_paths):
  split_dir_paths = {tuple(p.split('/')): p for p in dir_paths}
  result_set = set()
  for key, value in split_dir_paths.items():
    omit_from_result = False
    for parent_key_len in range(1, len(key)):
      parent_key = key[:parent_key_len]
      if parent_key in split_dir_paths:
        omit_from_result = True
        break
    if not omit_from_result:
      result_set.add(value)
  return sorted(result_set)
