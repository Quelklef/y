
export const mkNodes =
({ getId, printId, getDepIds, getTime, getDims }) => inNodes =>
{
  const map = {};

  for (const inNode of inNodes) {
    const outNode = {
      id: printId(getId(inNode)),
      deps: null,  // set later
      time: getTime(inNode),
      width: getDims(inNode).width,
      height: getDims(inNode).height,
    };
    map[outNode.id] = outNode;
  }

  for (const inNode of inNodes) {
    map[printId(getId(inNode))].deps = getDepIds(inNode).map(depId => map[printId(depId)]);
  }

  return map;
}
