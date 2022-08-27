
export function arrange_f({ nodes, focused, screenDims }) {

  const g = new dagre.graphlib.Graph();

  g.setGraph({});
  g.setDefaultEdgeLabel(function() { return {}; });

  for (const node of Object.values(nodes))
    g.setNode(node.id, { width: node.width, height: node.height });

  for (const node of Object.values(nodes)) {
    for (const dep of node.deps) {
      g.setEdge(node.id, dep.id);
    }
  }

  dagre.layout(g);

  let x0, y0;
  if (focused) {
    const f = g.node(focused.id);
    x0 = f.x + focused.width / 2;
    y0 = f.y + focused.height / 2;
  } else {
    x0 = 0;
    y0 = 0;
  }

  const coords = {};
  for (const node of Object.values(nodes)) {
    const n = g.node(node.id);
    coords[node.id] = { x: n.x - x0, y: (n.y - y0) * -1 };
  }
  return coords;

}

