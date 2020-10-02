type 'a graph_term = { nodes : 'a list;  edges : ('a * 'a) list }

let example_graph =
  { nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
    edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] }
