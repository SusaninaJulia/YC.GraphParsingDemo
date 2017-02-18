

var draw1 = function (edges) {
              return function (n) {
                return function (canvas) {
                    var v = edges;
                    var g = new Graph();
                    var i = 0;
                    while (i < v.length)
                    {
			
                        if (v[i][3]) g.addEdge(v[i][0] + 1, v[i][1] + 1, { stroke: "#ADFF2F", fill: "#ADFF2F", label: v[i][2], directed : true });
                        if (!v[i][3]) g.addEdge(v[i][0] + 1, v[i][1] + 1, { stroke: "#A9A9A9", fill: "#A9A9A9", label: v[i][2], directed : true });
                        i = i + 1;
                    }

                    var layouter = new Graph.Layout.Spring(g);
                    layouter.layout();

                    var renderer = new Graph.Renderer.Raphael(canvas, g, 540, 540);
                    renderer.draw();
               };
            };
        };

var draw2 = function (edges) {
              return function (n) {
		return function (canvas) {
                    var v = edges;
                    var g = new Graph();
                    var i = 0;
                    while (i < v.length)
                    {
                        g.addEdge(v[i][0], v[i][2], { stroke: "#ADFF2F", fill: "#ADFF2F", directed : true });
                        i = i + 1;
                    }

                    var layouter = new Graph.Layout.Spring(g);
                    layouter.layout();

                    var renderer = new Graph.Renderer.Raphael( canvas, g, 540, 540);
                    renderer.draw();
		};
            };
        };