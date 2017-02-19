	var render = function(r, n) {
	  var set = r.set().push(
		 r.ellipse(n.point[0]-30, n.point[1]-13, 30, 20)
			.attr({"fill":"#ff69b4", "stroke-width": 1, r : "9px"}))
			.push(r.text(n.point[0]-30, n.point[1]-10, n.label)
				.attr({"font-size":"12px"}));
	  return set;
 };

var draw = function (g) { 
	return function (canvas) { 
				return function (size) {
					var layouter = new Graph.Layout.Spring(g); 
					layouter.layout(); 
					var renderer = new Graph.Renderer.Raphael(canvas, g, size, size); 
					renderer.draw(); };}; };
		
var createGraph = function (edges) { 
	return function (n) { 
		return function (canvas) { 
			var v = edges; 
			var g = new Graph(); 
			var i = 0; 
			while (i < v.length) 
                    {
                        if(!v[i][0]) g.addNode(v[i][0] + 1, { label: "O", render: render });
                        if(v[i][0]) g.addNode(v[i][0] + 1, { label: v[i][0], render: render });
                        if(!v[i][1]) g.addNode(v[i][1] + 1, { label: "O", render: render });
                        if(v[i][1]) g.addNode(v[i][1] + 1, { label: v[i][1], render: render });

                        if (v[i][3]) g.addEdge(v[i][0] + 1, v[i][1] + 1, { stroke: "#772e96", fill: "#772e96", label: v[i][2], directed : true });
                        if (!v[i][3]) g.addEdge(v[i][0] + 1, v[i][1] + 1, { stroke: "#bf9ebf", fill: "#bf9ebf", label: v[i][2], directed : true });
                        i = i + 1;
                    }
		return g}; 
		}; 
}; 




var createTree = function (edges) { 
	return function (n) { 
		return function (canvas) { 
			var v = edges; 
			var g = new Graph(); 
			var i = 0; 
			while (i < v.length) 
                    {
                        g.addNode(v[i][0], { label: v[i][1], render: render });
                        g.addNode(v[i][2], { label: v[i][3], render: render });
                        
                        g.addEdge(v[i][0], v[i][2], { stroke: "#A9A9A9", fill: "#A9A9A9", directed : true });
                        i = i + 1;
                    }
		return g}; 

		}; 
};
